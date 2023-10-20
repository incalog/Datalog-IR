package inca.viatra.compile

import inca.ir.extension.*
import inca.ir.lowering.BaseLowering
import inca.ir.{Atom, Call, Cast, Eq, ExtensionalCall, Module, NegCall, NegExtensionalCall, Neq, Param, Relation, Term, TermType, Var, name2string, typing}
import inca.viatra.ir.primitiveScala
import inca.viatra.util.{LitCollector, VarCollector}
import inca.foreign.scala.ir.primitive
import inca.foreign.scala.ir.arithmetic
import inca.foreign.scala.ir.string
import inca.foreign.scala.ir.primitive.{ScalaTerm, ScalaType}
import inca.foreign.scala.syntax.Scala
import inca.util.Gensym

import scala.annotation.tailrec

object GeneratePSystem:
  val PARAMPREFIX = "param_"
  val VARPREFIX = "var_"
  val LITPREFIX = "lit_"
  val EVALPREFIX = "eval_"

  private trait BlockLowering extends primitive.Visitor with block.Lowering
  private trait DemandLowering extends primitive.Visitor with demand.Lowering
  private trait Typechecker extends typing.IRTypechecker with primitive.Typechecker

  val gensym = new Gensym()

  /** Maps rule name to the name of the module that defines it. */
  type RuleEnvironment = Map[String, String]
  type Code = String

  def compileModules(modules: Seq[Module]): Code = {
    val env: RuleEnvironment = modules.flatMap(m => m.relations.map(r => r._1 -> m.name.name)).toMap
    modules.map(m => compileModule(m)(env)).mkString("\n")
  }

  private def lowerAndTypeModule(module: Module)(implicit env: RuleEnvironment): Module = {
    val lowerings: List[() => BaseLowering] = List(
      () => new arithmetic.ScalaLowering {}, // Get rid of arithmetic
      () => new string.ScalaLowering {}, // Get rid of strings
      //() => new data.ScalaLowering {}, // Get rid of data
      () => new BlockLowering {}, // Get rid of reintroduced blocks
      () => new DemandLowering {} // Get rid of reintroduced demand symbols
    )

    // we need type information to translate the datalog code to scala code
    val typechecker = new Typechecker {}
    typechecker.typecheck(module)
    typechecker.failOnError()

    // apply and typecheck each lowering
    lowerings.foldLeft(module) {
      case (mod, lowering) =>
        val low = lowering()
        println()
        println(s"Backend lowering ${low}")
        val Seq(lowered) = low.visitProgram(Seq(mod))
        println(lowered)
        typechecker.typecheck(lowered)
        typechecker.failOnError()
        lowered
    }
  }

  def compileModule(module: Module)(implicit env: RuleEnvironment): Code = {
    val mod = lowerAndTypeModule(module)
    println()
    println("After: ")
    println(mod)

    val myenv = env ++ mod.relations.keys.map(r => r -> mod.name.name) // makes sure this module's names are found first
    val funs = mod.relations.values.map(r => compileRelation(mod.name, r)(indent=2)(myenv)).toList

    val nonEmptyRels = mod.relations.values.filter(!_.isEmpty).map {
      r => s""""${r.name}" -> (() => ${r.name}.instance)"""
    }

    val indent = "  "

    s"""
      |import org.eclipse.viatra.query.runtime.api.{GenericPatternMatcher, ViatraQueryEngine}
      |import org.eclipse.viatra.query.runtime.api.scope.{QueryScope => ViatraQueryScope}
      |import org.eclipse.viatra.query.runtime.matchers.psystem.{PBody, PVariable}
      |import org.eclipse.viatra.query.runtime.matchers.psystem.queries.{BasePQuery, PParameter, PVisibility}
      |import org.eclipse.viatra.query.runtime.matchers.tuple.Tuples
      |import org.eclipse.viatra.query.runtime.matchers.psystem.basicdeferred.ExportedParameter
      |
      |import org.eclipse.viatra.query.runtime.matchers.context.common.JavaTransitiveInstancesKey
      |
      |import java.util
      |
      |import org.eclipse.viatra.query.runtime.matchers.psystem.basicdeferred._
      |import org.eclipse.viatra.query.runtime.matchers.psystem.basicenumerables._
      |
      |import inca.viatra.compile.PSystem
      |import inca.viatra.runtime.Query.Specification
      |import inca.viatra.runtime.index.NamedRelationKey
      |
      |object ${mod.name} extends PSystem.Module {
      |  override val patterns: Map[String, () => Specification] = Map(${nonEmptyRels.mkString(",")})
      |  ${funs.mkString("\n")}
      |}
    """.stripMargin
  }

  private def compileBody(moduleName: String, relation: Relation, content: Code)(indent: Int = 0)(implicit env: RuleEnvironment): Code =
    s"""
      |val body: PBody = new PBody(this)
      |${relation.params.map(genBodyParam).mkString("\n")}
      |val exportedParams = new util.ArrayList[ExportedParameter]()
      ${relation.params.map { p =>
          s"|exportedParams.add(new ExportedParameter(body, $VARPREFIX${p.name}, $PARAMPREFIX${p.name}))"
        }.mkString("\n")}
      |
      |body.setSymbolicParameters(exportedParams)
      |$content
      |body""".stripMargin.indent(indent)

  /** Map expressions to their output variable */
  var evalExp: Seq[(Code, String)] = Seq()

  /** Map PVariable name to (name of the variable, getter code) or (None, literal value) */
  var pVar2Code: Map[String, (Option[String], Code)] = Map()

  private def compileRelation(moduleName: String, relation: Relation)(indent: Int = 0)(implicit env: RuleEnvironment): Code = {
    val qname = s"${moduleName}_${relation.name}"

    val paramNames = relation.params.map(_.name.name)
    val paramTermNames = paramNames.map { n => s"$PARAMPREFIX${n}" }
    //val allVars = VarCollector.collectAll(relation)

    //val gensym = new Gensym(allVars)

    if (relation.isEmpty) {
      return s"""
         |object ${relation.name} {
         |  val error = "This pattern was empty"
         |}""".stripMargin.indent(indent)
    }

    val bodies = if (relation.bodies.nonEmpty)
      relation.bodies.map { body =>
        val varContent = VarCollector.collectAll(body).distinct.diff(paramNames).map(genTempVar).mkString("\n")
        val litContent = LitCollector.collectAll(body).distinct.map { case (v, ty) => genLiteralVar(v, ty) }.mkString("\n")

        evalExp = Seq()
        pVar2Code = Map()

        val atomContent = body.atoms.map(compileAtom).mkString("\n")
        val exprsDef = evalExp.map(e => genExprEvalVar(e._2)).mkString("\n")
        val exprsContent = evalExp.map(_._1).mkString("\n")

        val bodyContent = s"$varContent\n$litContent\n$exprsDef\n$exprsContent\n$atomContent"
        compileBody(moduleName, relation, bodyContent)(indent + 4)
      }
    else {
      evalExp = Seq()
      pVar2Code = Map()

      val content = s"new Equality(body, body.newConstantVariable(1), body.newConstantVariable(0))"
      Seq(compileBody(moduleName, relation, content)(indent + 4))
    }

    s"""
     |object ${relation.name} {
     |  lazy val instance: Specification = new Specification(generatedPQuery)
     |
     |  private object generatedPQuery extends BasePQuery(PVisibility.PUBLIC) {
     |    ${relation.params.map(genPParam).mkString(s"\n    ")}
     |
     |    override protected def doGetContainedBodies(): util.Set[PBody] = util.Set.of(${bodies.mkString("{", "}, {", "}")} )
     |
     |    override def getFullyQualifiedName: String = "$qname"
     |    override def getParameters: util.List[PParameter] = util.List.of(${paramTermNames.mkString(",")})
     |    override def getParameterNames: util.List[String] = util.List.of(${paramNames.map(p => s""""$p"""").mkString(",")})
     |  }
     |}""".stripMargin.indent(indent)
  }

  private def compileAtom(atom: Atom)(implicit env: RuleEnvironment): Code = atom match
    case Call(name, args) =>
      val module = env.getOrElse(name, throw new IllegalArgumentException(s"Unknown rule $name"))
      val argTuple = s"Tuples.flatTupleOf(${args.map(compileTerm).mkString(",")})"
      val callQuery = s"$module.$name.instance.getInternalQueryRepresentation"
      s"new PositivePatternCall(body, $argTuple, $callQuery)"
    case NegCall(name, args) =>
      val module = env.getOrElse(name, throw new IllegalArgumentException(s"Unknown rule $name"))
      val argTuple = s"Tuples.flatTupleOf(${args.map(compileTerm).mkString(",")})"
      val callQuery = s"$module.$name.instance.getInternalQueryRepresentation"
      s"new NegativePatternCall(body, $argTuple, $callQuery)"
    case ExtensionalCall(name, args) =>
      val key = s"""NamedRelationKey("$name", ${args.size})"""
      val tuple = s"Tuples.flatTupleOf(${args.map(compileTerm).mkString(",")})"
      s"new TypeConstraint(body, $tuple, $key)"
    case NegExtensionalCall(name, args) =>
      // use a type filter ?
      ???
    case Eq(lhs, rhs) =>
      s"""new Equality(body, ${compileTerm(lhs)}, ${compileTerm(rhs)})"""
    case Neq(lhs, rhs) =>
      s"""new Inequality(body, ${compileTerm(lhs)}, ${compileTerm(rhs)})"""

  // This method should always return the name of a PVariable
  private def compileTerm(t: Term): Code = t match {
    case Var(name) =>
      val ty = t.typ match
        case Some(TermType(ScalaType(sty), _)) => compileScalaType(sty)
        case Some(TermType(ty, _)) => throw IllegalStateException(s"Can not compile none scala type $ty")
        case _ => throw IllegalStateException(s"Untyped term $t")
      val pvarName = s"$VARPREFIX$name"
      pVar2Code += pvarName -> (Some(name), s"""env.getValue("$name").asInstanceOf[$ty]""")
      pvarName
    case Cast(t, ty) => compileTerm(t)
    case primitive.ScalaTerm(lit: Scala.Literal[_], ty, args) =>
      val pvarName = s"$LITPREFIX${genLiteralVarName(lit, ty)}"
      pVar2Code += (pvarName -> (None, s"${lit.value}"))
      pvarName
    case primitive.ScalaTerm(lam@Scala.Lam(lamParams, t), sty, args) =>
      val compiledArgs = args.map(compileTerm)
      val lamCode = compileScalaTerm(lam)
      val tyCode = compileScalaType(sty.ty)

      val paramNames = compiledArgs.flatMap(c => pVar2Code(c)._1).map(v => s""""$v"""")
      val argTys = lamParams.map(p => compileScalaType(p.ty))
      val argsCode = compiledArgs.map(c => pVar2Code(c)._2)

      val description = s""""eval(${lam.toString})""""
      val outName = gensym.fresh("out")
      val pvarName = EVALPREFIX + outName

      val evalExpCode =
        s"""
           |new ExpressionEvaluation(body, new org.eclipse.viatra.query.runtime.matchers.psystem.IExpressionEvaluator {
           |  override def getShortDescription: String = $description
           |  override def getInputParameterNames: java.lang.Iterable[String] = java.util.Arrays.asList(${paramNames.mkString(",")})
           |  override def evaluateExpression(env: org.eclipse.viatra.query.runtime.matchers.psystem.IValueProvider): Any = {
           |    ($lamCode)(${argsCode.mkString(", ")})
           |  }
           |}, $pvarName)""".stripMargin

      evalExp :+= (evalExpCode, outName)
      pVar2Code += (pvarName -> (Some(outName), s"""env.getValue("$outName").asInstanceOf[${compileScalaType(sty.ty)}]"""))
      pvarName
  }

  private def compileScalaTerm(term: Scala.Term): Code = term match
    case Scala.Id(x) => x
    case Scala.Select(t, name) =>
      s"${compileScalaTerm(t)}.$name"
    case Scala.Lam(params, t) =>
      val args = params.map(p => s"${p.name}: ${compileScalaType(p.ty)}")
      s"(${args.mkString(", ")}) => ${compileScalaTerm(t)}"
    case Scala.App(fun, args) =>
      val inArgs = args.map(compileScalaTerm).mkString(",")
      s"${compileScalaTerm(fun)}($inArgs})"
    case Scala.AppInfix(t1, op, t2) =>
      s"${compileScalaTerm(t1)} $op ${compileScalaTerm(t2)}"

  private def compileScalaType(t: Scala.Type): Code = t match {
    case Scala.TypeName(s) => s
    case Scala.FunType(args, ret) => s"Function[${(args :+ ret).map(compileScalaType).mkString(",")}]"
  }

  private def genLamVarName(lam: Scala.Lam): String = lam.hashCode().toString

  private def genLiteral[T](lit: Scala.Literal[T]): Code = s"${lit.value}"

  private def genExprEvalVar(name: String): Code = {
    s"""val ${EVALPREFIX + name}: PVariable = body.getOrCreateVariableByName("$name")""".stripMargin
  }

  private def genLiteralVar[T](lit: Scala.Literal[T], ty: primitive.ScalaType): Code = {
    val varName = genLiteralVarName(lit, ty)
    s"val $LITPREFIX$varName: PVariable = body.newConstantVariable(${genLiteral(lit)})"
  }

  private def genLiteralVarName[T](lit: Scala.Literal[T], ty: primitive.ScalaType): String = {
    compileScalaType(ty.ty) + lit.value.hashCode
  }

  private def genPParam(param: Param): Code = {
    s"""private val $PARAMPREFIX${param.name}: PParameter = new PParameter("${param.name}")"""
  }

  private def genBodyParam(param: Param): Code = {
    s"""val $VARPREFIX${param.name}: PVariable = body.getOrCreateVariableByName("${param.name}")"""
  }

  private def genTempVar(name: String): Code = {
    s"""val $VARPREFIX$name: PVariable = body.getOrCreateVariableByName("$name")"""
  }
