package inca.backend.lowering

import inca.Scala
import inca.Scala.{App, AppInfix, FunType, Id, Lam, Literal, Select, TypeName}
import inca.backend.optimize.{ConstantFolding, ConstantPropagation, EliminateAliases, Optimization}
import inca.backend.util.{LitCollector, VarCollector}
import inca.ir.extension.primitiveScala.{Application, Constant, TScala}
import inca.ir.{Atom, Body, Call, Eq, ExtensionalCall, Module, NegCall, NegExtensionalCall, Neq, Param, Relation, Term, Type, Var, name2string}
import inca.util.Gensym
import inca.ir.extension.{arithmetic, block, data, demand, primitiveScala}
import inca.ir.lowering.BaseLowering
import inca.ir.typing

object GeneratePSystem:
  val PARAMPREFIX = "param_"
  val VARPREFIX = "var_"
  val LITPREFIX = "lit_"
  val EVALPREFIX = "eval_"

  private trait BlockLowering extends primitiveScala.Visitor with block.Lowering
  private trait DemandLowering extends primitiveScala.Visitor with demand.Lowering
  private trait Typechecker extends typing.IRTypechecker with primitiveScala.Typechecker

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
        val lowered = low.lower(mod)
        println(lowered)
        typechecker.typecheck(lowered)
        typechecker.failOnError()
        lowered
    }
  }

  // TODO: Move this to a better place. It should not be here
  def optimizeModule(module: Module): Module = {
    val optimizations: List[Optimization] = List(
      ConstantPropagation,
      EliminateAliases,
      ConstantFolding
    )

    optimizations.foldLeft(module) { case (m, optimization) =>
      // TODO: Use correct data model
      val mod = optimization.optimizer().visit(m)
      println(s"Optimize: ${optimization.name}")
      println(mod)
      mod
    }
  }

  def compileModule(module: Module)(implicit env: RuleEnvironment): Code = {
    var mod = lowerAndTypeModule(module)
    mod = optimizeModule(mod)

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
      |import inca.backend.lowering.PSystem
      |import inca.runtime.Query.Specification
      |import inca.runtime.index.NamedRelationKey
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
        val atomContent = body.atoms.map(compileAtom).mkString("\n")
        compileBody(moduleName, relation, s"$varContent\n$litContent\n$atomContent")(indent + 4)
      }
    else {
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
    case Application(out, ty, lam, args) if args.isEmpty =>
      val varName = genConstantLamVarName(lam)
      val rhs = EVALPREFIX + varName
      s"""new Equality(body, ${compileTerm(out)}, $rhs)"""
    case Application(out, TScala(ty), lam@Lam(params, t), args) =>
      val result = compileTerm(out)
      val description = s""""eval(${lam.toString})""""
      val paramNames = args.toList.flatMap {
        case Var(name) => Some(s""""$name"""")
        case _ => None
      }
      val argTerms = args.zip(params).toList.map {
        case (Var(name), p) => s"""env.getValue("$name").asInstanceOf[${compileScalaType(p.ty)}]"""
        case (Constant(lit, ty), p) => genLiteral(lit)
      }
      s"""
        |new ExpressionEvaluation(body, new org.eclipse.viatra.query.runtime.matchers.psystem.IExpressionEvaluator {
        |  override def getShortDescription: String = $description
        |  override def getInputParameterNames: java.lang.Iterable[String] = java.util.Arrays.asList(${paramNames.mkString(",")})
        |  override def evaluateExpression(env: org.eclipse.viatra.query.runtime.matchers.psystem.IValueProvider): Any = {
        |    (${compileScalaTerm(lam)})(${argTerms.mkString(",")})
        |  }
        |}, $result)""".stripMargin

  private def compileTerm(v: Term): Code = v match {
    case Var(name) => s"$VARPREFIX$name"
    case Constant(value, ty) => s"$LITPREFIX${genLiteralVarName(value, ty)}"
  }

  private def compileScalaTerm(term: Scala.Term): Code = term match
    case Id(x) => x
    case Select(t, name) =>
      s"${compileScalaTerm(t)}.$name"
    case Lam(params, t) =>
      val args = params.map(p => s"${p.name}: ${compileScalaType(p.ty)}")
      s"(${args.mkString(", ")}) => ${compileScalaTerm(t)}"
    case App(fun, args) =>
      val inArgs = args.map(compileScalaTerm).mkString(",")
      s"${compileScalaTerm(fun)}($inArgs})"
    case AppInfix(t1, op, t2) =>
      s"${compileScalaTerm(t1)} $op ${compileScalaTerm(t2)}"

  private def compileScalaType(t: Scala.Type): Code = t match {
    case TypeName(s) => s
    case FunType(args, ret) => s"Function[${(args :+ ret).map(compileScalaType).mkString(",")}]"
  }

  private def genConstantLamVarName(lam: Lam): String = lam.hashCode().toString

  private def genLiteral[T](lit: Scala.Literal[T]): Code = s"${lit.value}"

  private def genLiteralVar[T](lit: Scala.Literal[T], ty: TScala): Code = {
    val varName = genLiteralVarName(lit, ty)
    s"val $LITPREFIX$varName: PVariable = body.newConstantVariable(${genLiteral(lit)})"
  }

  private def genLiteralVarName[T](lit: Scala.Literal[T], ty: TScala): String = {
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
