package inca.viatra.compile

import inca.ir.extension.*
import inca.ir.lowering.BaseLowering
import inca.ir.{Atom, Call, Cast, Eq, ExtensionalCall, ExtensionalRelation, Module, NegCall, NegExtensionalCall, Neq, Param, Relation, Term, TermType, Var, name2string, typing}
import inca.viatra.util.{LitCollector, ScalaModuleEntryCollector, VarCollector}
import inca.foreign.scala.ir.primitive
import inca.foreign.scala.ir.arithmetic
import inca.foreign.scala.ir.data
import inca.foreign.scala.ir.string
import inca.foreign.scala.ir.primitive.{ScalaAggregationOperator, ScalaConstantTerm, ScalaDefnModuleEntry, ScalaTerm, ScalaType}
import inca.util.Gensym

object GeneratePSystem:
  val PARAMPREFIX = "param_"
  val VARPREFIX = "var_"
  val LITPREFIX = "lit_"
  val EVALPREFIX = "eval_"

  private trait BlockLowering extends primitive.Visitor with block.Lowering
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
    // Do not change this order
    val lowerings: List[() => BaseLowering] = List(
      () => new string.ScalaLowering {}, // lower strings
      () => new arithmetic.ScalaLowering {}, // lower arithmetic
      () => new data.ScalaLowering {}, // lower data
      () => new BlockLowering {}, // lower reintroduced blocks
    )

    // we need type information to translate the datalog code to scala code
    val typechecker = new Typechecker {}
    typechecker.typecheck(module)
    typechecker.failOnError()

    // apply and typecheck each lowering
    lowerings.foldLeft(module) {
      case (mod, lowering) =>
        val low = lowering()
        val Seq(lowered) = low.visitProgram(Seq(mod))
        typechecker.typecheck(lowered)
        typechecker.failOnError()
        lowered
    }
  }

  /**
   * Filter out all relations without a body and all relations that transitively depend on such a relation.
   */
  private def getProductiveRelations(module: Module): Map[String, Relation] = {
    val (nonEmptyRelations, emptyRelations) = module.relations.partition {
      case (_, r) => r.nonEmpty
    }

    var emptyRelationNames = emptyRelations.keys.toSet
    var result = nonEmptyRelations
    var isDirty = true

    while (isDirty) {
      isDirty = false
      result = result.flatMap {
        case (n, _) if emptyRelationNames.contains(n) =>
          isDirty = true
          None
        case (n, r) =>
          val productiveBodies = r.bodies.filter { b =>
            !b.atoms.exists {
              case Call(name, args) if emptyRelationNames.contains(name.name) => true
              case primitive.ScalaAggregationAtom(_, rel, _, _, _) if emptyRelationNames.contains(rel.name) => true
              case _ => false
            }
          }
          if (productiveBodies.nonEmpty)
            Some((n, Relation(r.name, r.params, productiveBodies)))
          else
            isDirty = true
            emptyRelationNames += n
            None
      }
    }
    result
  }

  def compileModule(module: Module)(implicit env: RuleEnvironment): Code = {
    val indent = 2
    val mod = lowerAndTypeModule(module)

    if (mod.contents.exists(c => c.name == mod.name))
      throw IllegalArgumentException("Modules must have a unique name different from all content entries")

    val relations = getProductiveRelations(mod)

    //println()
    //println(mod)
    //println()

    val myenv = env ++ relations.keys.map(r => r -> mod.name.name) // makes sure this module's names are found first
    val funs = relations.values.map(r => compileRelation(mod.name, r)(indent)(myenv)).toList

    val nonEmptyRels = relations.values.filter(!_.isEmpty).map {
      r => s""""${r.name}" -> (() => ${r.name}.instance)"""
    }

    // collect all external scala definitions
    val defns = ScalaModuleEntryCollector.collectAll(mod).map {
      case ScalaDefnModuleEntry(_, defn) => defn.indent(indent)
    }

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
      |import inca.viatra.runtime.aggregate.builtin
      |import org.eclipse.viatra.query.runtime.matchers.psystem.basicdeferred.AggregatorConstraint
      |import org.eclipse.viatra.query.runtime.matchers.psystem.aggregations.BoundAggregator
      |
      |object ${mod.name} extends PSystem.Module {
      |${defns.mkString("")}
      |  override val patterns: Map[String, () => Specification] = Map(${nonEmptyRels.mkString(",")})
      |${funs.mkString("\n")}
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

    val bodiesS = bodies.mkString("{", "}, {", "}")

    s"""
     |object ${relation.name} {
     |  lazy val instance: Specification = new Specification(generatedPQuery)
     |
     |  private object generatedPQuery extends BasePQuery(PVisibility.PUBLIC) {
     |    ${relation.params.map(genPParam).mkString(s"\n    ")}
     |
     |    override protected def doGetContainedBodies(): util.Set[PBody] = util.Set.of($bodiesS)
     |
     |    override def getFullyQualifiedName: String = "$qname"
     |    override def getParameters: util.List[PParameter] = util.List.of(${paramTermNames.mkString(",")})
     |    override def getParameterNames: util.List[String] = util.List.of(${paramNames.map(p => s""""$p"""").mkString(",")})
     |  }
     |}""".stripMargin.indent(indent)
  }

  private def compileAtom(atom: Atom)(implicit env: RuleEnvironment): Code = atom match
    case Call(name, args) =>
      val module = env.getOrElse(name, throw new IllegalArgumentException(s"Unknown relation $name"))
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
    case primitive.ScalaAggregationAtom(agg@ScalaAggregationOperator(sty, aggOpCode), rel, out, args, aggregatedColumn) =>
      val result = compileTerm(out)
      val module = env.getOrElse(rel, throw new IllegalArgumentException(s"Unknown relation $rel"))
      val argTuple = s"Tuples.flatTupleOf(${args.map(compileTerm).mkString(",")})"
      val callQuery = s"$module.$rel.instance.getInternalQueryRepresentation"

      val scalaTyp = sty.name
      agg match
        case ScalaAggregationOperator.Count =>
          s"new PatternMatchCounter(body, $argTuple, $callQuery, $result)"
        case _ =>
          val boundAggOp = s"new BoundAggregator($aggOpCode, classOf[$scalaTyp], classOf[$scalaTyp])"
          s"new AggregatorConstraint($boundAggOp, body, $argTuple, $callQuery, $result, $aggregatedColumn)"
    case primitive.ScalaAggregationAtom(agg, _, _, _, _) =>
      throw IllegalArgumentException(s"Unexpected aggregation operator $agg")


  // This method should always return the name of a PVariable
  private def compileTerm(t: Term): Code = t match {
    case Var(name) =>
      val ty = t.typ match
        case Some(TermType(ScalaType(sty), _)) => sty
        case Some(TermType(ty, _)) => throw IllegalStateException(s"Can not compile none scala type $ty")
        case _ => throw IllegalStateException(s"Untyped term $t")
      val pvarName = s"$VARPREFIX$name"
      pVar2Code += pvarName -> (Some(name), s"""env.getValue("$name").asInstanceOf[$ty]""")
      pvarName
    case Cast(t, ty) => compileTerm(t)
    case primitive.ScalaConstantTerm(code, ty) =>
      val pvarName = s"$LITPREFIX${genLiteralVarName(code, ty)}"
      pVar2Code += (pvarName -> (None, code))
      pvarName
    case scalaTerm@primitive.ScalaTerm(termCode, sty, args, isApp) =>
      val compiledArgs = args.map(compileTerm)
      val tyCode = sty.name

      val paramNames = compiledArgs.flatMap(c => pVar2Code(c)._1).map(v => s""""$v"""")
      val argTys = scalaTerm.inTypes.map(sty => sty.name)
      val code =
        if (isApp)
          s"""($termCode)(${compiledArgs.map(c => pVar2Code(c)._2).mkString(", ")})"""
        else
          s"$termCode"

      val description = s""""eval(${scalaTerm.toString})""""
      val outName = gensym.fresh("out")
      val pvarName = EVALPREFIX + outName

      val evalExpCode =
        s"""
           |new ExpressionEvaluation(body, new org.eclipse.viatra.query.runtime.matchers.psystem.IExpressionEvaluator {
           |  override def getShortDescription: String = \"\"\"$description\"\"\"
           |  override def getInputParameterNames: java.lang.Iterable[String] = java.util.Arrays.asList(${paramNames.mkString(",")})
           |  override def evaluateExpression(env: org.eclipse.viatra.query.runtime.matchers.psystem.IValueProvider): Any = {
           |    $code
           |  }
           |}, $pvarName)""".stripMargin

      evalExp :+= (evalExpCode, outName)
      pVar2Code += (pvarName -> (Some(outName), s"""env.getValue("$outName").asInstanceOf[${sty.name}]"""))
      pvarName
  }

  private def genExprEvalVar(name: String): Code = {
    s"""val ${EVALPREFIX + name}: PVariable = body.getOrCreateVariableByName("$name")""".stripMargin
  }

  private def genLiteralVar[T](lit: String, ty: primitive.ScalaType): Code = {
    val varName = genLiteralVarName(lit, ty)
    s"val $LITPREFIX$varName: PVariable = body.newConstantVariable(${lit})"
  }

  private def genLiteralVarName[T](lit: String, ty: primitive.ScalaType): String = {
    ty.name + lit.hashCode.toString.replace("-", "_")
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
