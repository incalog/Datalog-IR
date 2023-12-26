package inca.viatra.compile

import inca.ir.extension.*
import inca.ir.lowering.BaseLowering
import inca.ir.{Arg, Atom, Call, Cast, Eq, ExtensionalCall, ExtensionalRelation, Module, Name, Param, RefByName, Relation, Term, TermArg, TermType, Var, WildcardArg, name2string, typing}
import inca.viatra.util.{LitCollector, ScalaModuleEntryCollector, VarCollector}
import inca.foreign.scala.ir.primitive
import inca.foreign.scala.ir.arithmetic
import inca.foreign.scala.ir.data
import inca.foreign.scala.ir.string
import inca.foreign.scala.ir.primitive.{ScalaAggregationOperator, ScalaConstantTerm, ScalaDefnModuleEntry, ScalaMonoAggregationOperator, ScalaTerm, ScalaType}
import inca.ir.extension.arithmetic.ArithmeticAggregationOperator
import inca.ir.typing.Mode
import inca.ir.visitors.BaseIRVisitor
import inca.util.Gensym
import inca.util.compileroptions.CompilerOptions
import org.eclipse.viatra.query.runtime.matchers.psystem.aggregations
object GeneratePSystem:
  val PARAMPREFIX = "param_"
  val VARPREFIX = "var_"
  val LITPREFIX = "lit_"
  val EVALPREFIX = "eval_"

  private trait BlockLowering extends block.Lowering with primitive.Visitor
  private trait Typechecker extends typing.IRTypechecker with primitive.Typechecker

  val gensym = new Gensym()

  /** Maps rule name to the name of the module that defines it. */
  type RuleEnvironment = Map[String, String]
  type Code = String

  def compileModules(modules: Seq[Module], options: CompilerOptions): Code = {
    val env: RuleEnvironment = modules.flatMap(m => m.relations.map(r => r._1 -> m.name.name)).toMap
    modules.map(m => compileModule(m, options)(env)).mkString("\n")
  }

  protected def printStep(title: String, content: Any): Unit =
    println(title)
    println(content)
    println()
    println("~~~~~~~~~~~~~~~~~~~~~~~")
    println()

  private def lowerAndTypeModule(module: Module, options: CompilerOptions)(implicit env: RuleEnvironment): Module = {
    val viatraLogging = options("viatra_logging")
    val logTyped = viatraLogging.readBoolean("typed")
    val logModule = viatraLogging.readBoolean("module")
    val logLowerings = viatraLogging.readBoolean("lowerings")

    val viatraOptions = options("viatra_options")
    val withDoubleAggregationRewrite = viatraOptions.readBoolean("apply_double_aggregation_rewrite")

    // Do not change this order
    var lowerings: List[() => BaseIRVisitor] = List(
      () => new arithmetic.ScalaLowering {}, // lower arithmetic
      () => new string.ScalaLowering {}, // lower strings
      () => new data.ScalaLowering {}, // lower data
      () => new BlockLowering {}, // lower reintroduced blocks
    )

    if (withDoubleAggregationRewrite)
      lowerings :+= (() => new TimelyLatticeAggregationRewriter())

    if (logModule && !logTyped)
      printStep("Module", module)

    // we need type information to translate the datalog code to scala code
    val typechecker = new Typechecker {}
    typechecker.checkModule(module)

    if (logModule && logTyped)
      printStep("Module", module)

    typechecker.failOnError()

    // apply and typecheck each lowering
    lowerings.foldLeft(module) {
      case (mod, lowering) =>
        val low = lowering()
        val Seq(lowered) = low.visitProgram(Seq(mod))

        if (logLowerings && !logTyped)
          printStep(s"Lowering: ${low.name}", lowered)

        typechecker.checkModule(lowered)

        if (logLowerings && logTyped)
          printStep(s"Lowering: ${low.name}", lowered)

        typechecker.failOnError()
        lowered
    }
  }

  /**
   * Filter out all relations without a body and all relations that transitively depend on such a relation.
   */
  /*private def getProductiveRelations(module: Module): Map[String, Relation] = {
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
              case Call(name, args, _) if emptyRelationNames.contains(name.name) => true
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
  }*/

  def compileModule(module: Module, options: CompilerOptions)(implicit env: RuleEnvironment): Code = {
    val indent = 2

    val mod = lowerAndTypeModule(module, options)

    if (mod.contents.exists(c => c.name == mod.name))
      throw IllegalArgumentException("Modules must have a unique name different from all content entries")

    //val relations = getProductiveRelations(mod)
    val relations = mod.relations

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

  private def compileRelation(moduleName: String, relation: Relation)(indent: Int = 0)(implicit env: RuleEnvironment): Code = gensym.scoped {
    val qname = s"${moduleName}_${relation.name}"

    val allVars = relation.bodies.flatMap(_.atoms.flatMap(_.vars))
    gensym.register(allVars.map(_.name.name))

    val paramNames = relation.params.map(_.name.name)
    val paramTermNames = paramNames.map { n => s"$PARAMPREFIX$n" }
    
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
      // Bind all variables to null and insert an invalid equality constraint
      // That way, we produce the correct result when aggregating
      evalExp = Seq()
      pVar2Code = Map()

      val paramConstraints = relation.params.map { p =>
          s"new Equality(body, $VARPREFIX${p.name} ,body.newConstantVariable(null))"
      }
      val failingConstraint = s"new Equality(body, body.newConstantVariable(1), body.newConstantVariable(0))"
      val content = paramConstraints :+ failingConstraint
      Seq(compileBody(moduleName, relation, content.mkString("\n"))(indent + 4))
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
    case Call(RefByName(name), args, false) =>
      val module = env.getOrElse(name, throw new IllegalArgumentException(s"Unknown relation $name"))
      val argTuple = s"Tuples.flatTupleOf(${args.map(compileArg).mkString(",")})"
      val callQuery = s"$module.$name.instance.getInternalQueryRepresentation"
      s"new PositivePatternCall(body, $argTuple, $callQuery)"
    case Call(RefByName(name), args, true) =>
      val module = env.getOrElse(name, throw new IllegalArgumentException(s"Unknown rule $name"))
      val argTuple = s"Tuples.flatTupleOf(${args.map(compileArg).mkString(",")})"
      val callQuery = s"$module.$name.instance.getInternalQueryRepresentation"
      s"new NegativePatternCall(body, $argTuple, $callQuery)"
    case ExtensionalCall(RefByName(name), args, false) =>
      val key = s"""NamedRelationKey("$name", ${args.size})"""
      val tuple = s"Tuples.flatTupleOf(${args.map(compileArg).mkString(",")})"
      s"new TypeConstraint(body, $tuple, $key)"
    case ExtensionalCall(RefByName(name), args, true) =>
      // use a type filter ?
      ???
    case Eq(lhs, rhs, false) =>
      s"""new Equality(body, ${compileTerm(lhs)}, ${compileTerm(rhs)})"""
    case Eq(lhs, rhs, true) =>
      s"""new Inequality(body, ${compileTerm(lhs)}, ${compileTerm(rhs)})"""
    case primitive.ScalaAggregationAtom(ScalaMonoAggregationOperator(name, ScalaType(inTy), ScalaType(stateTy), initCode, addCode), rel, out, args, aggregatedColumn) =>
      val result = compileTerm(out)
      val module = env.getOrElse(rel, throw new IllegalArgumentException(s"Unknown relation $rel"))
      val argTuple = s"Tuples.flatTupleOf(${args.map(compileTerm).mkString(",")})"
      val callQuery = s"$module.$rel.instance.getInternalQueryRepresentation"

      val code = s"""
           | new inca.viatra.runtime.aggregate.MonoAggregation[$stateTy, $inTy] {
           |   override val name: String = "$name"
           |   override def init: $stateTy = $initCode
           |   override def add(st: $stateTy, a: $inTy): $stateTy = ($addCode)(st, a)
           | }.aggregator
           |""".stripMargin

      val boundAggOp = s"new BoundAggregator($code, classOf[$inTy], classOf[$stateTy])"
      s"new AggregatorConstraint($boundAggOp, body, $argTuple, $callQuery, $result, $aggregatedColumn)"
    // special case: count aggregation, which is neither associative nor commutative,
    // so we use a builtin count aggregation implemented in VIATRA system
    case primitive.ScalaAggregationAtom(ScalaAggregationOperator(name, scalaTy, initCode, addCode), rel, out, args, aggregatedColumn) if name.name == "Count" =>
      val result = compileTerm(out)
      val module = env.getOrElse(rel, throw new IllegalArgumentException(s"Unknown relation $rel"))
      val argTuple = s"Tuples.flatTupleOf(${args.map(compileTerm).mkString(",")})"
      val callQuery = s"$module.$rel.instance.getInternalQueryRepresentation"
      s"new PatternMatchCounter(body, $argTuple, $callQuery, $result)"
    case primitive.ScalaAggregationAtom(ScalaAggregationOperator(name, ScalaType(scalaTyp), initCode, addCode), rel, out, args, aggregatedColumn) =>
      val result = compileTerm(out)
      val module = env.getOrElse(rel, throw new IllegalArgumentException(s"Unknown relation $rel"))
      val argTuple = s"Tuples.flatTupleOf(${args.map(compileTerm).mkString(",")})"
      val callQuery = s"$module.$rel.instance.getInternalQueryRepresentation"

      val code =
        s"""new inca.viatra.runtime.aggregate.JoinAggregation[$scalaTyp] {
           |       override val name = "$name"
           |       override def init: $scalaTyp = $initCode
           |       override def join(v1: $scalaTyp, v2: $scalaTyp): $scalaTyp = ($addCode)(v1, v2)
           |       override val isAssociative = true
           |       override val isCommutative = true
           |     }.aggregator
           |""".stripMargin
      val boundAggOp = s"new BoundAggregator($code, classOf[$scalaTyp], classOf[$scalaTyp])"
      s"new AggregatorConstraint($boundAggOp, body, $argTuple, $callQuery, $result, $aggregatedColumn)"
//    case ArithmeticAggregationOperator.Count =>
//      ???
    case primitive.ScalaAggregationAtom(agg, _, _, _, _) =>
      throw IllegalArgumentException(s"Unexpected aggregation operator $agg")

  private def compileArg(a: Arg): Code = a match
    case TermArg(t) => compileTerm(t)
    case WildcardArg() => throw IllegalStateException("Encountered unexpected wildcard argument!")

  // This method should always return the name of a PVariable
  private def compileTerm(t: Term): Code = t match {
    case Var(RefByName(name)) =>
      val ty = t.typ match
        case Some(TermType(ScalaType(sty), _)) => sty
        case Some(TermType(ty, _)) => throw IllegalStateException(s"Can not compile none scala type $ty")
        case _ => throw IllegalStateException(s"Untyped term $t")
      val pvarName = s"$VARPREFIX$name"
      pVar2Code += pvarName -> (Some(name), s"""env.getValue("$name").asInstanceOf[$ty]""")
      pvarName
    case Cast(t, ty) =>
      // Cast the term
      t.typ = t.typ match
        case Some(TermType(_, mode)) => Some(TermType(ty, mode))
        case _ => throw IllegalStateException(s"Untyped term $t")
      compileTerm(t)
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
    s"val $LITPREFIX$varName: PVariable = body.newConstantVariable($lit)"
  }

  private def genLiteralVarName[T](lit: String, ty: primitive.ScalaType): String = {
    ty.name.replace("[", "$").replace("]", "$") + lit.hashCode.toString.replace("-", "_")
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
