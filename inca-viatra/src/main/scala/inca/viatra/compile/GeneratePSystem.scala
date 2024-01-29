package inca.viatra.compile

import inca.ir.extension.*
import inca.ir.lowering.BaseLowering
import inca.ir.{Arg, Atom, Call, Cast, Eq, ExtensionalCall, ExtensionalRelation, Module, Name, Param, RefByName, Relation, Term, TermArg, TermType, Type, Var, WildcardArg, name2string, typing}
import inca.viatra.util.{LitCollector, ScalaModuleEntryCollector, VarCollector}
import inca.foreign.scala.ir.primitive
import inca.foreign.scala.ir.arithmetic
import inca.foreign.scala.ir.data
import inca.foreign.scala.ir.string
import inca.foreign.scala.ir.primitive.{ScalaAggregationOperator, ScalaConstantTerm, ScalaDefnModuleEntry, ScalaMonoAggregationOperator, ScalaTerm, ScalaType}
import inca.ir.extension.aggregate.{Aggregate, AggregateColumnArg}
import inca.ir.extension.arithmetic.ArithmeticAggregationOperator
import inca.ir.extension.edbdata.{EdbType, Link, LookupEdbField, LookupEdbType, TEdbList, TEdbNode, TEdbValue}
import inca.ir.typing.Mode
import inca.ir.visitors.BaseIRVisitor
import inca.util.Gensym
import inca.util.compileroptions.CompilerOptions
import inca.viatra.runtime.index.dynamic.ParentIndex
import inca.viatra.runtime.index.{LinkNodeKey, PrimitiveTypeKey}
import org.eclipse.viatra.query.runtime.matchers.psystem.aggregations
import org.eclipse.viatra.query.runtime.matchers.psystem.basicdeferred.{Equality, TypeFilterConstraint}
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuples

import scala.collection.mutable.ListBuffer
object GeneratePSystem:
  val PARAMPREFIX = "param_"
  val VARPREFIX = "var_"
  val LITPREFIX = "lit_"
  val EVALPREFIX = "eval_"
  val EDB_PREFIX = "edb_"

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
    typechecker.checkProgram(Seq(module))

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

        val typechecker = new Typechecker {}
        typechecker.checkProgram(Seq(lowered))

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
       |import inca.viatra.runtime.index._
       |import inca.viatra.runtime.index.virtual._
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

  val varDeclarations: ListBuffer[String] = ListBuffer.empty
  /** Map expressions to their output variable */
  var evalExp: Seq[(Code, String)] = Seq()
  /** Map PVariable name to (name of the variable, getter code) or (None, literal value) */
  var pVar2Code: Map[String, (Option[String], Code)] = Map()
  val atomCode: ListBuffer[Code] = ListBuffer.empty

  private def compileRelation(moduleName: String, relation: Relation)(indent: Int = 0)(implicit env: RuleEnvironment): Code = gensym.scoped {
    val qname = s"${moduleName}_${relation.name}"

    val allVars = relation.bodies.flatMap(_.atoms.flatMap(_.vars))
    gensym.register(allVars.map(_.name.name))

    val paramNames = relation.params.map(_.name.name)
    val paramTermNames = paramNames.map { n => s"$PARAMPREFIX$n" }

    val bodies = if (relation.bodies.nonEmpty)
      relation.bodies.map { body =>
        evalExp = Seq()
        pVar2Code = Map()
        atomCode.clear()

        varDeclarations.clear()
        varDeclarations ++= VarCollector.collectAll(body).distinct.diff(paramNames).map(genTempVar)
        val litContent = LitCollector.collectAll(body).distinct.map { case (v, ty) => genLiteralVar(v, ty) }.mkString("\n")

        body.atoms.foreach(compileAtom)
        val atomContent = atomCode.mkString("\n")
        val exprsDef = evalExp.map(e => genExprEvalVar(e._2)).mkString("\n")
        val exprsContent = evalExp.map(_._1).mkString("\n")

        val varContent = varDeclarations.mkString("\n")
        val bodyContent = s"$varContent\n$litContent\n$exprsDef\n$exprsContent\n$atomContent"
        compileBody(moduleName, relation, bodyContent)(indent + 4)
      }
    else {
      // Bind all variables to null and insert an invalid equality constraint
      // That way, we produce the correct result when aggregating

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

  private def compileAtom(atom: Atom)(implicit env: RuleEnvironment): Unit = atom match
    case Call(RefByName(name), args, false) =>
      val module = env.getOrElse(name, throw new IllegalArgumentException(s"Unknown relation $name"))
      val argTuple = s"Tuples.flatTupleOf(${args.map(compileArg).mkString(",")})"
      val callQuery = s"$module.$name.instance.getInternalQueryRepresentation"
      atomCode += s"new PositivePatternCall(body, $argTuple, $callQuery)"
    case Call(RefByName(name), args, true) =>
      val module = env.getOrElse(name, throw new IllegalArgumentException(s"Unknown rule $name"))
      val argTuple = s"Tuples.flatTupleOf(${args.map(compileArg).mkString(",")})"
      val callQuery = s"$module.$name.instance.getInternalQueryRepresentation"
      atomCode += s"new NegativePatternCall(body, $argTuple, $callQuery)"
    case ExtensionalCall(RefByName(name), args, false) =>
      val key = s"""NamedRelationKey("$name", ${args.size})"""
      val tuple = s"Tuples.flatTupleOf(${args.map(compileArg).mkString(",")})"
      atomCode += s"new TypeConstraint(body, $tuple, $key)"
    case ExtensionalCall(RefByName(name), args, true) =>
      val key = s"""NotNamedRelationIndex.Key("$name", ${args.size})"""
      val tuple = s"Tuples.flatTupleOf(${args.map(compileArg).mkString(",")})"
      atomCode += s"new TypeFilterConstraint(body, $tuple, $key)"
    case Eq(lhs, rhs, false) =>
      atomCode += s"""new Equality(body, ${compileTerm(lhs)}, ${compileTerm(rhs)})"""
    case Eq(lhs, rhs, true) =>
      atomCode += s"""new Inequality(body, ${compileTerm(lhs)}, ${compileTerm(rhs)})"""

    case agg@Aggregate(rel, args, op) =>
      // We only support a single aggregation column
      val Seq(aggregatedColumn) = agg.aggregationColumns
      val outTerm = agg.args(aggregatedColumn).asInstanceOf[AggregateColumnArg].t
      val argTerms = args.map(compileArg)
      val result = compileTerm(outTerm)
      val module = env.getOrElse(rel.name, throw new IllegalArgumentException(s"Unknown relation $rel"))
      val argTuple = s"Tuples.flatTupleOf(${argTerms.mkString(",")})"
      val callQuery = s"$module.$rel.instance.getInternalQueryRepresentation"

      val code = op match
        case ScalaAggregationOperator(Name("Count"), scalaTy, initCode, addCode) =>
          s"new PatternMatchCounter(body, $argTuple, $callQuery, $result)"
        case ScalaAggregationOperator(name, ScalaType(scalaTyp), initCode, addCode) =>
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
        case ScalaMonoAggregationOperator(name, ScalaType(inTy), ScalaType(stateTy), initCode, addCode) =>
          val code =
            s"""
               | new inca.viatra.runtime.aggregate.MonoAggregation[$stateTy, $inTy] {
               |   override val name: String = "$name"
               |   override def init: $stateTy = $initCode
               |   override def add(st: $stateTy, a: $inTy): $stateTy = ($addCode)(st, a)
               | }.aggregator
               |""".stripMargin
          val boundAggOp = s"new BoundAggregator($code, classOf[$inTy], classOf[$stateTy])"
          s"new AggregatorConstraint($boundAggOp, body, $argTuple, $callQuery, $result, $aggregatedColumn)"
        case _ => throw IllegalArgumentException(s"Unexpected aggregation operator $op")
      atomCode += code

  private def compileArg(a: Arg): Code = a match
    case TermArg(t) =>
      compileTerm(t)
    case a@WildcardArg() =>
      val name = gensym.fresh("_")
      varDeclarations += genTempVar(name)
      compileTerm(Var(Name(name)).typed(a.typ.get))
    case AggregateColumnArg(t) =>
      val name = gensym.fresh("_")
      varDeclarations += genTempVar(name)
      compileTerm(Var(Name(name)).typed(t.typ.get))

  // This method should always return the name of a PVariable
  private def compileTerm(t: Term): Code = t match {
    case Var(RefByName(name)) =>
      val ty = t.typ match
        case Some(TermType(ScalaType(sty), _)) => sty
        case Some(TermType(ety: EdbType, _)) => compileEdbType(ety)
        case Some(TermType(ty, _)) => throw IllegalStateException(s"Can not compile none scala type $ty of term $t")
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

    case LookupEdbType(ety) =>
      val sty = compileEdbType(ety)
      
      val outName = gensym.fresh("edb_type")
      varDeclarations += genTempVar(outName)
      val pvarOut = s"$VARPREFIX$outName"
      pVar2Code += (pvarOut -> (Some(outName), s"""env.getValue("$outName").asInstanceOf[$sty]"""))

      val (sort, key) = genEdbTypeKey(ety)
      atomCode += s"new TypeConstraint(body, Tuples.staticArityFlatTupleOf($pvarOut), $key)"
      pvarOut

    case LookupEdbField(srcTerm, link) =>
      val src = compileTerm(srcTerm)
      val ety = t.typ.filter(_.ty.isInstanceOf[EdbType])
        .getOrElse(throw new IllegalStateException(s"EDB field lookup must have EDB type, but found ${t.typ}: $t"))
        .ty.asInstanceOf[EdbType]
      val sty = compileEdbType(ety)

      val outName = gensym.fresh("edb_lookup")
      varDeclarations += genTempVar(outName)
      val pvarOut = s"$VARPREFIX$outName"
      pVar2Code += (pvarOut -> (Some(outName), s"""env.getValue("$outName").asInstanceOf[$sty]"""))
      
      val key = genEdbLinkKey(link, srcTerm.typ.getOrElse(throw new IllegalArgumentException(s"Requires typed term $srcTerm")).ty)
      atomCode += s"new TypeConstraint(body, Tuples.staticArityFlatTupleOf($src, $pvarOut), $key)"
      pvarOut

    case _ => throw new UnsupportedOperationException(s"Unknown term $t")
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

  private def compileEdbType(ety: EdbType): Code = ety match
    case _: (TEdbNode | TEdbList) => "truechange.URI"
    case TEdbValue(ScalaType(sty)) => sty

  private def genEdbTypeKey(ety: EdbType): (String, String) = ety match
    case TEdbValue(ScalaType(sty)) =>
      val sort = s"truechange.JavaLitType(classOf[$sty])"
      val key = s"PrimitiveTypeKey($sort)"
      (sort, key)
    case TEdbNode(name) =>
      val sort = s"truechange.SortType(\"${name.name}\")"
      val key = s"NodeTypeKey($sort)"
      (sort, key)
    case TEdbList(ety) =>
      val sort = s"truechange.ListType(${genEdbTypeKey(ety)._1})"
      val key = s"NodeTypeKey($sort)"
      (sort, key)

  private def genEdbLinkKey(link: edbdata.Link, srcType: Type): String = link match
    case Link.Field(Name(name)) => srcType match
      case TEdbNode(Name(ty)) => s"""LinkNodeKey(("$ty", "$name"))"""
      case _ => throw new IllegalArgumentException(s"Cannot read edb field $name from $srcType")
    case Link.Parent => srcType match
      case ety: EdbType => "dynamic.ParentIndex.Key"
      case _ => throw new IllegalArgumentException(s"Cannot read edb parent from $srcType")
    case Link.Children => ???
    case Link.Next => ???
    case Link.Prev => ???
    case Link.Size => ???
    case Link.First => ???
    case Link.Last => ???
  
  private def genPParam(param: Param): Code = param.ty match
    case ety: EdbType =>
      val (sort, key) = genEdbTypeKey(ety)
      s"""private val $PARAMPREFIX${param.name}: PParameter = new PParameter("${param.name}", $sort.toString, $key)"""
    case _ =>
        s"""private val $PARAMPREFIX${param.name}: PParameter = new PParameter("${param.name}")"""


  private def genBodyParam(param: Param): Code = {
    s"""val $VARPREFIX${param.name}: PVariable = body.getOrCreateVariableByName("${param.name}")"""
  }

  private def genTempVar(name: String): Code = {
    s"""val $VARPREFIX$name: PVariable = body.getOrCreateVariableByName("$name")"""
  }
