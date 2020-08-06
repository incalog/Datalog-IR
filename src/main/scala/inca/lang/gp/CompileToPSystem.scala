package inca.lang.gp


import inca.lang.psystem.PSystem
import inca.runtime.index._
import inca.runtime.index.dynamic.ParentIndex
import inca.runtime.index.virtual.SizeIndex
import inca.util.Gensym
import inca.util.Meta._
import truechange.{AnyType, JavaLitType, ListType, SortType}

import scala.meta._

object CompileToPSystem {
  val PARAMPREFIX = "param_"
  val VARPREFIX = "var_"
  val LITPREFIX = "lit_"

  val oNodeTypeKey = objectOf(NodeTypeKey)
  val oPrimitiveKey = objectOf(PrimitiveTypeKey)
  val oLinkNodeKey = objectOf(LinkNodeKey)
  val oLinkPrimitiveKey = objectOf(LinkPrimitiveKey)
  val oLinkListNextKey = objectOf(LinkListNextKey)

  val oParentKey = objectOf(ParentIndex.Key)
  val oSizeKey = objectOf(SizeIndex.Key)

  val tAnyType = objectOf(AnyType)
  val tNodeType = symbolOf[SortType]
  val tListType = symbolOf[ListType]
  val tPrimitiveType = symbolOf[JavaLitType]

  val tyPSystemModule = typeOf[PSystem.Module]

  def genQueryName(moduleName: String, patName: String): String =
    s"${moduleName}_${patName}"

  /** Maps rule name to the name of the module that defines it. */
  type RuleEnvironment = Map[String, String]

  def transAnalysis(modules: Seq[Module]): Seq[Source] = {
    val env: RuleEnvironment = modules.flatMap(m => m.pats.map(p => p.name -> m.name)).toMap

    //TODO What is the exact visibiltity?
    modules.map(transModule(_)(env))
  }


  private def transModule(module: Module)(implicit env: RuleEnvironment): Source = {
    val myenv = env ++ module.pats.map(p => p.name -> module.name) // makes sure this module's names are found first
    val funs = module.pats.map(transGraphPattern(module.name, _)(myenv)).toList

    val name = Term.Name(module.name)
    source"""
      import org.eclipse.viatra.query.runtime.api.{GenericPatternMatcher, ViatraQueryEngine}
      import org.eclipse.viatra.query.runtime.api.scope.{QueryScope => ViatraQueryScope}
      import org.eclipse.viatra.query.runtime.matchers.psystem.{PBody, PVariable}
      import org.eclipse.viatra.query.runtime.matchers.psystem.queries.{BasePQuery, PParameter, PVisibility}
      import org.eclipse.viatra.query.runtime.matchers.tuple.Tuples
      import org.eclipse.viatra.query.runtime.matchers.psystem.basicdeferred.ExportedParameter

      import java.util

      import inca.runtime.Query
      import inca.runtime.context.QueryScope
      import inca.runtime.index.dynamic.ParentIndex
      import inca.runtime.index.virtual.SizeIndex

      import org.eclipse.viatra.query.runtime.matchers.psystem.basicdeferred._
      import org.eclipse.viatra.query.runtime.matchers.psystem.basicenumerables._
      import inca.runtime.index.MetaElements._

      object $name extends ${Init(tyPSystemModule, Term.Name(tyPSystemModule.toString), List())} {
        val patterns: Map[String, () => Query.Specification] = Map(..${
          module.pats.map(p => q"${p.name} -> (() => ${Term.Name(p.name)}.instance)").toList
        })

        ..${funs}
      }
    """
  }

  private def transGraphPattern(moduleName: String, pat: Pattern)(implicit env: RuleEnvironment): Stat = {
    val qname = CompileToPSystem.genQueryName(moduleName, pat.name)

    val paramNames = pat.params.map(_.name)
    val paramTermNames = paramNames.map { n => Term.Name(s"$PARAMPREFIX${n}") }
    val paramLitName = paramNames.map { n => Lit.String(n) }
    val allVars = CollectVars(pat).toSet
    val gensym = new Gensym(allVars)

    val vis =
      if (pat.vis.contains(GP.Private))
        q"PVisibility.PRIVATE"
      else
        q"PVisibility.PUBLIC"

    // TODO there is a hard coded package for the resulting class
    q"""
      object ${Term.Name(pat.name)} {
        lazy val instance: Query.Specification = new Query.Specification(GeneratedPQuery)

        private final object GeneratedPQuery extends BasePQuery($vis) {
          ..${pat.params.map(genPParam).toList}
          {}
          override protected def doGetContainedBodies(): util.Set[PBody] = {
            val bodies: util.Set[PBody] = util.Set.of(
              ..${pat.bodies.map { body =>
                    q"""{
                        val body: PBody = new PBody(this)
                        ..${pat.params.map(genBodyParam).toList}
                        ()
                        val exportedParams = new util.ArrayList[ExportedParameter]()
                        ..${pat.params.map { param =>
                              q"""exportedParams.add(new ExportedParameter(body,
                                ${Term.Name(s"$VARPREFIX${param.name}")},
                                ${Term.Name(s"$PARAMPREFIX${param.name}")}))"""
                          }.toList
                        }
                        body.setSymbolicParameters(exportedParams)

                        ..${(CollectVars.transBody(body).distinct.diff(paramNames)).map(genTempVar).toList}
                        ..${CollectLits.transBody(body).distinct.map(genLiteralVar(_)(gensym)).toList}
                        ..${pat.params.flatMap(genParamConstraint).toList}
                        ..${body.constraints.flatMap(genConstraints).toList}
                        body
                      }"""
                  }.toList
              }
            )
            bodies
          }

          override def getFullyQualifiedName: String = $qname
          override def getParameters: util.List[PParameter] = util.List.of(..${paramTermNames.toList})
          override def getParameterNames: util.List[String] = util.List.of(..${paramLitName.toList})
        }
    }"""
  }

  def genPParam(param: Param): Stat = {
    val pparam = param.typ match {
      case Some(typ) =>
        val gentyp = genType(typ)
        val key = genInputKey(typ)
        q"new PParameter(${Lit.String(param.name)}, $gentyp.toString, $key)"
      case None =>
        q"new PParameter(${Lit.String(param.name)})"
    }
    q"private val ${Pat.Var(Term.Name(s"$PARAMPREFIX${param.name}"))}: PParameter = $pparam"
  }

  def genParamConstraint(param: Param): Option[Stat] = param.typ match {
    case Some(typ) =>
      val key = genInputKey(typ)
      Some(q"""new TypeConstraint(
            body,
            Tuples.flatTupleOf(${Term.Name(s"$VARPREFIX${param.name}")}),
            $key)""")
    case None => None
  }


  def genInputKey(typ: GP.TypeAnno): meta.Term = {
    val gentyp = genType(typ)
    typ match {
      case TBool | TInt | TLong | TDouble | TString => q"$oPrimitiveKey($gentyp)"
      case TAnyLinked | _:TNode | _:TList => q"$oNodeTypeKey($gentyp)"
    }
  }

  def genBodyParam(param: Param): Stat =
    q"""val ${Pat.Var(Term.Name(VARPREFIX + param.name))}: PVariable =
          body.getOrCreateVariableByName(${Lit.String(param.name)})"""

  def genTempVar(name: String): Stat =
    q"val ${Pat.Var(Term.Name(VARPREFIX + name))}: PVariable = body.getOrCreateVariableByName(${Lit.String(name)})"

  def genLiteralVar(lit: Literal)(implicit gensym: Gensym): Stat = {
    val varName = genLiteralVarName(lit)
    q"val ${Pat.Var(Term.Name(LITPREFIX + varName))}: PVariable = body.newConstantVariable(${genLiteral(lit)})"
  }

  def genLiteralVarName(lit: Literal): String = lit match {
    case IntLiteral(v) => "int" + v.hashCode()
    case LongLiteral(v) => "long" + v.hashCode()
    case DoubleLiteral(v) => "double" + v.hashCode()
    case StringLiteral(v) => "string" + v.hashCode
    case BooleanLiteral(v) => "boolean" + v.hashCode()
  }

  def genLiteral(lit: Literal): Lit = lit match {
    case IntLiteral(v) => Lit.Int(v)
    case LongLiteral(v) => Lit.Long(v)
    case DoubleLiteral(v) => Lit.Double(v)
    case StringLiteral(v) => Lit.String(v)
    case BooleanLiteral(v) => Lit.Boolean(v)

  }

  def genConstraints(constraint: Constraint)(implicit env: RuleEnvironment): Seq[Stat] = constraint match {
    case Call(name, args, transitive, neg) =>
      val module = env.getOrElse(name, throw new IllegalArgumentException(s"Unknown rule $name"))
      val argTuple = q"Tuples.flatTupleOf(..${args.map(transTerm).toList})"
      val callQuery = q"${Term.Name(module)}.${Term.Name(name)}.instance.getInternalQueryRepresentation"
      if (neg) Seq(q"new NegativePatternCall(body, $argTuple, $callQuery)")
      else
        if (transitive)
          Seq(q"new BinaryTransitiveClosure(body, $argTuple, $callQuery)")
        else
          Seq(q"new PositivePatternCall(body, $argTuple, $callQuery)")
    case Compare(EqComparator, lhs, rhs) =>
      Seq(q"""new Equality(body, ${transTerm(lhs)}, ${transTerm(rhs)})""")
    case Compare(NeqComparator, lhs, rhs) =>
      Seq(q"""new Inequality(body, ${transTerm(lhs)}, ${transTerm(rhs)})""")
    case HasType(t, typ) =>
      // TODO if type is not enumerable emit TypeFilterConstraint (only needed when we introduce lattices)
      Seq(q"""new TypeConstraint(
            body,
            Tuples.flatTupleOf(${transTerm(t)}),
            $oNodeTypeKey(${genType(typ)}))""")
    case Path(src, trg, link, targetType) =>
      val key = genLinkKey(link, targetType)
      Seq(q"new TypeConstraint(body, Tuples.staticArityFlatTupleOf(${transTerm(src)}, ${transTerm(trg)}), $key)")

    case Computed(resultVar, computation) => transComputation(resultVar, computation)
  }

  def genLinkKey(link: Link, targetType: GP.TypeAnno): meta.Term = link match {
    case GP.ParentLink => oParentKey
    case GP.NextLink => oLinkListNextKey
    case GP.SizeLink => oSizeKey
    case GP.NamedLink(TNode(name), field) => targetType match {
      case _: GP.TLinked =>
        q"$oLinkNodeKey(($name, $field))"
      case GP.TBool | GP.TInt | GP.TLong | GP.TDouble | GP.TString =>
        q"$oLinkPrimitiveKey(($name, $field))"
    }

  }

  def transTerm(v: GP.Term): meta.Term = v match {
    case Var(name) => Term.Name(s"$VARPREFIX$name")
    case Constant(lit) => Term.Name(s"$LITPREFIX${genLiteralVarName(lit)}")
  }

  def transComputation(resultVar: GP.Var, computation: Computation)(implicit env: RuleEnvironment): Seq[Stat] = computation match {
    case CountAggregation(name, args) =>
      val result = transTerm(resultVar)
      val module = env.getOrElse(name, throw new IllegalArgumentException(s"Unknown rule $name"))
      val argTuple = q"Tuples.flatTupleOf(..${args.map(transTerm).toList})"
      val callQuery = q"${Term.Name(module)}.${Term.Name(name)}.instance.getInternalQueryRepresentation"
      Seq(q"new PatternMatchCounter(body, $argTuple, $callQuery, $result)")

    case Evaluation(usedvars, _, code) =>
      val result = transTerm(resultVar)
      val description = s"eval($code)"
      val codeTerm = code.parse[Stat].get
      Seq(
        q"""
        new ExpressionEvaluation(body, new org.eclipse.viatra.query.runtime.matchers.psystem.IExpressionEvaluator {
          override def getShortDescription: String = $description
          override def getInputParameterNames: java.lang.Iterable[String] = java.util.Arrays.asList(..${usedvars.toList.map(Lit.String.apply)})
          override def evaluateExpression(env: org.eclipse.viatra.query.runtime.matchers.psystem.IValueProvider): Any = {$codeTerm}
        }, $result)
         """)

    case LatticeAggregation() => ???
  }

  private def genType(typ: GP.TypeAnno): meta.Term = typ match {
    case TBool => q"$tPrimitiveType(classOf[java.lang.Boolean])"
    case TInt => q"$tPrimitiveType(classOf[java.lang.Integer])"
    case TLong => q"$tPrimitiveType(classOf[java.lang.Long])"
    case TDouble => q"$tPrimitiveType(classOf[java.lang.Double])"
    case TString => q"$tPrimitiveType(classOf[java.lang.String])"
    case TAnyLinked => tAnyType
    case TNode(name) => q"$tNodeType($name)"
    case TList(ty) =>
      val tygen = genType(ty)
      q"$tListType($tygen)"
  }

}
