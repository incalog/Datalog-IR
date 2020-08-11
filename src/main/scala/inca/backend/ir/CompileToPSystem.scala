package inca.backend.ir


import inca.backend.ir.GP._
import inca.runtime.Query
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

  val oNodeTypeKey = symbolOf(NodeTypeKey)
  val oPrimitiveKey = symbolOf(PrimitiveTypeKey)
  val oLinkNodeKey = symbolOf(LinkNodeKey)
  val oLinkPrimitiveKey = symbolOf(LinkPrimitiveKey)
  val oLinkListNextKey = symbolOf(LinkListNextKey)

  val oParentKey = symbolOf(ParentIndex.Key)
  val oSizeKey = symbolOf(SizeIndex.Key)

  val tAnyType = symbolOf(AnyType)
  val tNodeType = symbolOf[SortType]
  val tListType = symbolOf[ListType]
  val tPrimitiveType = symbolOf[JavaLitType]

  val tyPSystemModule = typeOf[PSystem.Module]

  val tyQuerySpecification = typeOf[Query.Specification]



  def genQueryName(moduleName: String, patName: String): String =
    s"${moduleName}_${patName}"

  /** Maps rule name to the name of the module that defines it. */
  type RuleEnvironment = Map[String, String]

  def compileModules(modules: Seq[Module]): Seq[Source] = {
    val env: RuleEnvironment = modules.flatMap(m => m.pats.map(p => p.name -> m.name)).toMap

    //TODO What is the exact visibiltity?
    modules.map(compileModule(_)(env))
  }


  private def compileModule(module: Module)(implicit env: RuleEnvironment): Source = {
    val myenv = env ++ module.pats.map(p => p.name -> module.name) // makes sure this module's names are found first
    val funs = module.pats.map(compilePattern(module.name, _)(myenv)).toList

    val name = Term.Name(module.name)
    source"""
      import org.eclipse.viatra.query.runtime.api.{GenericPatternMatcher, ViatraQueryEngine}
      import org.eclipse.viatra.query.runtime.api.scope.{QueryScope => ViatraQueryScope}
      import org.eclipse.viatra.query.runtime.matchers.psystem.{PBody, PVariable}
      import org.eclipse.viatra.query.runtime.matchers.psystem.queries.{BasePQuery, PParameter, PVisibility}
      import org.eclipse.viatra.query.runtime.matchers.tuple.Tuples
      import org.eclipse.viatra.query.runtime.matchers.psystem.basicdeferred.ExportedParameter

      import java.util

      import org.eclipse.viatra.query.runtime.matchers.psystem.basicdeferred._
      import org.eclipse.viatra.query.runtime.matchers.psystem.basicenumerables._

      object $name extends ${Init(tyPSystemModule, Term.Name(tyPSystemModule.toString), List())} {
        val patterns: Map[String, () => $tyQuerySpecification] = Map(..${
          module.pats.map(p => q"${p.name} -> (() => ${Term.Name(p.name)}.instance)").toList
        })

        ..${funs}
      }
    """
  }

  private def compilePattern(moduleName: String, pat: Pattern)(implicit env: RuleEnvironment): Stat = {
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

    q"""
      object ${Term.Name(pat.name)} {
        lazy val instance: $tyQuerySpecification = new $tyQuerySpecification(generatedPQuery)

        private final object generatedPQuery extends BasePQuery($vis) {
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
                        ..${body.constraints.flatMap(compileConstraint).toList}
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

  private def genPParam(param: Param): Stat = {
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

  private def genParamConstraint(param: Param): Option[Stat] = param.typ match {
    case Some(typ) =>
      val key = genInputKey(typ)
      Some(q"""new TypeConstraint(
            body,
            Tuples.flatTupleOf(${Term.Name(s"$VARPREFIX${param.name}")}),
            $key)""")
    case None => None
  }


  private def genInputKey(typ: GP.TypeAnno): meta.Term = {
    val gentyp = genType(typ)
    typ match {
      case TBool | TInt | TLong | TDouble | TString => q"$oPrimitiveKey($gentyp)"
      case TAnyLinked | _:TNode | _:TList => q"$oNodeTypeKey($gentyp)"
    }
  }

  private def genBodyParam(param: Param): Stat =
    q"""val ${Pat.Var(Term.Name(VARPREFIX + param.name))}: PVariable =
          body.getOrCreateVariableByName(${Lit.String(param.name)})"""

  private def genTempVar(name: String): Stat =
    q"val ${Pat.Var(Term.Name(VARPREFIX + name))}: PVariable = body.getOrCreateVariableByName(${Lit.String(name)})"

  private def genLiteralVar(lit: Literal)(implicit gensym: Gensym): Stat = {
    val varName = genLiteralVarName(lit)
    q"val ${Pat.Var(Term.Name(LITPREFIX + varName))}: PVariable = body.newConstantVariable(${genLiteral(lit)})"
  }

  private def genLiteralVarName(lit: Literal): String = lit match {
    case IntLiteral(v) => "int" + v.hashCode()
    case LongLiteral(v) => "long" + v.hashCode()
    case DoubleLiteral(v) => "double" + v.hashCode()
    case StringLiteral(v) => "string" + v.hashCode
    case BooleanLiteral(v) => "boolean" + v.hashCode()
  }

  private def genLiteral(lit: Literal): Lit = lit match {
    case IntLiteral(v) => Lit.Int(v)
    case LongLiteral(v) => Lit.Long(v)
    case DoubleLiteral(v) => Lit.Double(v)
    case StringLiteral(v) => Lit.String(v)
    case BooleanLiteral(v) => Lit.Boolean(v)

  }

  private def compileConstraint(constraint: Constraint)(implicit env: RuleEnvironment): Seq[Stat] = constraint match {
    case Call(name, args, transitive, neg) =>
      val module = env.getOrElse(name, throw new IllegalArgumentException(s"Unknown rule $name"))
      val argTuple = q"Tuples.flatTupleOf(..${args.map(compileTerm).toList})"
      val callQuery = q"${Term.Name(module)}.${Term.Name(name)}.instance.getInternalQueryRepresentation"
      if (neg) Seq(q"new NegativePatternCall(body, $argTuple, $callQuery)")
      else
        if (transitive)
          Seq(q"new BinaryTransitiveClosure(body, $argTuple, $callQuery)")
        else
          Seq(q"new PositivePatternCall(body, $argTuple, $callQuery)")
    case Compare(EqComparator, lhs, rhs) =>
      Seq(q"""new Equality(body, ${compileTerm(lhs)}, ${compileTerm(rhs)})""")
    case Compare(NeqComparator, lhs, rhs) =>
      Seq(q"""new Inequality(body, ${compileTerm(lhs)}, ${compileTerm(rhs)})""")
    case HasType(t, typ) =>
      // TODO if type is not enumerable emit TypeFilterConstraint (only needed when we introduce lattices)
      Seq(q"""new TypeConstraint(
            body,
            Tuples.flatTupleOf(${compileTerm(t)}),
            $oNodeTypeKey(${genType(typ)}))""")
    case Path(src, trg, link, targetType) =>
      val key = genLinkKey(link, targetType)
      Seq(q"new TypeConstraint(body, Tuples.staticArityFlatTupleOf(${compileTerm(src)}, ${compileTerm(trg)}), $key)")

    case Computed(lhs, computation) => compileComputation(lhs, computation)
  }

  private def genLinkKey(link: Link, targetType: GP.TypeAnno): meta.Term = link match {
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

  private def compileTerm(v: GP.Term): meta.Term = v match {
    case Var(name) => Term.Name(s"$VARPREFIX$name")
    case Constant(lit) => Term.Name(s"$LITPREFIX${genLiteralVarName(lit)}")
  }

  private def compileComputation(lhs: GP.Term, computation: Computation)(implicit env: RuleEnvironment): Seq[Stat] = computation match {
    case CountAggregation(name, args) =>
      val result = compileTerm(lhs)
      val module = env.getOrElse(name, throw new IllegalArgumentException(s"Unknown rule $name"))
      val argTuple = q"Tuples.flatTupleOf(..${args.map(compileTerm).toList})"
      val callQuery = q"${Term.Name(module)}.${Term.Name(name)}.instance.getInternalQueryRepresentation"
      Seq(q"new PatternMatchCounter(body, $argTuple, $callQuery, $result)")

    case Evaluation(freeVars, _, code) =>
      val result = compileTerm(lhs)
      val description = s"eval($code)"
      val codeTerm = code.parse[Stat].get
      val paramNames = freeVars.toList.flatMap {
        case Var(name) => Some(Lit.String(name))
        case Constant(lit) => None
      }
      Seq(
        q"""
        new ExpressionEvaluation(body, new org.eclipse.viatra.query.runtime.matchers.psystem.IExpressionEvaluator {
          override def getShortDescription: String = $description
          override def getInputParameterNames: java.lang.Iterable[String] = java.util.Arrays.asList(..$paramNames)
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
