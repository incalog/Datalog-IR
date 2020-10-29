package inca.backend.ir


import inca.backend.ir.GP._
import inca.runtime.Query
import inca.runtime.aggregate.{AggregatorAssocComm, AggregatorAssocCommInv}
import inca.runtime.index._
import inca.runtime.index.dynamic.ParentIndex
import inca.runtime.index.virtual.{NodeNotLinkedIndex, NotNodeTypeIndex, SizeIndex}
import inca.util.Meta._
import inca.util.{Gensym, Meta}
import org.eclipse.viatra.query.runtime.matchers.psystem.aggregations.BoundAggregator
import org.eclipse.viatra.query.runtime.matchers.psystem.basicdeferred.AggregatorConstraint
import truechange.{AnyType, JavaLitType, ListType, SortType}

import scala.meta._

object CompileToPSystem {
  val PARAMPREFIX = "param_"
  val VARPREFIX = "var_"
  val LITPREFIX = "lit_"

  private val oNodeTypeKey = symbolOf(NodeTypeKey)
  private val oNotNodeTypeKey = symbolOf(NotNodeTypeIndex.Key)
  private val oPrimitiveKey = symbolOf(PrimitiveTypeKey)
  private val oLinkNodeKey = symbolOf(LinkNodeKey)
  private val oLinkPrimitiveKey = symbolOf(LinkPrimitiveKey)
  private val oLinkListNextKey = symbolOf(LinkListNextKey)

  private val oParentKey = symbolOf(ParentIndex.Key)
  private val oSizeKey = symbolOf(SizeIndex.Key)
  private val oNotLinkNodeKey = symbolOf(NodeNotLinkedIndex.Key)

  private val tAnyType = symbolOf(AnyType)
  private val tNodeType = symbolOf[SortType]
  private val tListType = symbolOf[ListType]
  private val tPrimitiveType = symbolOf[JavaLitType]

  private val tyPSystemModule = typeOf[PSystem.Module]

  private val tyQuerySpecification = typeOf[Query.Specification]

  private val tAggregatorAssocCommInv = typeOf[AggregatorAssocCommInv[_]]
  private val tAggregatorAssocComm = typeOf[AggregatorAssocComm[_]]
  private val tBoundAggregator = typeOf[BoundAggregator]
  private val tAggregatorConstraint = typeOf[AggregatorConstraint]


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

      import org.eclipse.viatra.query.runtime.matchers.context.common.JavaTransitiveInstancesKey

      import java.util

      import org.eclipse.viatra.query.runtime.matchers.psystem.basicdeferred._
      import org.eclipse.viatra.query.runtime.matchers.psystem.basicenumerables._

      object $name extends ${Init(tyPSystemModule, Term.Name(tyPSystemModule.toString), List())} {
        val patterns: Map[String, () => $tyQuerySpecification] = Map(..${
          module.pats.map(p => q"${p.name} -> (() => ${Term.Name(p.name)}.instance)").toList
        })

        ..${funs}

        ..${module.stats.toList}
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
                    val constantEvals = CollectComputedConstantEvals.transBody(body)
                    val lhsOfConstantEvaluation = CollectConstantEvaluationLhs.transBody(body)
                    q"""{
                        val body: PBody = new PBody(this)
                        ..${pat.params.filter{ p => !lhsOfConstantEvaluation.contains(p.name) }.map(genBodyParam).toList}
                        ..${constantEvals.map { case (lhs, e) => genConstantEvaluationVar(lhs, e) }.toList}
                        ()
                        val exportedParams = new util.ArrayList[ExportedParameter]()
                        ..${pat.params.map { param =>
                              q"""exportedParams.add(new ExportedParameter(body,
                                ${Term.Name(s"$VARPREFIX${param.name}")},
                                ${Term.Name(s"$PARAMPREFIX${param.name}")}))"""
                          }.toList
                        }
                        body.setSymbolicParameters(exportedParams)

                        ..${(CollectVars.transBody(body).distinct.diff(paramNames ++ CollectConstantEvaluationLhs.transBody(body))).map(genTempVar).toList}
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
    val pparam = genInputKeyAndType(param.typ) match {
      case Some((key,gentyp)) =>
          q"new PParameter(${Lit.String(param.name)}, $gentyp.toString, $key)"
      case None =>
        q"new PParameter(${Lit.String(param.name)})"
    }
    q"private val ${Pat.Var(Term.Name(s"$PARAMPREFIX${param.name}"))}: PParameter = $pparam"
  }

  private def genParamConstraint(param: Param): Option[Stat] = {
    param.typ match {
      case TUnbounded(_) => return None
      case _ => // continue
    }

    genInputKeyAndType(param.typ) match {
      case Some((key, _)) =>
        Some(
          q"""new TypeConstraint(
                body,
                Tuples.flatTupleOf(${Term.Name (s"$VARPREFIX${param.name}")}),
                $key)""")
      case None => None
    }
  }



  private def genInputKeyAndType(typ: GP.TypeAnno): Option[(meta.Term, meta.Term)] = typ match {
    case TAny => None
    case TDataType(_) => None
    case TUnbounded(ty) =>
      val gentyp = genProperLitType(ty)
      Some(q"new JavaTransitiveInstancesKey($gentyp)", genProperLitType(ty))
    case TBool | TInt | TLong | TDouble | TString =>
      val gentyp = genLitType(typ)
      Some(q"$oPrimitiveKey($gentyp)", gentyp)
    case TAnyLinked | _: TNode | _: TList =>
      val gentyp = genNodeType(typ)
      Some(q"$oNodeTypeKey($gentyp)", gentyp)
  }


  private def genBodyParam(param: Param): Stat =
    q"""val ${Pat.Var(Term.Name(VARPREFIX + param.name))}: PVariable =
          body.getOrCreateVariableByName(${Lit.String(param.name)})"""

  private def genTempVar(name: String): Stat =
    q"val ${Pat.Var(Term.Name(VARPREFIX + name))}: PVariable = body.getOrCreateVariableByName(${Lit.String(name)})"

  private def genConstantEvaluationVar(lhs: GP.Term, eval: ConstantEvaluation): Stat = {
    val result = compileTerm(lhs)
    val codeTerm = eval.code.parse[meta.Term].get
    val resultName = result match {
      case n@Term.Name(_) => n
      case _ => throw new IllegalArgumentException(s"Expected Term.Name, but got $result")
    }
    q"val ${Pat.Var(resultName)}: PVariable = body.newConstantVariable($codeTerm)"
  }

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
      if (typ == TAny)
        Seq()
      else {
        val gentyp = genNodeType(typ)
        Seq(q"""new TypeConstraint(
            body,
            Tuples.flatTupleOf(${compileTerm(t)}),
            $oNodeTypeKey($gentyp))""")
      }

    case NotHasType(t, typ) =>
      if (typ == TAny)
        throw new IllegalArgumentException(s"Cannot compile $constraint")
      else {
        val gentyp = genNodeType(typ)
        Seq(q"""new TypeFilterConstraint(
            body,
            Tuples.flatTupleOf(${compileTerm(t)}),
            $oNotNodeTypeKey($gentyp))""")
      }

    case Path(src, srcTy, link, trg, trgTy) =>
      val key = genLinkKey(link, trgTy)
      Seq(q"new TypeConstraint(body, Tuples.staticArityFlatTupleOf(${compileTerm(src)}, ${compileTerm(trg)}), $key)")

    case NoPath(t, ty, link, termIsSource) =>
      val nodeKey = q"$oNodeTypeKey(${genNodeType(ty)})"
      val linkKey = genLinkKey(link, GP.TAnyLinked)
      val key = q"$oNotLinkNodeKey($nodeKey, $linkKey, $termIsSource)"
      Seq(q"new TypeConstraint(body, Tuples.staticArityFlatTupleOf(${compileTerm(t)}), $key)")

    case Computed(lhs, computation) => compileComputation(lhs, computation)
  }

  private def genLinkKey(link: Link, targetType: GP.TypeAnno): meta.Term = link match {
    case GP.ParentLink => oParentKey
    case GP.NextLink => oLinkListNextKey
    case GP.SizeLink => oSizeKey
    case GP.NamedLink(TNode(name), field) => targetType match {
      case TAny => throw new IllegalArgumentException(s"Cannot resolve links to type $targetType")
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
    case CountAggregation(patName, args) =>
      val result = compileTerm(lhs)
      val module = env.getOrElse(patName, throw new IllegalArgumentException(s"Unknown rule $patName"))
      val argTuple = q"Tuples.flatTupleOf(..${args.map(compileTerm).toList})"
      val callQuery = q"${Term.Name(module)}.${Term.Name(patName)}.instance.getInternalQueryRepresentation"
      Seq(q"new PatternMatchCounter(body, $argTuple, $callQuery, $result)")

    case ConstantEvaluation(_, _) =>
      // we already emit code earlier to avoid forward references
      Seq()

    case Evaluation(args, _, code) =>
      val result = compileTerm(lhs)
      val description = s"eval(${code.syntax})"
      val paramNames = args.toList.flatMap {
        case (Var(name),_) => Some(Lit.String(name))
        case _ => None
      }
      val argTerms = args.toList.map {
        case (v:Var, ty) => q"env.getValue(${Lit.String(v.name)}).asInstanceOf[${genScalaType(ty)}]"
        case (Constant(lit), ty) => genLiteral(lit)
      }
      Seq(
        q"""
        new ExpressionEvaluation(body, new org.eclipse.viatra.query.runtime.matchers.psystem.IExpressionEvaluator {
          override def getShortDescription: String = $description
          override def getInputParameterNames: java.lang.Iterable[String] = java.util.Arrays.asList(..$paramNames)
          override def evaluateExpression(env: org.eclipse.viatra.query.runtime.matchers.psystem.IValueProvider): Any = {
            $code(..$argTerms)
          }
        }, $result)
         """)

    case CustomAggregation(typ, initOpName, joinOpName, unjoinOpName, patName, args, aggregatedColumn) =>
      val result = compileTerm(lhs)
      val module = env.getOrElse(patName, throw new IllegalArgumentException(s"Unknown rule $patName"))
      val argTuple = q"Tuples.flatTupleOf(..${args.map(compileTerm).toList})"
      val callQuery = q"${Term.Name(module)}.${Term.Name(patName)}.instance.getInternalQueryRepresentation"

      val scalaTyp = genScalaType(typ)
      val initOp = Meta.mkQualName(initOpName)
      val joinOp = Meta.mkQualName(joinOpName)
      val aggOp = unjoinOpName match {
        case Some(unjoin) =>
          val tagg = t"$tAggregatorAssocCommInv[$scalaTyp]"
          q"new $tagg($joinOpName, $initOp, $joinOp, ${Meta.mkQualName(unjoin)})"
        case None =>
          val tagg = t"$tAggregatorAssocComm[$scalaTyp]"
          q"new $tagg($joinOpName, $initOp, $joinOp)"
      }
      val boundAggOp = q"new $tBoundAggregator($aggOp, classOf[$scalaTyp], classOf[$scalaTyp])"
      Seq(q"new $tAggregatorConstraint($boundAggOp, body, $argTuple, $callQuery, $result, $aggregatedColumn)")
  }

  private def genLitType(typ: GP.TypeAnno): meta.Term = typ match {
    case TUnbounded(ty) => genProperLitType(ty)
    case ty => q"$tPrimitiveType(${genProperLitType(ty)})"
  }

  private def genProperLitType(typ: GP.TypeAnno): meta.Term = typ match {
    case TBool => q"classOf[java.lang.Boolean]"
    case TInt => q"classOf[java.lang.Integer]"
    case TLong => q"classOf[java.lang.Long]"
    case TDouble => q"classOf[java.lang.Double]"
    case TString => q"classOf[java.lang.String]"
    case _ => throw new IllegalArgumentException(s"Cannot compile $typ as literal type")
  }


  private def genNodeType(typ: GP.TypeAnno): meta.Term = typ match {
    case TAnyLinked => tAnyType
    case TNode(name) => q"$tNodeType($name)"
    case TList(ty) => q"$tListType(${genNodeType(ty)})"
    case _ => throw new IllegalArgumentException(s"Cannot compile $typ as node type")
  }

  private def genScalaType(typ: GP.TypeAnno): meta.Type = typ match {
    case TUnbounded(ty) => genScalaType(ty)
    case TAny => t"Any"
    case TBool => t"Boolean"
    case TInt => t"Int"
    case TLong => t"Long"
    case TDouble => t"Double"
    case TString => t"String"
    case TDataType(qname) => Meta.mkQualTypename(qname)
    case _: TLinked => typeOf[truechange.URI]
  }

}
