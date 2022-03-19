package inca.backend.ir


import inca.backend.ir.Datalog._
import inca.runtime.Query
import inca.runtime.index._
import inca.runtime.index.dynamic.ParentIndex
import inca.runtime.index.virtual.{NodeNotLinkedIndex, NotNodeTypeIndex, SizeIndex}
import inca.util.Scala._
import org.eclipse.viatra.query.runtime.matchers.psystem.aggregations.BoundAggregator
import org.eclipse.viatra.query.runtime.matchers.psystem.basicdeferred.AggregatorConstraint
import truechange.{AnyType, JavaLitType, ListType, SortType}

import scala.meta._

object GeneratePSystem {
  val PARAMPREFIX = "param_"
  val VARPREFIX = "var_"
  val LITPREFIX = "lit_"
  val EVALPREFIX = "eval_"

  private val oNamedRelationKey = symbolOf(NamedRelationKey)
  private val oNodeTypeKey = symbolOf(NodeTypeKey)
  private val oNotNodeTypeKey = symbolOf(NotNodeTypeIndex.Key)
  private val oPrimitiveKey = symbolOf(PrimitiveTypeKey)
  private val oLinkNodeKey = symbolOf(LinkNodeKey)
  private val oLinkPrimitiveKey = symbolOf(LinkPrimitiveKey)
  private val oLinkListNextKey = symbolOf(LinkListNextKey)

  private val oParentKey = symbolOf(ParentIndex.Key)
  private val oSizeKey = symbolOf(SizeIndex.Key)
  private val oNotLinkNodeKey = symbolOf(NodeNotLinkedIndex.Key)

  private val oAnyType = symbolOf(AnyType)
  private val oNodeType = symbolOf[SortType]
  private val oListType = symbolOf[ListType]
  private val oPrimitiveType = symbolOf[JavaLitType]

  private val tyPSystemModule = typeOf[PSystem.Module]

  private val tyQuerySpecification = typeOf[Query.Specification]

  private val tBoundAggregator = typeOf[BoundAggregator]
  private val tAggregatorConstraint = typeOf[AggregatorConstraint]

  private val tMap = typeOf[Map[_,_]]
  private val oMap = symbolOf(Map)


  def genQueryName(moduleName: String, patName: String): String =
    s"${moduleName}_$patName"

  /** Maps rule name to the name of the module that defines it. */
  type RuleEnvironment = Map[String, String]

  def compileModules(modules: Seq[Module]): Seq[Source] = {
    val env: RuleEnvironment = modules.flatMap(m => m.pats.map(p => p.name -> m.name)).toMap

    modules.map(compileModule(_)(env))
  }

  def compileModule(module: Module)(implicit env: RuleEnvironment): Source = {
    // TODO: handle module.imports

    val myenv = env ++ module.pats.map(p => p.name -> module.name) // makes sure this module's names are found first
    val funs = module.pats.map(compilePattern(module.name, _)(myenv)).toList

    val scalaContent = module.scalaContent.map(_.tree).toList

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


        override val patterns: $tMap[String, () => $tyQuerySpecification] = $oMap(..${
          module.pats.filter(!_.isEmpty).map(p => q"${p.name} -> (() => ${Term.Name(p.name)}.instance)").toList
        })

        ..$scalaContent
        ..$funs
      }
    """
  }

  private def compilePattern(moduleName: String, pat: Pattern)(implicit env: RuleEnvironment): Stat = {
    val qname = GeneratePSystem.genQueryName(moduleName, pat.name)

    val paramNames = pat.params.map(_.name)
    val paramTermNames = paramNames.map { n => Term.Name(s"$PARAMPREFIX$n") }
    val paramLitName = paramNames.map { n => Lit.String(n) }

    val vis =
      if (pat.vis.contains(Datalog.Private))
        q"PVisibility.PRIVATE"
      else
        q"PVisibility.PUBLIC"

    if (pat.isEmpty) {
      val obj =
        q"""
         object ${Term.Name(pat.name)} {
           val error = "This pattern was empty"
         }
        """
      return obj
    }

    val bodies = if (pat.bodies.nonEmpty) pat.bodies else
      Seq(Body(Seq(Compare(EqComparator, Constant(IntLiteral(0)), Constant(IntLiteral(1))))))

    q"""
      object ${Term.Name(pat.name)} {
        lazy val instance: $tyQuerySpecification = new $tyQuerySpecification(generatedPQuery)

        private final object generatedPQuery extends BasePQuery($vis) {
          ..${pat.params.map(genPParam).toList}
          {}
          override protected def doGetContainedBodies(): util.Set[PBody] =
            util.Set.of(
              ..${bodies.map { body =>
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

                        ..${CollectVars.transBody(body).distinct.diff(paramNames).map(genTempVar).toList}
                        ..${CollectLits.transBody(body).distinct.map(genLiteralVar).toList}
                        ..${CollectConstantEvaluation.transBody(body).distinct.map(genConstantEval).toList}
                        ..${pat.params.flatMap(genParamConstraint).toList}
                        ..${body.atoms.flatMap(compileAtom).toList}
                        body
                      }"""
                  }.toList
              }
            )

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



  private def genInputKeyAndType(typ: Datalog.Type): Option[(meta.Term, meta.Term)] = typ match {
    case TAny => None
    case _: TScala => None
    case _: TData => None
    case tlit@TLiteral(litType) =>
      litType match {
        case JavaLitType(_) =>
          val gentyp = q"$oPrimitiveType(classOf[${tlit.asScala}])"
          Some((q"$oPrimitiveKey($gentyp)", gentyp))
        case _ => throw new UnsupportedOperationException
      }
    case _: TLinked =>
      val gentyp = genNodeType(typ)
      Some((q"$oNodeTypeKey($gentyp)", gentyp))
  }


  private def genBodyParam(param: Param): Stat =
    q"""val ${Pat.Var(Term.Name(VARPREFIX + param.name))}: PVariable =
          body.getOrCreateVariableByName(${Lit.String(param.name)})"""

  private def genTempVar(name: String): Stat =
    q"val ${Pat.Var(Term.Name(VARPREFIX + name))}: PVariable = body.getOrCreateVariableByName(${Lit.String(name)})"

  private def genLiteralVar(lit: Literal): Stat = {
    val varName = genLiteralVarName(lit)
    q"val ${Pat.Var(Term.Name(LITPREFIX + varName))}: PVariable = body.newConstantVariable(${genLiteral(lit)})"
  }

  private def genConstantEval(eval: Evaluation): Stat = {
    val varName = genConstantEvalVarName(eval)
    q"val ${Pat.Var(Term.Name(EVALPREFIX + varName))}: PVariable = body.newConstantVariable((${eval.code.tree})())"
  }

  private def genLiteralVarName(lit: Literal): String = lit match {
    case IntLiteral(v) => "int" + v.hashCode()
    case LongLiteral(v) => "long" + v.hashCode()
    case DoubleLiteral(v) => "double" + v.hashCode()
    case StringLiteral(v) => "string" + v.hashCode
    case BooleanLiteral(v) => "boolean" + v.hashCode()
  }

  private def genConstantEvalVarName(eval: Evaluation): String = eval.code.hashCode().toString

  def genLiteral(lit: Literal): Lit = lit match {
    case IntLiteral(v) => Lit.Int(v)
    case LongLiteral(v) => Lit.Long(v)
    case DoubleLiteral(v) => Lit.Double(v)
    case StringLiteral(v) => Lit.String(v)
    case BooleanLiteral(v) => Lit.Boolean(v)
  }

  private def compileAtom(atom: Atom)(implicit env: RuleEnvironment): Seq[Stat] = atom match {
    case Undef(_) =>
      throw new IllegalStateException(s"Cannot compile undef constraint. Use undef elimination transformation first.")

    case Call(name, args, transitive, neg) =>
      val module = env.getOrElse(name, throw new IllegalStateException(s"Unknown rule $name"))
      val argTuple = q"Tuples.flatTupleOf(..${args.map(compileTerm).toList})"
      val callQuery = q"${Term.Name(module)}.${Term.Name(name)}.instance.getInternalQueryRepresentation"
      if (neg) Seq(q"new NegativePatternCall(body, $argTuple, $callQuery)")
      else
        if (transitive)
          Seq(q"new BinaryTransitiveClosure(body, $argTuple, $callQuery)")
        else
          Seq(q"new PositivePatternCall(body, $argTuple, $callQuery)")

    case ExtensionalCall(name, args, neg) =>
      val key = q"$oNamedRelationKey($name, ${args.size})"
      val tuple = q"Tuples.flatTupleOf(..${args.map(compileTerm).toList})"
      if (neg) {
        // use a type filter
        throw new IllegalStateException("Currently do not support negation of extensional call")
      } else {
        Seq(q"new TypeConstraint(body, $tuple, $key)")
      }

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
            Tuples.staticArityFlatTupleOf(${compileTerm(t)}),
            $oNodeTypeKey($gentyp))""")
      }

    case NotHasType(t, typ) =>
      if (typ == TAny)
        throw new IllegalStateException(s"Cannot compile $atom")
      else {
        val gentyp = genNodeType(typ)
        Seq(q"""new TypeFilterConstraint(
            body,
            Tuples.staticArityFlatTupleOf(${compileTerm(t)}),
            $oNotNodeTypeKey($gentyp))""")
      }

    case Path(src, _, link, trg, trgTy) =>
      val key = genLinkKey(link, trgTy)
      Seq(q"new TypeConstraint(body, Tuples.staticArityFlatTupleOf(${compileTerm(src)}, ${compileTerm(trg)}), $key)")

    case NoPath(t, ty, link, termIsSource) =>
      val nodeKey = q"$oNodeTypeKey(${genNodeType(ty)})"
      val linkKey = genLinkKey(link, Datalog.TAnyLinked)
      val key = q"$oNotLinkNodeKey($nodeKey, $linkKey, $termIsSource)"
      Seq(q"new TypeConstraint(body, Tuples.staticArityFlatTupleOf(${compileTerm(t)}), $key)")

    case Computed(lhs, computation) => compileComputation(lhs, computation)
  }

  private def genLinkKey(link: Link, targetType: Datalog.Type): meta.Term = link match {
    case Datalog.ParentLink => oParentKey
    case Datalog.NextLink => oLinkListNextKey
    case Datalog.SizeLink => oSizeKey
    case Datalog.NamedLink(TNode(name), field) => targetType match {
      case TAny => throw new IllegalStateException(s"Cannot resolve links to type $targetType")
      case _: Datalog.TLinked =>
        q"$oLinkNodeKey(($name, $field))"
      case _: Datalog.TLiteral =>
        q"$oLinkPrimitiveKey(($name, $field))"
      case _ => throw new IllegalStateException(s"Generating LinkKey for NamedLink with target type $targetType not supported")
    }
    case _ => throw new IllegalStateException(s"Generating LinkKey for $link not supported")

  }

  private def compileTerm(v: Datalog.Term): meta.Term = v match {
    case Var(name) => Term.Name(s"$VARPREFIX$name")
    case Constant(lit) => Term.Name(s"$LITPREFIX${genLiteralVarName(lit)}")
  }

  private def compileComputation(lhs: Datalog.Term, computation: Computation)(implicit env: RuleEnvironment): Seq[Stat] = computation match {
    case CountAggregation(patName, args) =>
      val result = compileTerm(lhs)
      val module = env.getOrElse(patName, throw new IllegalStateException(s"Unknown rule $patName"))
      val argTuple = q"Tuples.flatTupleOf(..${args.map(compileTerm).toList})"
      val callQuery = q"${Term.Name(module)}.${Term.Name(patName)}.instance.getInternalQueryRepresentation"
      Seq(q"new PatternMatchCounter(body, $argTuple, $callQuery, $result)")

    case eval@Evaluation(args, _, _) if args.isEmpty =>
      val varName = genConstantEvalVarName(eval)
      val rhs = Term.Name(EVALPREFIX + varName)
      Seq(q"""new Equality(body, ${compileTerm(lhs)}, $rhs)""")

    case Evaluation(args, _, code) =>
      val result = compileTerm(lhs)
      val description = s"eval(${code.syntax})"
      val paramNames = args.toList.flatMap {
        case (Var(name),_) => Some(Lit.String(name))
        case _ => None
      }
      val argTerms = args.toList.map {
        case (v:Var, ty) => q"env.getValue(${Lit.String(v.name)}).asInstanceOf[${ty.asScala}]"
        case (Constant(lit), _) => genLiteral(lit)
      }
      Seq(
        q"""
        new ExpressionEvaluation(body, new org.eclipse.viatra.query.runtime.matchers.psystem.IExpressionEvaluator {
          override def getShortDescription: String = $description
          override def getInputParameterNames: java.lang.Iterable[String] = java.util.Arrays.asList(..$paramNames)
          override def evaluateExpression(env: org.eclipse.viatra.query.runtime.matchers.psystem.IValueProvider): Any = {
            ${code.tree}(..$argTerms)
          }
        }, $result)
         """)

    case CustomAggregation(typ, _, agg, patName, args, aggregatedColumn) =>
      val result = compileTerm(lhs)
      val module = env.getOrElse(patName, throw new IllegalStateException(s"Unknown rule $patName"))
      val argTuple = q"Tuples.flatTupleOf(..${args.map(compileTerm).toList})"
      val callQuery = q"${Term.Name(module)}.${Term.Name(patName)}.instance.getInternalQueryRepresentation"

      val scalaTyp = typ.asScala
      val boundAggOp = q"new $tBoundAggregator(${agg.tree}.aggregator, classOf[$scalaTyp], classOf[$scalaTyp])"
      Seq(q"new $tAggregatorConstraint($boundAggOp, body, $argTuple, $callQuery, $result, $aggregatedColumn)")
  }

  private def genNodeType(typ: Datalog.Type): meta.Term = typ match {
    case TAnyLinked => oAnyType
    case TNode(name) => q"$oNodeType($name)"
    case TList(ty) => q"$oListType(${genNodeType(ty)})"
    case _ => throw new IllegalStateException(s"Cannot compile $typ as node type")
  }
}
