package inca.frontend.datalog.lowering

import inca.backend.hints.MagicSetHints
import inca.backend.ir.IR
import inca.frontend.datalog.syntax._
import inca.frontend.util.Typeable
import inca.util.{Gensym, Scala}

import scala.collection.mutable.ListBuffer

class GenerateIR(module: Module) {
  private val gensym: Gensym = new Gensym(Iterable.empty)
  private val genScala = new GenerateScala

  private val generatedPatterns = ListBuffer[IR.Pattern]()

  def transModule(): IR.Module = {
    val Module(name, contents) = module
    gensym.register(name.name)
    contents.foreach {
      case DataDef(annos, name, constrs) =>
        gensym.register(name.name)
        constrs.foreach(c => gensym.register(c.name.name))
      case RuleSig(annos, name, params) =>
        gensym.register(name.name)
      case _ =>
    }

    var currentSig: RuleSig = null
    val rules: ListBuffer[Rule] = ListBuffer()
    contents.foreach {
      case sig: RuleSig if currentSig == null =>
        currentSig = sig
      case sig: RuleSig =>
        generatedPatterns ++= transRelation(currentSig, rules.toSeq)
        currentSig = sig
        rules.clear()
      case rule: Rule =>
        if (rule.target.exists(sig => sig != currentSig))
          throw new IllegalArgumentException(s"Expected rules for relation ${currentSig.name} but found $rule")
        rules += rule
      case data: DataDef =>
        genScala.genDataDef(data)
    }
    if (currentSig != null)
      generatedPatterns ++= transRelation(currentSig, rules.toSeq)

    IR.Module(
      name.name,
      Seq(),
      generatedPatterns.toList,
      genScala.generated.map(Scala.apply))
  }

  private def transRelation(sig: RuleSig, rules: Seq[Rule]): Option[IR.Pattern] = gensym.scoped {
    if (sig.hasAnnotation(ExtensionalAnno.key)) {
      if (rules.nonEmpty)
        throw new IllegalArgumentException(s"Extensional relations cannot have rules, in $sig")
      return None
    }

    val params = sig.params.zipWithIndex.map { case (param, i) =>
      IR.Param(gensym.fresh(s"x_$i"), transType(param.typ))
    }
    val bodies = rules.map(rule => transRule(sig, params, rule))

    val pat = IR.Pattern(None, sig.name.name, params, bodies)
    if (sig.hasAnnotation(MainAnno.key))
      pat.addHint(MagicSetHints.Main(params.map(_ => false)))
    Some(pat)
  }

  private def transRule(sig: RuleSig, params: Seq[IR.Param], rule: Rule): IR.Body = {
    val (headterms, headConstraints) = rule.headTerms.map(transTerm).unzip
    val paramEqs = params.zip(headterms).map { case (param, term) =>
      IR.Eq(IR.Var(param.name), term)
    }
    val bodyConstraints = rule.body.flatMap(transAtom)
    IR.Body(paramEqs ++ headConstraints.flatten ++ bodyConstraints)
  }

  private def transAtom(a: Atom): Seq[IR.Atom] = a match {
    case c@Call(_, _, _) if c.target.isEmpty =>
      throw new IllegalArgumentException(s"Cannot compile unresolved call $c")
    case c@Call(name, args, not) if c.target.get.isInstanceOf[RuleSig] =>
      // querying a relation
      val relation = c.target.get.asInstanceOf[RuleSig]
      val (argTerms, argConstraints) = args.map(transTerm).unzip
      val dcall = {
        if (relation.hasAnnotation(ExtensionalAnno.key))
          IR.ExtensionalCall(name.name, argTerms, neg = not)
        else
          IR.Call(name.name, argTerms, transitive = false, neg = not)
      }
      argConstraints.flatten :+ dcall
    case c@Call(name, Seq(arg), false) if c.target.get.isInstanceOf[DataDef] =>
      // querying a type
      val (argTerm, argConstraints) = transTerm(arg)
      val hasType = IR.HasType(argTerm, IR.TNode(name.name))
      argConstraints :+ hasType
    case c@Call(name, args, false) if c.target.get.isInstanceOf[DataConstructor] =>
      // querying a constructor
      val constructed = args.last
      val (constructedTerm, constructedConstraints) = transTerm(constructed)
      val constructedType = transType(constructed).asInstanceOf[IR.TNode]
      val hasType = IR.HasType(constructedTerm, constructedType)

      val constr = c.target.get.asInstanceOf[DataConstructor]
      val constrArgs = args.take(args.size - 1)
      val (constrArgTerms, constrArgConstraints) = constrArgs.map(transTerm).unzip
      val paths = constr.params.zip(constrArgTerms).map { case (param, term) =>
        IR.Path(
          constructedTerm,
          constructedType,
          IR.NamedLink(constructedType, param.name.name),
          term,
          transType(param.typ)
        )
      }
      (constructedConstraints :+ hasType) ++ constrArgConstraints.flatten ++ paths
    case Compare(comp, lhs, rhs) =>
      val (lhsTerm, lhsConstraints) = transTerm(lhs)
      val (rhsTerm, rhsConstraints) = transTerm(rhs)
      val d = comp match {
        case EqComparator => IR.Eq(lhsTerm, rhsTerm)
        case NeqComparator => IR.Neq(lhsTerm, rhsTerm)
      }
      lhsConstraints ++ rhsConstraints :+ d
  }

  private def transTerm(t: Term): (IR.Term, Seq[IR.Atom]) = t match {
    case Wildcard() => (IR.Var(gensym.fresh("_")), Seq())
    case Var(name) => (IR.Var(name.name), Seq())
    case Constant(lit) => (IR.Constant(transLit(lit)), Seq())
    case Path(src, link) =>
      val (srcTerm, srcConstraints) = transTerm(src)
      val trgTerm = IR.Var(gensym.fresh(link.name))
      val srcTy = transType(src)
      val path = IR.Path(srcTerm, srcTy, IR.NamedLink(srcTy.asInstanceOf[IR.TNode], link.name), trgTerm, transType(t))
      (trgTerm, srcConstraints :+ path)
  }

  private def transLit(literal: Literal): IR.Literal = literal match {
    case IntLiteral(v) => IR.IntLiteral(v)
    case LongLiteral(v) => IR.LongLiteral(v)
    case DoubleLiteral(v) => IR.DoubleLiteral(v)
    case StringLiteral(v) => IR.StringLiteral(v)
    case BooleanLiteral(v) => IR.BooleanLiteral(v)
  }

  private def transType(hasType: Typeable[Type]): IR.Type = hasType.typ match {
    case Some(value) => transType(value)
    case None => throw new IllegalStateException(s"Untyped $hasType")
  }

  private def transType(typ: Type): IR.Type = typ match {
    case TAny => IR.TAny
    case TData(name) => IR.TNode(name.name)
    case TScala(ty) => IR.TScala(ty)
    case TLiteral(lit) => IR.TLiteral(lit)
    case _ => throw new IllegalArgumentException(s"Cannot translate $typ to Datalog")
  }
}
