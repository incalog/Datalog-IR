package inca.frontend.datalog.lowering

import inca.backend.hints.MagicSetHints
import inca.backend.ir.Datalog
import inca.frontend.datalog.syntax._
import inca.frontend.util.Typeable
import inca.util.Gensym
import inca.util.Scala
import scala.collection.mutable.ListBuffer

class GenerateIR(module: Module) {
  private val gensym: Gensym = new Gensym(Iterable.empty)
  private val genScala = new GenerateScala

  private val generatedPatterns = ListBuffer[Datalog.Pattern]()

  def transModule(): Datalog.Module = {
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
          throw new IllegalArgumentException(
            s"Expected rules for relation ${currentSig.name} but found $rule")
        rules += rule
      case data: DataDef =>
        genScala.genDataDef(data)
    }
    if (currentSig != null)
      generatedPatterns ++= transRelation(currentSig, rules.toSeq)

    Datalog.Module(name.name, Seq(), generatedPatterns.toList, genScala.generated.map(Scala.apply))
  }

  private def transRelation(sig: RuleSig, rules: Seq[Rule]): Option[Datalog.Pattern] =
    gensym.scoped {
      if (sig.hasAnnotation(ExtensionalAnno.key)) {
        if (rules.nonEmpty)
          throw new IllegalArgumentException(s"Extensional relations cannot have rules, in $sig")
        return None
      }

      val params = sig.params.zipWithIndex.map { case (param, i) =>
        Datalog.Param(gensym.fresh(s"x_$i"), transType(param.typ))
      }
      val bodies = rules.map(rule => transRule(sig, params, rule))

      val pat = Datalog.Pattern(None, sig.name.name, params, bodies)
      if (sig.hasAnnotation(MainAnno.key))
        pat.addHint(MagicSetHints.Main(params.map(_ => false)))
      Some(pat)
    }

  private def transRule(sig: RuleSig, params: Seq[Datalog.Param], rule: Rule): Datalog.Body = {
    val (headterms, headConstraints) = rule.headTerms.map(transTerm).unzip
    val paramEqs = params.zip(headterms).map { case (param, term) =>
      Datalog.Eq(Datalog.Var(param.name), term)
    }
    val bodyConstraints = rule.body.flatMap(transAtom)
    Datalog.Body(paramEqs ++ headConstraints.flatten ++ bodyConstraints)
  }

  private def transAtom(a: Atom): Seq[Datalog.Atom] = a match {
    case c @ Call(_, _, _) if c.target.isEmpty =>
      throw new IllegalArgumentException(s"Cannot compile unresolved call $c")
    case c @ Call(name, args, not) if c.target.get.isInstanceOf[RuleSig] =>
      // querying a relation
      val relation = c.target.get.asInstanceOf[RuleSig]
      val (argTerms, argConstraints) = args.map(transTerm).unzip
      val dcall = {
        if (relation.hasAnnotation(ExtensionalAnno.key))
          Datalog.ExtensionalCall(name.name, argTerms, neg = not)
        else
          Datalog.Call(name.name, argTerms, transitive = false, neg = not)
      }
      argConstraints.flatten :+ dcall
    case c @ Call(name, Seq(arg), false) if c.target.get.isInstanceOf[DataDef] =>
      // querying a type
      val (argTerm, argConstraints) = transTerm(arg)
      val hasType = Datalog.HasType(argTerm, Datalog.TNode(name.name))
      argConstraints :+ hasType
    case c @ Call(name, args, false) if c.target.get.isInstanceOf[DataConstructor] =>
      // querying a constructor
      val constructed = args.last
      val (constructedTerm, constructedConstraints) = transTerm(constructed)
      val constructedType = transType(constructed).asInstanceOf[Datalog.TNode]
      val hasType = Datalog.HasType(constructedTerm, constructedType)

      val constr = c.target.get.asInstanceOf[DataConstructor]
      val constrArgs = args.take(args.size - 1)
      val (constrArgTerms, constrArgConstraints) = constrArgs.map(transTerm).unzip
      val paths = constr.params.zip(constrArgTerms).map { case (param, term) =>
        Datalog.Path(
          constructedTerm,
          constructedType,
          Datalog.NamedLink(constructedType, param.name.name),
          term,
          transType(param.typ)
        )
      }
      (constructedConstraints :+ hasType) ++ constrArgConstraints.flatten ++ paths
    case Compare(comp, lhs, rhs) =>
      val (lhsTerm, lhsConstraints) = transTerm(lhs)
      val (rhsTerm, rhsConstraints) = transTerm(rhs)
      val d = comp match {
        case EqComparator => Datalog.Eq(lhsTerm, rhsTerm)
        case NeqComparator => Datalog.Neq(lhsTerm, rhsTerm)
      }
      lhsConstraints ++ rhsConstraints :+ d
  }

  private def transTerm(t: Term): (Datalog.Term, Seq[Datalog.Atom]) = t match {
    case Wildcard() => (Datalog.Var(gensym.fresh("_")), Seq())
    case Var(name) => (Datalog.Var(name.name), Seq())
    case Constant(lit) => (Datalog.Constant(transLit(lit)), Seq())
    case Path(src, link) =>
      val (srcTerm, srcConstraints) = transTerm(src)
      val trgTerm = Datalog.Var(gensym.fresh(link.name))
      val srcTy = transType(src)
      val path = Datalog.Path(
        srcTerm,
        srcTy,
        Datalog.NamedLink(srcTy.asInstanceOf[Datalog.TNode], link.name),
        trgTerm,
        transType(t))
      (trgTerm, srcConstraints :+ path)
  }

  private def transLit(literal: Literal): Datalog.base.Literal = literal match {
    case IntLiteral(v) => Datalog.base.IntLiteral(v)
    case LongLiteral(v) => Datalog.base.LongLiteral(v)
    case DoubleLiteral(v) => Datalog.base.DoubleLiteral(v)
    case StringLiteral(v) => Datalog.base.StringLiteral(v)
    case BooleanLiteral(v) => Datalog.base.BooleanLiteral(v)
  }

  private def transType(hasType: Typeable[Type]): Datalog.Type = hasType.typ match {
    case Some(value) => transType(value)
    case None => throw new IllegalStateException(s"Untyped $hasType")
  }

  private def transType(typ: Type): Datalog.Type = typ match {
    case TAny => Datalog.TAny
    case TData(name) => Datalog.TNode(name.name)
    case TScala(ty) => Datalog.TScala(ty)
    case TLiteral(lit) => Datalog.TLiteral(lit)
    case _ => throw new IllegalArgumentException(s"Cannot translate $typ to Datalog")
  }
}
