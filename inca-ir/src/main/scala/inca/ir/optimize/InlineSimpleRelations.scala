package inca.ir.optimize

import inca.ir
import inca.ir.Hint.Key
import inca.ir.extension.aggregate.Aggregate
import inca.ir.{Atom, Body, Call, Hint, ModuleEntry, Name, Relation, Term, TermArg, Var, WildcardArg}
import inca.ir.extension.impure.MainHint
import inca.ir.visitors.IRVisitor
import inca.util.Gensym

object NoInlineHint extends Hint, Hint.Key:
  override def key: Key = this

trait InlineSimpleRelations extends IRVisitor:
  override def name: String = "InlineSimpleRelations"

  enum Phase:
    case FindInlineableRelations
    case RemoveFalsePositives
    case InlineRelations
  private var phase: Phase = _

  val maxAtomsToInline = 100

  var gensym: Gensym = Gensym()

  private var inlineableRelations: Map[Name, Relation] = _
  private var noneInlineableRelations: Set[Name] = _

  private def shouldInline(relName: Name): Boolean =
    inlineableRelations.contains(relName) && ! noneInlineableRelations.contains(relName)

  override def visitModule(module: ir.Module): ir.Module =
    inlineableRelations = Map()
    noneInlineableRelations = Set()

    phase = Phase.FindInlineableRelations
    super.visitModule(module)
    phase = Phase.RemoveFalsePositives
    super.visitModule(module)
    phase = Phase.InlineRelations
    super.visitModule(module)

  private def isInlineable(relation: Relation): Boolean =
    val isMain = relation.hasHint(MainHint)
    val isNoInline = relation.hasHint(NoInlineHint)
    val singleBody = relation.bodies.size == 1
    val isSmall = relation.bodies.head.atoms.size < maxAtomsToInline
    !isMain && !isNoInline && singleBody && isSmall

  private var currentRelation: Relation = _

  override def visitRelation(relation: Relation): Seq[Relation] =
    currentRelation = relation
    phase match
      case Phase.FindInlineableRelations =>
        if isInlineable(relation) then
          inlineableRelations += relation.name -> relation
        super.visitRelation(relation)
      case Phase.RemoveFalsePositives =>
        super.visitRelation(relation)
      case Phase.InlineRelations =>
        if shouldInline(relation.name) then
          Seq()
        else
          super.visitRelation(relation)

  override def visitBody(body: Body): Seq[Body] = gensym.scoped {
    gensym.register(body.atoms.flatMap(_.vars).map(_.name.name))
    super.visitBody(body)
  }

  var varSubstitution: Map[Name, Name] = Map()
  var paramSubstitution: Map[Name, Seq[Term]] = Map()

  def withSubstitutions[A](paramSubst: Map[Name, Seq[Term]], varSubst: Map[Name, Name])(f: => A): A = {
    val oldVarSubstitution = this.varSubstitution
    val oldParamSubstitution = this.paramSubstitution

    this.paramSubstitution = paramSubst
    this.varSubstitution = varSubst

    try {
      val a = f
      a
    } finally {
      this.paramSubstitution = oldParamSubstitution
      this.varSubstitution = oldVarSubstitution
    }
  }

  override def visitAtom(atom: Atom): Seq[Atom] = phase match
    case Phase.FindInlineableRelations => atom match
      case Call(ref, _, true) =>
        // we don't inline relations used in negative calls
        noneInlineableRelations += ref.name
        super.visitAtom(atom)
      case Call(ref, _, _) if ref.name == currentRelation.name =>
        // Don't inline direct recursive calls
        noneInlineableRelations += currentRelation.name
        super.visitAtom(atom)
      case Aggregate(ref, args, op) =>
        // we can not inline relations that we aggregate over
        noneInlineableRelations += ref.name
        super.visitAtom(atom)
      case _ =>
        super.visitAtom(atom)
    case Phase.RemoveFalsePositives => atom match
      case Call(ref, _, _) if shouldInline(ref.name) =>
        // if the current relation calls an already inlineable relation, we don't inline it itself
        noneInlineableRelations += currentRelation.name
        super.visitAtom(atom)
      case _ =>
        super.visitAtom(atom)
    case Phase.InlineRelations => atom match
      case Call(ref, args, _) if shouldInline(ref.name) =>
        val relation = inlineableRelations(ref.name)

        val body = relation.bodies.head

        val paramSubst = relation.params.zip(args).map {
          case (p, TermArg(t)) => p.name -> super.visitTerm(t)
          case (p, WildcardArg()) => p.name -> Seq(Var(gensym.freshName(p.name)))
          case (_, arg) => throw new RuntimeException(s"Unexpected argument $arg")
        }.toMap

        val varSubst = body.atoms.flatMap(_.vars).flatMap {
          case v if !paramSubst.contains(v.name) => Some(v.name -> gensym.freshName(v.name))
          case _ => None
        }.toMap

        withSubstitutions(paramSubst, varSubst) {
          body.atoms.flatMap(super.visitAtom)
        }
      case _ =>
        super.visitAtom(atom)

  override def visitTerm(term: Term): Seq[Term] = phase match
    case Phase.InlineRelations => term match
      case v: Var if varSubstitution.contains(v.name) =>
        val freshName = varSubstitution(v.name)
        Seq(Var(freshName))
      case v: Var if paramSubstitution.contains(v.name) =>
        paramSubstitution(v.name)
      case _ =>
        super.visitTerm(term)
    case _ =>
      super.visitTerm(term)


