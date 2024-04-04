package inca.ir.optimize

import inca.ir
import inca.ir.Hint.Key
import inca.ir.{Atom, Body, Call, Hint, ModuleEntry, Name, Relation, Term, TermArg, Var, WildcardArg}
import inca.ir.extension.impure.MainHint
import inca.ir.visitors.IRVisitor
import inca.util.Gensym

object NoInlineHint extends Hint, Hint.Key:
  override def key: Key = this

trait InlineSimpleRelations extends IRVisitor:
  enum Phase:
    case FindInlineableRelations
    case InlineRelations
  private var phase: Phase = _

  val maxAtomsToInline = 10

  var gensym: Gensym = Gensym()
  
  private var inlineableRelations: Map[Name, Relation] = _

  private def shouldInline(relName: Name): Boolean =
    inlineableRelations.contains(relName)

  override def visitModule(module: ir.Module): ir.Module =
    phase = Phase.FindInlineableRelations
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
  
  override def visitAtom(atom: Atom): Seq[Atom] = phase match
    case Phase.FindInlineableRelations => atom match
      case Call(ref, _, true) =>
        // we don't inline negative calls
        inlineableRelations -= ref.name
        super.visitAtom(atom)
      case Call(ref, _, _) if ref.name == currentRelation.name =>
        // we don't inline direct recursive calls
        inlineableRelations -= ref.name
        super.visitAtom(atom)
      case _ =>
        super.visitAtom(atom)
    case Phase.InlineRelations => atom match
      case Call(ref, args, _) if shouldInline(ref.name) =>
        val relation = inlineableRelations(ref.name)
        val body = relation.bodies.head

        paramSubstitution = relation.params.zip(args).map {
          case (p, TermArg(t)) => p.name -> super.visitTerm(t)
          case (p, WildcardArg()) => p.name -> Seq(Var(gensym.freshName(p.name)))
          case (_, arg) => throw new RuntimeException(s"Unexpected argument $arg")
        }.toMap

        varSubstitution = body.atoms.flatMap(_.vars).flatMap {
          case v if paramSubstitution.contains(v.name) => Some(v.name -> gensym.freshName(v.name))
          case _ => None
        }.toMap
        
        body.atoms.flatMap(super.visitAtom)

  override def visitTerm(term: Term): Seq[Term] =
    phase match
      case Phase.FindInlineableRelations =>
        super.visitTerm(term)
      case Phase.InlineRelations => 
        term match
        case v: Var if varSubstitution.contains(v.name) =>
          val freshName = varSubstitution(v.name)
          Seq(Var(freshName))
        case v: Var if paramSubstitution.contains(v.name) =>
          paramSubstitution(v.name)
        case _ => 
          super.visitTerm(term)


