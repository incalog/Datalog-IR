package inca.ir.visitors

import inca.ir
import inca.ir.*
import inca.ir.Hint.preserveHints
import inca.ir.Var.Target

import scala.collection.immutable.Seq

trait BaseIRVisitor:
  case object FailedBody extends Throwable

  def visitProgram(modules: Seq[ir.Module]): Seq[ir.Module] =
    modules.map(visitModule)

  protected def visitModule(module: ir.Module): ir.Module =
    ir.Module(module.name, module.lang, module.contents.flatMap(visitModuleEntry))

  def visitModuleEntry(moduleEntry: ModuleEntry): Seq[ModuleEntry] = preserveHints(moduleEntry)(moduleEntry match {
    case rel: Relation => visitRelation(rel)
    case rel: ExtensionalRelation => visitExtensionalRelation(rel)
    case _ => throw IllegalStateException(s"Can not visit unknown entry: $moduleEntry")
  })

  def visitExtensionalRelation(relation: ExtensionalRelation): Seq[ExtensionalRelation] = preserveHints(relation) {
    Seq(ExtensionalRelation(relation.name, relation.params.flatMap(visitParam)))
  }

  def visitRelation(relation: Relation): Seq[Relation] = preserveHints(relation) {
    Seq(Relation(relation.name, relation.params.flatMap(visitParam), relation.bodies.flatMap(visitBody)))
  }

  def visitParam(param: Param): Seq[Param] = preserveHints(param) {
    Seq(Param(param.name, visitType(param.ty)))
  }

  def visitBody(body: Body): Seq[Body] = preserveHints(body) {
    try Seq(Body(body.atoms.flatMap(visitAtom)))
    catch { case FailedBody => Seq() }
  }

  def visitAtom(atom: Atom): Seq[Atom] = preserveHints(atom)(atom match {
    case Call(ref, args) => Seq(Call(visitRef(ref), args.flatMap(visitTerm)))
    case NegCall(ref, args) => Seq(NegCall(visitRef(ref), args.flatMap(visitTerm)))
    case ExtensionalCall(ref, args) => Seq(ExtensionalCall(visitRef(ref), args.flatMap(visitTerm)))
    case NegExtensionalCall(ref, args) => Seq(NegExtensionalCall(visitRef(ref), args.flatMap(visitTerm)))
    case Eq(lhs, rhs) => visitTerm(lhs).zip(visitTerm(rhs)).map(Eq.apply)
    case Neq(lhs, rhs) => visitTerm(lhs).zip(visitTerm(rhs)).map(Neq.apply)
    case _ => throw IllegalStateException(s"Can not visit unknown atom: $atom")
  })

  def visitRef[Target](ref: Ref[Target]): Ref[Target] = preserveHints(ref)(ref match
    case RefByName(name) => RefByName(name)
    case _ => throw IllegalStateException(s"Can not visit unknown reference: $ref")
  )

  def visitTerm(term: Term): Seq[Term] = preserveHints(term)(term match {
    case Var(name) => Seq(Var(name))
    case Cast(t, ty) =>
      val tty = visitType(ty)
      visitTerm(t).map(Cast(_, tty))
    case _ => throw IllegalStateException(s"Can not visit unknown term: $term")
  })

  def visitType(ty: Type): Type = preserveHints(ty)(ty match {
    case TAny => TAny
    case TNothing => TNothing
    case _ => throw IllegalStateException(s"Can not visit unknown type: $ty")
  })
