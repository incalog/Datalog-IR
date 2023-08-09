package inca.ir.visitors

import inca.ir
import inca.ir.extensions.*
import inca.ir.*

import scala.collection.immutable.Seq

trait BaseIRVisitor:
  def visit(module: ir.Module): ir.Module =
    ir.Module(module.name, module.lang, module.contents.flatMap(visitModuleEntry))

  def visitModuleEntry(moduleEntry: ModuleEntry): Seq[ModuleEntry] =(moduleEntry match {
    case rel: Relation => visitRelation(rel)
    case _ => throw IllegalStateException(s"Can not visit unknown entry: $moduleEntry")
  }).map(_.withHints(moduleEntry))

  def visitRelation(relation: Relation): Seq[Relation] =
    Seq(Relation(relation.name, relation.params.flatMap(visitParam), relation.bodies.flatMap(visitBody)))

  def visitParam(param: Param): Seq[Param] =
    Seq(Param(param.name, visitType(param.ty)).withHints(param))

  def visitBody(body: Body): Seq[Body] =
    Seq(Body(body.atoms.flatMap(visitAtom)).withHints(body))

  def visitAtom(atom: Atom): Seq[Atom] = (atom match {
    case Call(name, args) => Seq(Call(name, args.flatMap(visitTerm)))
    case NegCall(name, args) => Seq(NegCall(name, args.flatMap(visitTerm)))
    case ExtensionalCall(name, args) => Seq(ExtensionalCall(name, args.flatMap(visitTerm)))
    case NegExtensionalCall(name, args) => Seq(NegExtensionalCall(name, args.flatMap(visitTerm)))
    // TODO: Check that lhs and rhs has the same size
    case Eq(lhs, rhs) => visitTerm(lhs).zip(visitTerm(rhs)).map(Eq.apply)
      (visitTerm(lhs), visitTerm(rhs)) match
    case Neq(lhs, rhs) => visitTerm(lhs).zip(visitTerm(rhs)).map(Neq.apply)
    case _ => throw IllegalStateException(s"Can not visit unknown atom: $atom")
  }).map(_.withHints(atom))

  def visitTerm(term: Term): Seq[Term] = (term match {
    case Var(name) => Seq(Var(name))
    case _ => throw IllegalStateException(s"Can not visit unknown term: $term")
  }).map(_.withHints(term))

  def visitType(ty: Type): Type = ty match {
    case TAny => TAny.withHints(ty)
    case TNothing => TNothing.withHints(ty)
    case _ => throw IllegalStateException(s"Can not visit unknown type: $ty")
  }
