package inca.ir.visitors

import inca.ir
import inca.ir.*
import inca.ir.Hint.preserveHints

import scala.collection.immutable.Seq

trait BaseIRVisitor:
  def visit(module: ir.Module): ir.Module =
    ir.Module(module.name, module.lang, module.contents.flatMap(visitModuleEntry))

  def visitModuleEntry(moduleEntry: ModuleEntry): Seq[ModuleEntry] = preserveHints(moduleEntry)(moduleEntry match {
    case rel: Relation => visitRelation(rel)
    case _ => throw IllegalStateException(s"Can not visit unknown entry: $moduleEntry")
  })

  def visitRelation(relation: Relation): Seq[Relation] = preserveHints(relation) {
    Seq(Relation(relation.name, relation.params.flatMap(visitParam), relation.bodies.flatMap(visitBody)))
  }

  def visitParam(param: Param): Seq[Param] = preserveHints(param) {
    Seq(Param(param.name, visitType(param.ty)))
  }

  def visitBody(body: Body): Seq[Body] = preserveHints(body) {
    Seq(Body(body.atoms.flatMap(visitAtom)))
  }

  def visitAtom(atom: Atom): Seq[Atom] = preserveHints(atom)(atom match {
    case Call(name, args) => Seq(Call(name, args.flatMap(visitTerm)))
    case NegCall(name, args) => Seq(NegCall(name, args.flatMap(visitTerm)))
    case ExtensionalCall(name, args) => Seq(ExtensionalCall(name, args.flatMap(visitTerm)))
    case NegExtensionalCall(name, args) => Seq(NegExtensionalCall(name, args.flatMap(visitTerm)))
    // TODO: Check that lhs and rhs has the same size
    case Eq(lhs, rhs) => visitTerm(lhs).zip(visitTerm(rhs)).map(Eq.apply)
    case Neq(lhs, rhs) => visitTerm(lhs).zip(visitTerm(rhs)).map(Neq.apply)
    case _ => throw IllegalStateException(s"Can not visit unknown atom: $atom")
  })

  def visitTerm(term: Term): Seq[Term] = preserveHints(term)(term match {
    case Var(name) => Seq(Var(name))
    case _ => throw IllegalStateException(s"Can not visit unknown term: $term")
  })

  def visitType(ty: Type): Type = preserveHints(ty)(ty match {
    case TAny => TAny
    case TNothing => TNothing
    case _ => throw IllegalStateException(s"Can not visit unknown type: $ty")
  })
