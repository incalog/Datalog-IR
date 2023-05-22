package inca.ir.visitors

import inca.ir
import inca.ir.extensions.*
import inca.ir.*

import scala.collection.immutable.Seq

trait BaseIRVisitor:
  def visit(module: ir.Module): ir.Module =
    ir.Module(module.name, module.lang, module.contents.flatMap(visitModuleEntry))

  def visitModuleEntry(moduleEntry: ModuleEntry): Seq[ModuleEntry] = moduleEntry match {
    case rel: Relation => visitRelation(rel)
    case _ => throw IllegalArgumentException(s"Can not visit unknown object: $moduleEntry")
  }

  def visitRelation(relation: Relation): Seq[Relation] =
    Seq(Relation(relation.name, relation.params.flatMap(visitParam), relation.bodies.flatMap(visitBody)))

  def visitParam(param: Param): Seq[Param] =
    Seq(Param(param.name, visitType(param.ty).head))

  def visitBody(body: Body): Seq[Body] =
    Seq(Body(body.atoms.flatMap(visitAtom)))

  def visitAtom(atom: Atom): Seq[Atom] = Seq(atom match {
    case Call(name, terms) => Call(name, terms.flatMap(visitTerm))
    case _ => throw IllegalArgumentException(s"Can not visit unknown atom: $atom")
  })

  def visitTerm(term: Term): Seq[Term] = term match {
    case Var(name) => Seq(Var(name))
    case _ => throw IllegalArgumentException(s"Can not visit unknown term: $term")
  }

  def visitType(ty: Type): Seq[Type] = ty match {
    case TInt => Seq(TInt)
    case _ => throw IllegalArgumentException(s"Can not visit unknown type: $ty")
  }
