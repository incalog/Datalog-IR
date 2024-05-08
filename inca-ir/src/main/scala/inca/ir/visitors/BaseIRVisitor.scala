package inca.ir.visitors

import inca.ir
import inca.ir.*
import inca.ir.Hint.preserveHints
import inca.ir.Var.Target

import scala.collection.immutable.Seq

trait BaseIRVisitor:
  case object FailedBody extends Throwable

  // Name used for debugging
  def name: String = ""

  def visitProgram(modules: Seq[ir.Module]): Seq[ir.Module] =
    modules.map(visitModule)

  def visitModule(module: ir.Module): ir.Module =
    ir.Module(module.name, module.lang, module.contents.flatMap(visitModuleEntry))

  def visitModuleEntry(moduleEntry: ModuleEntry): Seq[ModuleEntry] = preserveHints(moduleEntry)(moduleEntry match {
    case rel: Relation => visitRelation(rel)
    case rel: ExtensionalRelation => visitExtensionalRelation(rel)
    case rel: RelationImport => visitRelationImport(rel)
    case rel: RelationExport => visitRelationExport(rel)
    case rel: ExtensionalRelationImport => visitExtensionalRelationImport(rel)
    case rel: ExtensionalRelationExport => visitExtensionalRelationExport(rel)
    case _ => throw IllegalStateException(s"Can not visit unknown entry: $moduleEntry")
  })

  def visitRelationExport(relExport: RelationExport): Seq[RelationExport] = preserveHints(relExport) {
    Seq(RelationExport(relExport.name, relExport.types.map(visitType)))
  }

  def visitExtensionalRelationExport(relExport: ExtensionalRelationExport): Seq[ExtensionalRelationExport] = preserveHints(relExport) {
    Seq(ExtensionalRelationExport(relExport.name, relExport.types.map(visitType)))
  }

  def visitRelationImport(relImport: RelationImport): Seq[RelationImport] = preserveHints(relImport) {
    Seq(RelationImport(relImport.name, relImport.types.map(visitType)))
  }

  def visitExtensionalRelationImport(relImport: ExtensionalRelationImport): Seq[ExtensionalRelationImport] = preserveHints(relImport) {
    Seq(ExtensionalRelationImport(relImport.name,relImport.types.map(visitType)))
  }

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

  def visitRef[Target](ref: Ref[Target]): Ref[Target] = preserveHints(ref)(ref match
    case RefByName(name) => RefByName(name)
    case _ => throw IllegalStateException(s"Can not visit unknown reference: $ref")
  )

  def visitAtom(atom: Atom): Seq[Atom] = preserveHints(atom)(atom match {
    case Call(ref, args, neg) => Seq(Call(visitRef(ref), args.flatMap(visitArg), neg))
    case ExtensionalCall(ref, args, neg) => Seq(ExtensionalCall(visitRef(ref), args.flatMap(visitArg), neg))
    case Eq(lhs, rhs, neg) => visitTerm(lhs).zip(visitTerm(rhs)).map((l, r) => Eq(l, r, neg))
    case _ => throw IllegalStateException(s"Can not visit unknown atom: $atom")
  })

  def visitArg(arg: Arg): Seq[Arg] = arg match
    case TermArg(t) => visitTerm(t).map(TermArg.apply)
    case WildcardArg() => Seq(WildcardArg())

  def visitTerm(term: Term): Seq[Term] = preserveHints(term)(term match {
    case Var(ref) => Seq(Var(visitRef(ref)))
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
