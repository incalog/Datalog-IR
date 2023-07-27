package inca.ir.lowering

import inca.ir.{Atom, BaseIR, Body, Call, Module, ModuleEntry, Param, Relation, Term, Var, name2string}
import inca.ir.visitors.IRVisitor

// TODO: Do we still need S and T ??
trait BaseLowering[S <: BaseIR, T <: BaseIR] extends IRVisitor {
  def src: S
  def trg: T

  protected[ir] val gensym = new Gensym()

  def loweredIRs: Set[BaseIR] = Set()

  def addedIRs: Set[BaseIR] = Set()

  def lower(module: Module): Module = gensym.scoped {
    if (!(module.lang ++ addedIRs).includes(trg.requires))
      throw new IllegalArgumentException(s"Module $module misses required features: ${trg.requires.features}")
    val loweredLang = (module.lang -- loweredIRs) ++ addedIRs
    //println(s"module lang ${module.lang}, lowered $loweredIRs, lowered lang $loweredLang")
    visit(Module(module.name, loweredLang, module.contents))
  }

  override def visit(module: Module): Module = {
    gensym.register(module.contents.map(_.name.toString))
    super.visit(module)
  }

  override def visitModuleEntry(moduleEntry: ModuleEntry): Seq[ModuleEntry] = moduleEntry match
    case r: Relation => gensym.scoped { visitRelation(r) }
    case _ => super.visitModuleEntry(moduleEntry)

  override def visitRelation(relation: Relation): Seq[Relation] =
    Seq(Relation(relation.name, relation.params.flatMap(visitParam), relation.bodies.flatMap(b => gensym.scoped { visitBody(b) })))

  override def visitParam(param: Param): Seq[Param] = {
    gensym.register(param.name)
    super.visitParam(param)
  }

  override def visitTerm(term: Term): Seq[Term] = term match
    case Var(name) =>
      gensym.register(name)
      super.visitTerm(term)
    case _ =>
      super.visitTerm(term)
}
