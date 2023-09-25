package inca.ir.lowering

import inca.ir.Hint.preserveHints
import inca.ir.util.Gensym
import inca.ir.{Atom, BaseIR, Body, Call, Module, ModuleEntry, Param, Relation, Term, Var, name2string}
import inca.ir.visitors.IRVisitor

trait BaseLowering extends IRVisitor:

  protected val gensym = new Gensym()

  def loweredIRs: Set[BaseIR]
  def requiredIRs: Set[BaseIR]

  def lower(module: Module): Module = gensym.scoped {
    val loweredLang = module.lang -- loweredIRs
    if (loweredLang.features.size == module.lang.features.size) {
      // module does not use any features lowered here
      module
    } else {
      val resultLang = loweredLang ++ requiredIRs
      //println(s"module lang ${module.lang}, lowered $loweredIRs, lowered lang $loweredLang")
      visit(module).copy(lang = resultLang)
    }
  }

  override def visit(module: Module): Module = {
    gensym.register(module.contents.map(_.name.toString))
    super.visit(module)
  }

  override def visitModuleEntry(moduleEntry: ModuleEntry): Seq[ModuleEntry] = moduleEntry match
    case r: Relation => gensym.scoped { visitRelation(r) }
    case _ => super.visitModuleEntry(moduleEntry)

  override def visitRelation(relation: Relation): Seq[Relation] = preserveHints(relation) {
    Seq(
      Relation(
        relation.name,
        relation.params.flatMap(visitParam),
        relation.bodies.flatMap(b => gensym.scoped {
          visitBody(b)
        })
      )
    )
  }

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

