package inca.ir.lowering

import inca.ir.Hint.preserveHints
import inca.ir.{Atom, BaseIR, Body, Call, Module, ModuleEntry, Param, Relation, Term, Var, name2string}
import inca.ir.visitors.IRVisitor
import inca.util.Gensym

trait BaseLowering extends IRVisitor:
  protected val gensym = new Gensym()

  def loweredIRs: Set[BaseIR]
  def requiredIRs: Set[BaseIR]

  override def toString: String = s"Lowering ${loweredIRs.mkString(", ")}"

  def lower(ms: Seq[Module]): Module = visitProgram(ms) match
    case Seq(mod) => mod
    case mods => throw IllegalStateException(s"Expected a single output module, but got ${mods.size}")

  def lower(m: Module): Module = visitProgram(Seq(m)).head

  override def visitProgram(modules: Seq[Module], dependencies: Seq[Module] = Seq()): Seq[Module] = gensym.scoped {
    modules.map { module =>
      val loweredLang = module.lang -- loweredIRs
      val loweringNecessary = loweredIRs.exists { l => module.lang.features.contains(l) }
      if (!loweringNecessary) {
        // module does not use any features lowered here
        module
      } else {
        val resultLang = loweredLang ++ requiredIRs
        //println(s"module lang ${module.lang}, lowered $loweredIRs, lowered lang $loweredLang")
        visitModule(module).copy(lang = resultLang)
      }
    }
  }

  override def visitModule(module: Module): Module = {
    gensym.register(module.contents.map(_.name.toString))
    super.visitModule(module)
  }

  override def visitModuleEntry(moduleEntry: ModuleEntry): Seq[ModuleEntry] = moduleEntry match
    case r: Relation => gensym.scoped { visitRelation(r) }
    case _ => super.visitModuleEntry(moduleEntry)

  // Make sure to register all variables in all bodies before we process the relation
  def registerAllVars(relation: Relation): Unit =
    val allVarNames = relation.bodies.flatMap { b =>
      b.atoms.flatMap { a =>
        a.vars.map(_.name.name)
      }
    }
    val allParamNames = relation.params.map(p => p.name.name)
    gensym.register(allVarNames ++ allParamNames)

  override def visitRelation(relation: Relation): Seq[Relation] = preserveHints(relation) {
    registerAllVars(relation)

    Seq(
      Relation(
        relation.name,
        relation.params.flatMap(visitParam),
        relation.bodies.flatMap(b => gensym.scoped {
          visitBody(b)
        }).distinct // Remove exact duplicates
      )
    )
  }

