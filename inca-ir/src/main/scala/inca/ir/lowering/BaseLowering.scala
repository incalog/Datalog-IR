package inca.ir.lowering

import inca.ir.{Atom, Body, Call, BaseIR, Module}
import inca.ir.visitors.IRVisitor

// TODO: Do we still need S and T ??
trait BaseLowering[S <: BaseIR, T <: BaseIR] extends IRVisitor {
  def src: S
  def trg: T

  def loweredIRs: Set[BaseIR] = Set()

  def addedIRs: Set[BaseIR] = Set()

  def lower(module: Module): Module = {
    if (!(module.lang ++ addedIRs).includes(trg.requires))
      throw new IllegalArgumentException(s"Module $module misses required features: ${trg.requires.features}")
    val loweredLang = (module.lang -- loweredIRs) ++ addedIRs
    //println(s"module lang ${module.lang}, lowered $loweredIRs, lowered lang $loweredLang")
    visit(Module(module.name, loweredLang, module.contents))
  }
}
