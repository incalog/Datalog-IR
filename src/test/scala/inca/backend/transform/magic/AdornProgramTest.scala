package inca.backend.transform.magic

import inca.backend.ir.GP
import inca.backend.transform.magic.Examples._
import org.scalatest.funsuite.AnyFunSuite

class AdornProgramTest extends AnyFunSuite {

  def moduleEqual(m1: GP.Module, m2: GP.Module): Boolean =
    m1.pats.size == m2.pats.size && m1.pats.forall(m2.pats.contains)

  test("Adornment of flat function") {
    val trans = AdornProgram.transformer

    val adorned = trans.transformModule(incModuleGP)
    assert(moduleEqual(adorned, adornedIncModuleGP))
  }

  test("Adornment of recursive function") {
    val trans = AdornProgram.transformer

    val adorned = trans.transformModule(factModuleGP)
    assert(moduleEqual(adorned, adornedFactModuleGP))
  }

  test("Adornment of negative call") {
    val trans = AdornProgram.transformer

    val adorned = trans.transformModule(unreachableModuleGP)
    assert(moduleEqual(adorned, adornedUnreachableModuleGP))
  }
}
