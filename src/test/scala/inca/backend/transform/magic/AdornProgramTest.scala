package inca.backend.transform.magic

import inca.backend.ir.GP
import inca.backend.transform.magic.Examples._
import inca.runtime.context.LanguageMetaInfo
import org.scalatest.funsuite.AnyFunSuite

class AdornProgramTest extends AnyFunSuite {

  def moduleEqual(m1: GP.Module, m2: GP.Module): Boolean =
    m1.pats.size == m2.pats.size && m1.pats.forall(m2.pats.contains)

  test("Adornment of flat function") {
    val adorned = AdornProgram.transformer(new LanguageMetaInfo()).transformModule(incModuleGP)
    assert(moduleEqual(adorned, adornedIncModuleGP))
  }

  test("Adornment of recursive function") {
    val adorned = AdornProgram.transformer(new LanguageMetaInfo()).transformModule(factModuleGP)
    assert(moduleEqual(adorned, adornedFactModuleGP))
  }

  test("Adornment of negative call") {
    val adorned = AdornProgram.transformer(new LanguageMetaInfo()).transformModule(unreachableModuleGP)
    assert(moduleEqual(adorned, adornedUnreachableModuleGP))
  }

//  test("Adornment with fixed adornment") {
//    val moduleGP = GenerateDatalog.transformModule(AST.plusModule)
//    val adorned = AdornProgram.transformer.transformModule(moduleGP)
//    println(adorned)
//  }
//
//  test("Adornment with fixed adornment (real plus)") {
//    val moduleGP = GenerateDatalog.transformModule(AST.plusRealModule)
//    val adorned = AdornProgram.transformer.transformModule(moduleGP)
//    println(adorned)
//  }
}
