package inca.frontend.typecheck

import inca.examples.ADT.NAT_lmi
import inca.examples.AST
import inca.frontend.core.Module
import inca.frontend.typechecker.Typechecker
import inca.runtime.context.LanguageMetaInfo
import org.scalatest.funsuite.AnyFunSuite

class TypecheckerTest extends AnyFunSuite {

  def newTypechecker(): Typechecker = new Typechecker {
    override val lang: LanguageMetaInfo = NAT_lmi
  }

  def checkModule(mod: Module): Unit = {
    val checker = newTypechecker()
    checker.typecheck(mod)
    assert(checker.getErrors.isEmpty, s"Found type errors ${checker.getErrors}")
  }

  test("base example") {
    checkModule(AST.baseExample)
  }

  test("base example 2") {
    checkModule(AST.baseExample2)
  }

  test("var example") {
    checkModule(AST.varExample)
  }

  test("if example") {
    checkModule(AST.ifExample)
  }

  test("if example 2") {
    checkModule(AST.ifExample2)
  }

  test("inc example") {
    checkModule(AST.incModule)
  }

  test("fact example") {
    checkModule(AST.factModule)
  }

  test("plus example") {
    checkModule(AST.plusModule)
  }

  test("plus real example") {
    checkModule(AST.plusRealModule)
  }

}
