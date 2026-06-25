package inca.codeql.typecheck

import inca.codeql.syntax.Parser
import inca.ir.typing.TypeErrorException
import org.scalatest.funsuite.AnyFunSuite

class TypecheckerTest extends AnyFunSuite:
  test("typecheck a result predicate"):
    val program = Parser.parseProgram(
      """
        |int successor(int value) {
        |  value in [0 .. 2] and result = value + 1
        |}
        |from int value where value in [0 .. 2] select successor(value)
        |""".stripMargin
    )
    val typechecker = Typechecker()
    typechecker.checkProgram(program)
    typechecker.failOnError()

  test("reject a result predicate used as a formula"):
    val program = Parser.parseProgram(
      """
        |int successor(int value) { result = value + 1 }
        |predicate invalid(int value) { successor(value) }
        |select 1
        |""".stripMargin
    )
    val typechecker = Typechecker()
    typechecker.checkProgram(program)
    assertThrows[TypeErrorException](typechecker.failOnError())

  test("reject incompatible comparison types"):
    val program = Parser.parseProgram("from int value where value = \"one\" select value")
    val typechecker = Typechecker()
    typechecker.checkProgram(program)
    assertThrows[TypeErrorException](typechecker.failOnError())

  test("typecheck inherited fields and an override"):
    val program = Parser.parseProgram(
      """
        |class Base extends int {
        |  string label;
        |  Base() { this = [1 .. 2] }
        |  string getLabel() { result = label }
        |}
        |class Child extends Base {
        |  Child() { this = 2 }
        |  override string getLabel() { result = "child" }
        |}
        |from Base value select value.getLabel()
        |""".stripMargin
    )
    val typechecker = Typechecker()
    typechecker.checkProgram(program)
    typechecker.failOnError()
