package inca.codeql.syntax

import org.scalatest.funsuite.AnyFunSuite

class ParserTest extends AnyFunSuite:
  test("parse predicates, result predicates, and a select query"):
    val program = Parser.parseProgram(
      """
        |external predicate edge(int source, int target);
        |
        |int successor(int value) {
        |  value in [0 .. 2] and result = value + 1
        |}
        |
        |predicate path(int source, int target) {
        |  edge(source, target) or exists(int via | edge(source, via) and path(via, target))
        |}
        |
        |from int value
        |where path(1, value) and not value = 4
        |select value as reachable, successor(value) as next
        |""".stripMargin
    )

    assertResult(3)(program.predicates.size)
    assert(program.predicates.head.external)
    assertResult(Some(QlType.IntType))(program.predicates(1).resultType)
    assertResult(2)(program.query.get.columns.size)

  test("reject a body on an external predicate"):
    assertThrows[IllegalArgumentException] {
      Parser.parseProgram("external predicate edge(int x) { x = 1 }")
    }

  test("parse a class with fields, inheritance, and member predicates"):
    val program = Parser.parseProgram(
      """
        |class LabelledInt extends int {
        |  string label;
        |  LabelledInt() { this = [1 .. 3] }
        |  string getLabel() { result = label }
        |  predicate isEven() { this = 2 }
        |}
        |from LabelledInt value
        |where value.isEven()
        |select value, value.getLabel()
        |""".stripMargin
    )

    val clazz = program.classes.head
    assertResult("LabelledInt")(clazz.name.name)
    assertResult(1)(clazz.fields.size)
    assertResult(2)(clazz.members.size)
    assert(clazz.characteristic.nonEmpty)
