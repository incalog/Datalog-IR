package inca.codeql.executor

import inca.codeql.executor.CodeQlExecutor.?
import inca.ir.execution.Relation
import inca.ir.execution.interpreter.Executor
import org.scalatest.funsuite.AnyFunSuite

class CodeQlExecutorTest extends AnyFunSuite:
  private val codeQl = CodeQlExecutor(Executor())

  test("execute recursive predicates over an external EDB"):
    val compiled = codeQl.compileCodeQl(
      """
        |external predicate edge(int source, int target);
        |predicate path(int source, int target) {
        |  edge(source, target) or exists(int via | edge(source, via) and path(via, target))
        |}
        |from int value where path(1, value) select value
        |""".stripMargin
    )
    val edge = Relation.from("edge", Seq("source", "target"), Seq(Seq(1, 2), Seq(2, 3), Seq(3, 4)))
    val loaded = codeQl.loadCodeQl(compiled, Seq(edge))

    assertResult(Set(2, 3, 4))(loaded.select().entries.toSet)
    assertResult(Set((1, 2), (1, 3), (1, 4)))(loaded.query("path", Seq(1, ?)).entries.toSet)

  test("execute finite ranges and result predicates"):
    val compiled = codeQl.compileCodeQl(
      """
        |int getSuccessor(int i) {
        |  i in [1 .. 9] and
        |  result = i + 1
        |}
        |from int value where value in [1 .. 3] select getSuccessor(value)
        |""".stripMargin
    )
    val loaded = codeQl.loadCodeQl(compiled)
    assertResult(Set(2, 3, 4))(loaded.select().entries.toSet)

  test("execute disjunction and negated equality"):
    val compiled = codeQl.compileCodeQl(
      """
        |predicate chosen(int value) { value = 1 or value = 2 }
        |from int value where chosen(value) and not value = 2 select value
        |""".stripMargin
    )
    val loaded = codeQl.loadCodeQl(compiled)

    assertResult(Set(1))(loaded.select().entries.toSet)

  test("execute classes, member predicates, and dynamic overrides"):
    val compiled = codeQl.compileCodeQl(
      """
        |class Base extends int {
        |  Base() { this = [1 .. 2] }
        |  string label() { result = "base" }
        |}
        |class Child extends Base {
        |  Child() { this = 2 }
        |  override string label() { result = "child" }
        |}
        |from Base value select value, value.label()
        |""".stripMargin
    )
    val loaded = codeQl.loadCodeQl(compiled)

    assertResult(Set((1, "base"), (2, "child")))(loaded.select().entries.toSet)

  test("execute class fields and unqualified member calls"):
    // TODO: isNamed is probably a library method?
    val compiled = codeQl.compileCodeQl(
      """
        |external predicate labels(int value, string label);
        |class LabelledInt extends int {
        |  string label;
        |  LabelledInt() { labels(this, label) }
        |  string getLabel() { result = label }
        |  predicate isNamed(string expected) { getLabel() = expected }
        |}
        |from LabelledInt value
        |where value.isNamed("two")
        |select value, value.getLabel()
        |""".stripMargin
    )
    val labels = Relation.from("labels", Seq("value", "label"), Seq(Seq(1, "one"), Seq(2, "two")))
    val loaded = codeQl.loadCodeQl(compiled, Seq(labels))
    assertResult(Set((2, "two")))(loaded.select().entries.toSet)

  test("execute primitive built-in member calls"):
    val stringVariable = codeQl.loadCodeQl(codeQl.compileCodeQl(
      """
        |from string s
        |where s = "lgtm"
        |select s.length()
        |""".stripMargin
    ))
    assertResult(Set(4))(stringVariable.select().entries.toSet)

    val stringLiteral = codeQl.loadCodeQl(codeQl.compileCodeQl(
      """
        |select "lgtm".length()
        |""".stripMargin
    ))
    assertResult(Set(4))(stringLiteral.select().entries.toSet)

    val numeric = codeQl.loadCodeQl(codeQl.compileCodeQl(
      """
        |from float x, float y
        |where x = 3.pow(5) and y = 245.6
        |select x.minimum(y)
        |""".stripMargin
    ))
    assertResult(Set(243.0))(numeric.select().entries.toSet)

    val boolean = codeQl.loadCodeQl(codeQl.compileCodeQl(
      """
        |from boolean b
        |where b = false
        |select b.booleanNot()
        |""".stripMargin
    ))
    // Because of lowering we get an int
    assertResult(Set(1))(boolean.select().entries.toSet)


  test("cross the river"):
    val query = codeQl.loadCodeQl(codeQl.compileCodeQl(
      """
        |class Cargo extends string {
        |  Cargo() {
        |    this = "Nothing" or
        |    this = "Goat" or
        |    this = "Cabbage" or
        |    this = "Wolf"
        |  }
        |}
        |
        |class Shore extends string {
        |  Shore() {
        |    this = "Left" or
        |    this = "Right"
        |  }
        |
        |  Shore other() {
        |    this = "Left" and result = "Right"
        |    or
        |    this = "Right" and result = "Left"
        |  }
        |}
        |""".stripMargin
    ))
    println(query.select().entries.toSet)

