package inca.codeql.compile

import inca.ir.{ExtensionalRelation, Relation}
import org.scalatest.funsuite.AnyFunSuite

class GenerateIRTest extends AnyFunSuite:
  test("lower external, recursive, range, and select predicates"):
    val compiled = CompiledCodeQlUnit.fromSource(
      """
        |external predicate edge(int source, int target);
        |predicate path(int source, int target) {
        |  edge(source, target) or exists(int via | edge(source, via) and path(via, target))
        |}
        |int successor(int value) {
        |  value in [1 .. 3] and result = value + 1
        |}
        |from int value where path(1, value) select successor(value)
        |""".stripMargin
    )

    val entries = compiled.irModules.head.contents
    assert(entries.exists { case relation: ExtensionalRelation => relation.name.name == "edge"; case _ => false })
    val path = entries.collectFirst { case relation: Relation if relation.name.name == "path" => relation }.get
    val successor = entries.collectFirst { case relation: Relation if relation.name.name == "successor" => relation }.get
    assertResult(2)(path.bodies.size)
    assertResult(3)(successor.bodies.size)
    assert(entries.exists { case relation: Relation => relation.name == CompiledCodeQlUnit.SelectRelationName; case _ => false })

  test("lower class membership, members, and override dispatch"):
    val compiled = CompiledCodeQlUnit.fromSource(
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

    val names = compiled.irModules.head.contents.map(_.name.name).toSet
    assert(names.contains("__codeql_class_Base"))
    assert(names.contains("__codeql_class_Child"))
    assert(names.contains("__codeql_member_impl_Base_label"))
    assert(names.contains("__codeql_member_Base_label"))
