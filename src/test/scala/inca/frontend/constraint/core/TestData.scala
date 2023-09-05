package inca.frontend.constraint.core

import inca.analyzedLangs.Exp
import inca.frontend.constraint.compiler.ConstraintOptions
import inca.runtime.context.QueryScope
import inca.util.matchers.IncaConstraintMatchers
import org.scalatest.flatspec.AnyFlatSpec
import scala.language.implicitConversions

class TestData extends AnyFlatSpec with IncaConstraintMatchers {
  val dataModel = Exp.model
  val scope = new QueryScope(Exp.model)
  val options = ConstraintOptions()

  implicit def name(s: String): Name = Name(s)

  "data op" should "support zero-arg data op" in {
    val code =
      """module Test
        |`import inca.analyzedData.Nat`
        |def zero(): `Nat.Nat` = {
        |  yield `Nat.Zero`
        |}
        |""".stripMargin

    val input = Exp.BooleanLit(true)

    assertMatch(code, "zero", input) { matcher =>
      assert(matcher.getAllMatches.size() == 1)
    }
  }

  "data op" should "support non-zero-arg data op" in {
    val code =
      """module Test
        |`import inca.analyzedData.Nat`
        |def one(): `Nat.Nat` = {
        |  yield `Nat.Succ`(`Nat.Zero`)
        |}
        |""".stripMargin

    val input = Exp.BooleanLit(true)

    assertMatch(code, "one", input) { matcher =>
      assert(matcher.getAllMatches.size() == 1)
    }
  }

  "data op" should "support finite enumerations" in {
    val code =
      """module Test
        |`import inca.analyzedData.Nat`
        |
        |def range(): `Nat.Nat` = {
        |  yield `Nat.Zero`
        |} union {
        |  val pred = range()
        |  if (`pred.toInt < 10`) {
        |    yield `Nat.Succ`(pred)
        |  } else {
        |    continue
        |  }
        |}
        |""".stripMargin
    val input = Exp.BooleanLit(true)

    assertMatch(code, "range", input) { matcher =>
      assert(matcher.getAllMatches.size() == 11)
    }
  }
}
