package inca.frontend_old.core

import inca.IncaMatchers
import inca.analyzedLangs.Exp
import inca.compiler.Options
import inca.frontend_old.core.tree._
import inca.runtime.context.QueryScope
import org.scalatest.flatspec.AnyFlatSpec

import scala.language.implicitConversions

class TestData extends AnyFlatSpec with IncaMatchers {
  val scope = new QueryScope(Exp.languageMetaInfo)
  val options = Options(scope.langMetaInfo)

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

    assertMatchFunCode(code, "zero", input, scope) { matcher =>
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

    assertMatchFunCode(code, "one", input, scope) { matcher =>
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

    assertMatchFunCode(code, "range", input, scope) { matcher =>
      assert(matcher.getAllMatches.size() == 11)
    }
  }
}
