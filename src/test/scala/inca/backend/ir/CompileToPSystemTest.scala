package inca.backend.ir

import inca.IncaMatchers
import inca.analyzedLangs.Exp
import inca.analyzedLangs.Exp._
import inca.compiler.Options
import inca.runtime.context.QueryScope
import inca.util.Meta.Scala
import org.scalatest.funsuite.AnyFunSuite

import scala.meta._

class CompileToPSystemTest extends AnyFunSuite with IncaMatchers {

  private val testInput = Add(And(Or(BooleanLit(false), BooleanLit(false)), IntegerLit(5)), LongLit(10L))
  private val testInputNumericAddition = Add(Add(IntegerLit(5), IntegerLit(7)), Add(LongLit(7), IntegerLit(8)))

  val scope: QueryScope = new QueryScope(Exp.languageMetaInfo)
  val options: Options = Options(scope.langMetaInfo)

  test("unbounded literal parameter determined by eval") {
    val module = GP.Module("test_eval", Seq(), Seq(),
      Seq(GP.Pattern(None, "intToString", Seq(GP.Param("exp", GP.TNode(Exp.intTag)), GP.Param("str", GP.TScalaString)),
        Seq(GP.Body(Seq(
          GP.Path(GP.Var("exp"), GP.TNode(Exp.intTag), GP.NamedLink(GP.TNode(Exp.intTag), "value"), GP.Var("value"), GP.TLiteral.Int),
          GP.Computed(GP.Var("str"), GP.Evaluation(Seq((GP.Var("value"), GP.TLiteral.Int)), GP.TScalaString, Scala(q"(value: Int) => value.toString"))))
        )))), Seq())
    assertMatchGPProg(module, "intToString", testInputNumericAddition) { matcher =>
      assert(matcher.getAllMatches.size == 3)
    }
  }

  test("unbounded argument of second eval") {
    val module = GP.Module("test_eval", Seq(), Seq(),
      Seq(GP.Pattern(None, "intToString", Seq(GP.Param("exp", GP.TNode(Exp.intTag)), GP.Param("str2", GP.TScalaString)),
        Seq(GP.Body(Seq(
          GP.Path(GP.Var("exp"), GP.TNode(Exp.intTag), GP.NamedLink(GP.TNode(Exp.intTag), "value"), GP.Var("value"), GP.TLiteral.Int),
          GP.Computed(GP.Var("str"), GP.Evaluation(Seq((GP.Var("value"), GP.TLiteral.Int)), GP.TScalaString, Scala(q"(value: Int) => value.toString"))),
          GP.Computed(GP.Var("str2"), GP.Evaluation(Seq((GP.Var("str"), GP.TScalaString)), GP.TScalaString, Scala(q"""(str: String) => str + "_appended" """))))
        )))), Seq())
    assertMatchGPProg(module, "intToString", testInputNumericAddition) { matcher =>
      println(matcher.getAllMatches)
      assert(matcher.getAllMatches.size == 3)
    }
  }
}
