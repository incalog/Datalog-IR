package inca.frontend.extensions

import inca.analyzedLangs.Exp
import inca.frontend.core.Core._
import inca.runtime.context.QueryScope
import inca.{CompilerOptions, IncaMatchers}
import org.scalatest.flatspec.AnyFlatSpec
import scala.meta.quasiquotes._

class TestStringOps extends AnyFlatSpec with IncaMatchers {

  val one = Constant(IntLiteral(1))
  val two = Constant(IntLiteral(2))

  val scope: QueryScope = new QueryScope(Exp.languageMetaInfo)
  val options: CompilerOptions = CompilerOptions(scope.langMetaInfo, desugarables = Seq(StringOps))

  "desugaring" should "eliminate concat" in {
    val sugared = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), Seq(), Seq(Body(Seq(
        Assign(Seq("con"), Concat(Constant(StringLiteral("s1")), Constant(StringLiteral("s2")))),
        Assign(Seq("con2"), Concat(Concat(Constant(StringLiteral("s1")), Constant(StringLiteral("s2"))), Var("param")))
      ))))
    ))

    val core = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), Seq(), Seq(Body(Seq(
        Assign(Seq("lhs"), Constant(StringLiteral("s1"))),
        Assign(Seq("rhs"), Constant(StringLiteral("s2"))),
        Assign(Seq("res"), Eval(Seq("lhs", "rhs"), q"lhs ++ rhs")),
        Assign(Seq("con"), Var("res")),
        Assign(Seq("lhs_1"), Constant(StringLiteral("s1"))),
        Assign(Seq("rhs_1"), Constant(StringLiteral("s2"))),
        Assign(Seq("res_1"), Eval(Seq("lhs_1", "rhs_1"), q"lhs_1 ++ rhs_1")),
        Assign(Seq("lhs_0"), Var("res_1")),
        Assign(Seq("rhs_0"), Var("param")),
        Assign(Seq("res_0"), Eval(Seq("lhs_0", "rhs_0"), q"lhs_0 ++ rhs_0")),
        Assign(Seq("con2"), Var("res_0"))
      ))))
    ))
    assertDesugar(core, sugared)
  }
}
