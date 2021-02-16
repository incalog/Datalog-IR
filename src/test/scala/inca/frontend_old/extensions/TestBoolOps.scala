package inca.frontend_old.extensions

import inca.IncaMatchers
import inca.analyzedLangs.Exp
import inca.compiler.Options
import inca.frontend_old.core
import inca.frontend_old.core.tree._
import inca.frontend_old.extensions.boolOps.Trees._
import inca.runtime.context.{LanguageMetaInfo, QueryScope}
import inca.util.Meta.Scala
import org.scalatest.flatspec.AnyFlatSpec

import scala.language.implicitConversions
import scala.meta.XtensionQuasiquoteTerm

class TestBoolOps extends AnyFlatSpec with IncaMatchers {

  implicit def name(s: String): Name = Name(s)

  val one = Constant(IntLiteral(1))
  val two = Constant(IntLiteral(2))

  val scope: QueryScope = new QueryScope(Exp.languageMetaInfo)
  val options: Options = Options(scope.langMetaInfo, info => new core.Frontend with boolOps.Frontend {
    override val lang: LanguageMetaInfo = info
  })

  "desugaring" should "eliminate not conditions" in {
    val sugared = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), TUnit, Seq(Body(Seq(
        Assert(Not(Eq(one, two))),
        Assert(Not(Neq(one, two))),
        Assert(Not(InstanceOf(one, TNode("Num")))),
        Assert(Not(NotInstanceOf(one, TNode("Num")))),
        Assert(Not(Def(Call("foo", Seq())))),
        Assert(Not(Undef(Call("foo", Seq())))),
        Assert(Not(Constant(BooleanLiteral(true)))),
        Assert(Not(Constant(BooleanLiteral(false))))
      ))))
    ))

    val core = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), TUnit, Seq(Body(Seq(
        Assert(Neq(one, two)),
        Assert(Eq(one, two)),
        Assert(NotInstanceOf(one, TNode("Num"))),
        Assert(InstanceOf(one, TNode("Num"))),
        Assert(Undef(Call("foo", Seq()))),
        Assert(Def(Call("foo", Seq()))),
        Assert(Constant(BooleanLiteral(false))),
        Assert(Constant(BooleanLiteral(true)))
      ))))
    ))

    assertDesugar(core, sugared)
  }

  "desugaring" should "eliminate nested not conditions" in {
    val sugared = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), TUnit, Seq(Body(Seq(
        Assert(Not(Not(Eq(one, two)))),
        Assert(Not(Not(Not(Eq(one, two))))),
        Assert(Not(Not(Not(Not(Eq(one, two)))))),
        Assert(Not(Not(Not(Not(Not(Eq(one, two)))))))
      ))))
    ))

    val core = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), TUnit, Seq(Body(Seq(
        Assert(Eq(one, two)),
        Assert(Neq(one, two)),
        Assert(Eq(one, two)),
        Assert(Neq(one, two))
      ))))
    ))

    assertDesugar(core, sugared)
  }

  "desugaring" should "negate eval code" in {
    val module = Module("Test_Cast", Seq(), Seq(
      PatternFunction(None, "integerlits", Seq(), TNode(Exp.expTag), Seq(Body(Seq(
        Values("e", TNode(Exp.intTag)),
        Assign(Seq("i"), PathAccess(Var("e"), NamedLink("value"))),
        Assign(Seq("cond"),
          // "i" is _not_ a square number
          Not(Eval(Seq(EvalParam("i")), Scala(q"Math.sqrt(i.doubleValue()).isValidInt")))),
        Assert(Eq(Var("cond"), Constant(BooleanLiteral(true)))),
        Yield(Var("e"))
      ))))
    ))

    val input = {
      import Exp._
      Add(
        Mul(
          IntegerLit(1),
          IntegerLit(2)
        ),
        Many(
          List(
            IntegerLit(3),
            IntegerLit(4),
            IntegerLit(5)
          )
        )
      )
    }

    assertMatchFunModule(module, "integerlits", input) { matcher =>
      assert(matcher.getAllMatches.size() == 3)
    }
  }

  "desugaring" should "implement `and` semantics" in {
    val module = Module("Test_Cast", Seq(), Seq(
      PatternFunction(None, "add_mul", Seq(), TNode(Exp.expTag), Seq(Body(Seq(
        Values("e", TNode(Exp.expTag)),
        Assert(And(
          InstanceOf(Var("e"), TNode(Exp.addTag)),
          InstanceOf(PathAccess(Cast(Var("e"), TNode(Exp.addTag)), NamedLink("lhs")), TNode(Exp.multTag))
        )),
        Yield(Var("e"))
      ))))
    ))

    val input = {
      import Exp._
      Add(
        Mul(
          IntegerLit(1),
          IntegerLit(2)
        ),
        Many(
          List(
            IntegerLit(3),
            IntegerLit(4),
            IntegerLit(5)
          )
        )
      )
    }

    assertMatchFunModule(module, "add_mul", input) { matcher =>
      assert(matcher.getAllMatches.size() == 1)
    }
  }

  "desugaring" should "implement `not and` semantics" in {
    val module = Module("Test_Cast", Seq(), Seq(
      PatternFunction(None, "integerLits", Seq(), TNode(Exp.expTag), Seq(Body(Seq(
        Values("e", TNode(Exp.expTag)),
        Assert(Not(And(
          InstanceOf(Var("e"), TNode(Exp.addTag)),
          InstanceOf(Var("e"), TNode(Exp.multTag))
        ))),
        Yield(Var("e"))
      ))))
    ))

    val input = {
      import Exp._
      Add(
        Mul(
          IntegerLit(1),
          IntegerLit(2)
        ),
        Many(
          List(
            IntegerLit(3),
            IntegerLit(4),
            IntegerLit(5)
          )
        )
      )
    }

    assertMatchFunModule(module, "integerLits", input) { matcher =>
      assert(matcher.getAllMatches.size() == 8)
    }
  }

  "desugaring" should "implement `not or` semantics" in {
    val module = Module("Test_Cast", Seq(), Seq(
      PatternFunction(None, "integerLits", Seq(), TNode(Exp.expTag), Seq(Body(Seq(
        Values("e", TNode(Exp.expTag)),
        Assert(Not(Or(
          InstanceOf(Var("e"), TNode(Exp.addTag)),
          InstanceOf(Var("e"), TNode(Exp.multTag))
        ))),
        Yield(Var("e"))
      ))))
    ))

    val input = {
      import Exp._
      Add(
        Mul(
          IntegerLit(1),
          IntegerLit(2)
        ),
        Many(
          List(
            IntegerLit(3),
            IntegerLit(4),
            IntegerLit(5)
          )
        )
      )
    }

    assertMatchFunModule(module, "integerLits", input) { matcher =>
      assert(matcher.getAllMatches.size() == 6)
    }
  }
}
