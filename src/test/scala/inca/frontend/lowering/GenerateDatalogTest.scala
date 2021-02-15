package inca.frontend.lowering

import inca.backend.ir.GP
import inca.frontend.core._
import inca.util.Meta.Scala
import org.scalatest.funsuite.AnyFunSuite

import scala.meta.XtensionQuasiquoteTerm

class GenerateDatalogTest extends AnyFunSuite {

  def module(content: ModuleContent*): Module =
    Module(Name("Main"), Seq(), content)
  def gpmodule(content: GP.Pattern*): GP.Module =
    GP.Module("Main", Seq(), content, Seq())

  val baseExample: Module = module(FunctionDef(None, Name("main"), Seq(), TScalaInt,
    Eval(Scala(q"7 + (12 * 3)")).typed(TScalaInt)
  ))
  val baseExampleGP: GP.Module = gpmodule(GP.Pattern(None, "main", Seq(GP.Param("out", GP.TScalaInt)), Seq(GP.Body(Seq(
    GP.Computed(GP.Var("eval"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 7 + (12 * 3)"))),
    GP.Eq(GP.Var("out"), GP.Var("eval"))
  )))))

  val varExample: Module = module(FunctionDef(None, Name("main"), Seq(), TScalaInt,
    Let(Seq(Name("x")), None, Eval(Scala(q"7")).typed(TScalaInt),
      Let(Seq(Name("y")), None, Eval(Scala(q"3")).typed(TScalaInt),
        Eval(Seq(EvalParam(Name("x")).typed(TScalaInt), EvalParam(Name("y")).typed(TScalaInt)),
          Scala(q"x + (12 * y)")).typed(TScalaInt)
      )
    )
  ))
  val varExampleGP: GP.Module = gpmodule(GP.Pattern(None, "main", Seq(GP.Param("out", GP.TScalaInt)), Seq(GP.Body(Seq(
    GP.Computed(GP.Var("eval"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 7"))),
    GP.Eq(GP.Var("x"), GP.Var("eval")),
    GP.Computed(GP.Var("eval_0"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 3"))),
    GP.Eq(GP.Var("y"), GP.Var("eval_0")),
    GP.Computed(GP.Var("eval_1"), GP.Evaluation(Seq(GP.Var("x") -> GP.TScalaInt, GP.Var("y") -> GP.TScalaInt), GP.TScalaInt,
      Scala(q"(x: Int, y: Int) => x + (12 * y)"))),
    GP.Eq(GP.Var("out"), GP.Var("eval_1"))
  )))))

  val ifExample: Module = module(FunctionDef(None, Name("main"), Seq(), TScalaInt,
    Let(Seq(Name("x")), None, Eval(Scala(q"7")).typed(TScalaInt),
      If(Eval(Seq(EvalParam(Name("x")).typed(TScalaInt)), Scala(q"x > 0")).typed(TScalaBoolean),
        Var("x"),
        Eval(Seq(EvalParam(Name("x")).typed(TScalaInt)), Scala(q"x * -1")).typed(TScalaInt)
      )
    )
  ))
  val ifExampleGP: GP.Module = gpmodule(GP.Pattern(None, "main", Seq(GP.Param("out", GP.TScalaInt)), Seq(GP.Body(Seq(
    GP.Computed(GP.Var("eval"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 7"))),
    GP.Eq(GP.Var("x"), GP.Var("eval")),
    GP.Computed(GP.Var("eval_0"), GP.Evaluation(Seq(GP.Var("x") -> GP.TScalaInt), GP.TScalaBoolean, Scala(q"(x: Int) => x > 0"))),
    GP.Eq(GP.Var("eval_0"), GP.True),
    GP.Eq(GP.Var("out"), GP.Var("x"))
  )), GP.Body(Seq(
    GP.Computed(GP.Var("eval"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 7"))),
    GP.Eq(GP.Var("x"), GP.Var("eval")),
    GP.Computed(GP.Var("eval_0"), GP.Evaluation(Seq(GP.Var("x") -> GP.TScalaInt), GP.TScalaBoolean, Scala(q"(x: Int) => x > 0"))),
    GP.Eq(GP.Var("eval_0"), GP.False),
    GP.Computed(GP.Var("eval_1"), GP.Evaluation(Seq(GP.Var("x") -> GP.TScalaInt), GP.TScalaInt, Scala(q"(x: Int) => x * -1"))),
    GP.Eq(GP.Var("out"), GP.Var("eval_1"))
  )))))

  val ifExample2: Module = module(FunctionDef(None, Name("main"), Seq(), TScalaInt,
    Let(Seq(Name("x")), None, Eval(Scala(q"7")).typed(TScalaInt),
      Let(Seq(Name("y")), None, Eval(Scala(q"-3")).typed(TScalaInt),
        Let(Seq(Name("xpos")), None,
          If(Eval(Seq(EvalParam(Name("x")).typed(TScalaInt)), Scala(q"x > 0")).typed(TScalaBoolean),
            Var("x"),
            Eval(Seq(EvalParam(Name("x")).typed(TScalaInt)), Scala(q"x * -1")).typed(TScalaInt)
          ),
          Let(Seq(Name("ypos")), None,
            If(Eval(Seq(EvalParam(Name("y")).typed(TScalaInt)), Scala(q"y > 0")).typed(TScalaBoolean),
              Var("y"),
              Eval(Seq(EvalParam(Name("y")).typed(TScalaInt)), Scala(q"y * -1")).typed(TScalaInt)
            ),
            Eval(Seq(EvalParam(Name("xpos")).typed(TScalaInt),EvalParam(Name("ypos")).typed(TScalaInt)),
              Scala(q"xpos + ypos")).typed(TScalaInt)
          )
        )
      )
    )
  ))
  val ifExample2GP: GP.Module = gpmodule(GP.Pattern(None, "main", Seq(GP.Param("out", GP.TScalaInt)), Seq(GP.Body(Seq(
    GP.Computed(GP.Var("eval"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 7"))),
    GP.Eq(GP.Var("x"), GP.Var("eval")),
    GP.Computed(GP.Var("eval_0"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => -3"))),
    GP.Eq(GP.Var("y"), GP.Var("eval_0")),
    GP.Computed(GP.Var("eval_1"), GP.Evaluation(Seq(GP.Var("x") -> GP.TScalaInt), GP.TScalaBoolean, Scala(q"(x: Int) => x > 0"))),
    GP.Eq(GP.Var("eval_1"), GP.True),
    GP.Eq(GP.Var("xpos"), GP.Var("x")),
    GP.Computed(GP.Var("eval_3"), GP.Evaluation(Seq(GP.Var("y") -> GP.TScalaInt), GP.TScalaBoolean, Scala(q"(y: Int) => y > 0"))),
    GP.Eq(GP.Var("eval_3"), GP.True),
    GP.Eq(GP.Var("ypos"), GP.Var("y")),
    GP.Computed(GP.Var("eval_5"), GP.Evaluation(Seq(GP.Var("xpos") -> GP.TScalaInt, GP.Var("ypos") -> GP.TScalaInt), GP.TScalaInt,
      Scala(q"(xpos: Int, ypos: Int) => xpos + ypos"))),
    GP.Eq(GP.Var("out"), GP.Var("eval_5"))
  )), GP.Body(Seq(
    GP.Computed(GP.Var("eval"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 7"))),
    GP.Eq(GP.Var("x"), GP.Var("eval")),
    GP.Computed(GP.Var("eval_0"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => -3"))),
    GP.Eq(GP.Var("y"), GP.Var("eval_0")),
    GP.Computed(GP.Var("eval_1"), GP.Evaluation(Seq(GP.Var("x") -> GP.TScalaInt), GP.TScalaBoolean, Scala(q"(x: Int) => x > 0"))),
    GP.Eq(GP.Var("eval_1"), GP.True),
    GP.Eq(GP.Var("xpos"), GP.Var("x")),
    GP.Computed(GP.Var("eval_3"), GP.Evaluation(Seq(GP.Var("y") -> GP.TScalaInt), GP.TScalaBoolean, Scala(q"(y: Int) => y > 0"))),
    GP.Eq(GP.Var("eval_3"), GP.False),
    GP.Computed(GP.Var("eval_4"), GP.Evaluation(Seq(GP.Var("y") -> GP.TScalaInt), GP.TScalaInt, Scala(q"(y: Int) => y * -1"))),
    GP.Eq(GP.Var("ypos"), GP.Var("eval_4")),
    GP.Computed(GP.Var("eval_6"), GP.Evaluation(Seq(GP.Var("xpos") -> GP.TScalaInt, GP.Var("ypos") -> GP.TScalaInt), GP.TScalaInt,
      Scala(q"(xpos: Int, ypos: Int) => xpos + ypos"))),
    GP.Eq(GP.Var("out"), GP.Var("eval_6"))
  )), GP.Body(Seq(
    GP.Computed(GP.Var("eval"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 7"))),
    GP.Eq(GP.Var("x"), GP.Var("eval")),
    GP.Computed(GP.Var("eval_0"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => -3"))),
    GP.Eq(GP.Var("y"), GP.Var("eval_0")),
    GP.Computed(GP.Var("eval_1"), GP.Evaluation(Seq(GP.Var("x") -> GP.TScalaInt), GP.TScalaBoolean, Scala(q"(x: Int) => x > 0"))),
    GP.Eq(GP.Var("eval_1"), GP.False),
    GP.Computed(GP.Var("eval_2"), GP.Evaluation(Seq(GP.Var("x") -> GP.TScalaInt), GP.TScalaInt, Scala(q"(x: Int) => x * -1"))),
    GP.Eq(GP.Var("xpos"), GP.Var("eval_2")),
    GP.Computed(GP.Var("eval_7"), GP.Evaluation(Seq(GP.Var("y") -> GP.TScalaInt), GP.TScalaBoolean, Scala(q"(y: Int) => y > 0"))),
    GP.Eq(GP.Var("eval_7"), GP.True),
    GP.Eq(GP.Var("ypos"), GP.Var("y")),
    GP.Computed(GP.Var("eval_9"), GP.Evaluation(Seq(GP.Var("xpos") -> GP.TScalaInt, GP.Var("ypos") -> GP.TScalaInt), GP.TScalaInt,
      Scala(q"(xpos: Int, ypos: Int) => xpos + ypos"))),
    GP.Eq(GP.Var("out"), GP.Var("eval_9"))
  )), GP.Body(Seq(
    GP.Computed(GP.Var("eval"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 7"))),
    GP.Eq(GP.Var("x"), GP.Var("eval")),
    GP.Computed(GP.Var("eval_0"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => -3"))),
    GP.Eq(GP.Var("y"), GP.Var("eval_0")),
    GP.Computed(GP.Var("eval_1"), GP.Evaluation(Seq(GP.Var("x") -> GP.TScalaInt), GP.TScalaBoolean, Scala(q"(x: Int) => x > 0"))),
    GP.Eq(GP.Var("eval_1"), GP.False),
    GP.Computed(GP.Var("eval_2"), GP.Evaluation(Seq(GP.Var("x") -> GP.TScalaInt), GP.TScalaInt, Scala(q"(x: Int) => x * -1"))),
    GP.Eq(GP.Var("xpos"), GP.Var("eval_2")),
    GP.Computed(GP.Var("eval_7"), GP.Evaluation(Seq(GP.Var("y") -> GP.TScalaInt), GP.TScalaBoolean, Scala(q"(y: Int) => y > 0"))),
    GP.Eq(GP.Var("eval_7"), GP.False),
    GP.Computed(GP.Var("eval_8"), GP.Evaluation(Seq(GP.Var("y") -> GP.TScalaInt), GP.TScalaInt, Scala(q"(y: Int) => y * -1"))),
    GP.Eq(GP.Var("ypos"), GP.Var("eval_8")),
    GP.Computed(GP.Var("eval_10"), GP.Evaluation(Seq(GP.Var("xpos") -> GP.TScalaInt, GP.Var("ypos") -> GP.TScalaInt), GP.TScalaInt,
      Scala(q"(xpos: Int, ypos: Int) => xpos + ypos"))),
    GP.Eq(GP.Var("out"), GP.Var("eval_10"))
  )))))


  val incFun = FunctionDef(None, Name("inc"), Seq(Param(Name("n"), TScalaInt)), TScalaInt,
    Eval(Seq(EvalParam(Name("n")).typed(TScalaInt)), Scala(q"n + 1")).typed(TScalaInt)
  )
  val incMain = FunctionDef(None, Name("main"), Seq(), TScalaInt,
    Call(Name("inc"), Seq(Eval(Scala(q"0")).typed(TScalaInt))).resolved(incFun)
  )
  val incModule = module(incFun, incMain)
  val incFunGP = GP.Pattern(None, "inc", Seq(GP.Param("n", GP.TScalaInt), GP.Param("out", GP.TScalaInt)), Seq(GP.Body(Seq(
    GP.Computed(GP.Var("eval"), GP.Evaluation(Seq(GP.Var("n") -> GP.TScalaInt), GP.TScalaInt, Scala(q"(n: Int) => n + 1"))),
    GP.Eq(GP.Var("out"), GP.Var("eval"))
  ))))
  val incMainGP = GP.Pattern(None, "main", Seq(GP.Param("out", GP.TScalaInt)), Seq(GP.Body(Seq(
    GP.Computed(GP.Var("eval"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 0"))),
    GP.Call("inc", Seq(GP.Var("eval"), GP.Var("out_0")), transitive = false, neg = false),
    GP.Eq(GP.Var("out"), GP.Var("out_0"))
  ))))
  val incModuleGP = gpmodule(incFunGP, incMainGP)


  test("base example") {
    val result = GenerateDatalog.transformModule(baseExample)
    assert(result == baseExampleGP)
  }

  test("var example") {
    val result = GenerateDatalog.transformModule(varExample)
    assert(result == varExampleGP)
  }

  test("if example") {
    val result = GenerateDatalog.transformModule(ifExample)
    assert(result == ifExampleGP)
  }

  test("if example 2") {
    val result = GenerateDatalog.transformModule(ifExample2)
    assert(result == ifExample2GP)
  }

  test("inc example") {
    val result = GenerateDatalog.transformModule(incModule)
    assert(result == incModuleGP)
  }
}
