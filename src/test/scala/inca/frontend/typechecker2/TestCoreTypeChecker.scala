package inca.frontend.typechecker2

import fastparse.parse
import inca.analyzedLangs
import inca.frontend.core.Core._
import inca.frontend.parser.CoreParser
import org.scalatest.flatspec.AnyFlatSpec

class TestCoreTypeChecker extends AnyFlatSpec{

  def parseExp(str: String): Exp = {
    val parser = new CoreParser()
    // exp are always syntactically correct
    parse(str, parser.exp(_)).get.value
  }

  def parseStatement(str: String): Statement = {
    val parser = new CoreParser()
    // statements are always syntactically correct
    parse(str, parser.statement(_)).get.value
  }

  def parseBody(str: String): Body = {
    val parser = new CoreParser()
    // bodies are always syntactically correct
    parse(str, parser.body(_)).get.value
  }

  def parsePatternFunction(str: String): PatternFunction = {
    val parser = new CoreParser()
    // bodies are always syntactically correct
    parse(str, parser.patternFunction(_)).get.value
  }

  def parseModule(str: String): Module = {
    val parser = new CoreParser()
    // bodies are always syntactically correct
    parse(str, parser.module(_)).get.value
  }

  val typer = new TypeChecker(Nil)(analyzedLangs.Exp.languageMetaInfo)

  "checkExp" should "type var correctly" in {
    val varExp = parseExp("x")

    assertResult(TBool)(typer.checkExp(TypeContext(Map("x" -> TBool), Map(), null))(varExp))
    assertThrows[TypeError](typer.checkExp(TypeContext(Map(), Map(), null))(varExp))
  }

  "checkExp" should "type constant literals correctly" in {
    val boolConst = parseExp("true")
    val intConst = parseExp("12")
    val longConst = parseExp("12L")
    val doubleConst = parseExp("12.0")
    val stringConst = parseExp("\"str\"")

    val ctx = TypeContext(Map(), Map(), null)
    assertResult(TBool)(typer.checkExp(ctx)(boolConst))
    assertResult(TInt)(typer.checkExp(ctx)(intConst))
    assertResult(TLong)(typer.checkExp(ctx)(longConst))
    assertResult(TDouble)(typer.checkExp(ctx)(doubleConst))
    assertResult(TString)(typer.checkExp(ctx)(stringConst))
  }

  "checkExp" should "type wildcard correctly" in {
    val wildcard = parseExp("_")

    val ctx = TypeContext(Map(), Map(), null)
    assertResult(typer.checkExp(ctx)(wildcard))(TAny)
  }

  "checkExp" should "type path access named link correctly" in {
    val pathAccess = parseExp("add.lhs")
    val ctx = TypeContext(Map("add" -> TNode(analyzedLangs.Exp.addTag)), Map(), null)
    assertResult(TNode(analyzedLangs.Exp.expTag))(typer.checkExp(ctx)(pathAccess))

    val emptyCtx = TypeContext(Map(), Map(), null)
    assertThrows[TypeError](typer.checkExp(emptyCtx)(pathAccess))

    val invalidPathAccess = parseExp("add.vl")
    assertThrows[TypeError](typer.checkExp(ctx)(invalidPathAccess))

    val listPathAccess = parseExp("many.exps")
    val listCtx = TypeContext(Map("many" -> TNode(analyzedLangs.Exp.manyTag)), Map(), null)
    assertResult(TList(TNode(analyzedLangs.Exp.expTag)))(typer.checkExp(listCtx)(listPathAccess))
    val listSize = parseExp("many.exps.size")
    assertResult(TInt)(typer.checkExp(listCtx)(listSize))

    val listChilds = parseExp("many.exps.children")
    assertResult(TNode(analyzedLangs.Exp.expTag))(typer.checkExp(listCtx)(listChilds))
  }

  "checkExp" should "type path access parent link correctly" in {
    val parent = parseExp("many.parent")
    val ctx = TypeContext(Map("many" -> TNode(analyzedLangs.Exp.manyTag)), Map(), null)
    assertResult(TAny)(typer.checkExp(ctx)(parent))
  }

  "checkExp" should "type path access size link correctly" in {
    val listSize = parseExp("many.exps.size")
    val ctx = TypeContext(Map("many" -> TNode(analyzedLangs.Exp.manyTag)), Map(), null)
    assertResult(TInt)(typer.checkExp(ctx)(listSize))
  }

  "checkExp" should "type path access children link correctly" in {
    val listChilds = parseExp("many.exps.children")
    val ctx = TypeContext(Map("many" -> TNode(analyzedLangs.Exp.manyTag)), Map(), null)
    assertResult(TNode(analyzedLangs.Exp.expTag))(typer.checkExp(ctx)(listChilds))
  }

  "checkExp" should "type path access next link correctly" in {
    val listChildsNext = parseExp("many.exps.children.next")
    val ctx = TypeContext(Map("many" -> TNode(analyzedLangs.Exp.manyTag)), Map(), null)
    assertResult(TNode(analyzedLangs.Exp.expTag))(typer.checkExp(ctx)(listChildsNext))
  }

  "checkExp" should "type path access previous link correctly" in {
    val ctx = TypeContext(Map("many" -> TNode(analyzedLangs.Exp.manyTag)), Map(), null)
    val listChildsPrev = parseExp("many.exps.children.previous")
    assertResult(TNode(analyzedLangs.Exp.expTag))(typer.checkExp(ctx)(listChildsPrev))
  }

  "checkExp" should "type tuple correctly" in {
    val ctx = TypeContext(Map("x" -> TString), Map(), null)

    val tuple = parseExp("(1, x)")
    assertResult(TTuple(Seq(TInt, TString)))(typer.checkExp(ctx)(tuple))

    val tuple2 = parseExp("(1, x, 2L, true)")
    assertResult(TTuple(Seq(TInt, TString, TLong, TBool)))(typer.checkExp(ctx)(tuple2))

    val tuple3 = parseExp("(1, x, 2L, y)")
    assertThrows[TypeError](typer.checkExp(ctx)(tuple3))
  }

  "checkExp" should "type eq correctly" in {
    val ctx = TypeContext(Map("x" -> TInt), Map(), null)

    val eq = parseExp("1 == x")
    assertResult(TBool)(typer.checkExp(ctx)(eq))

    val neq = parseExp("x != 1")
    assertResult(TBool)(typer.checkExp(ctx)(neq))

    val notCompatibleEq = parseExp("true == x")
    assertThrows[TypeError](typer.checkExp(ctx)(notCompatibleEq))

    val notCompatibleNeq = parseExp("true != x")
    assertThrows[TypeError](typer.checkExp(ctx)(notCompatibleNeq))
  }

  "checkExp" should "type def correctly" in {
    val funEnv = Map("f" -> PatternFunction(None, "f", Seq(Param("x", TInt)), Seq(), Seq()))
    val ctx = TypeContext(Map("x" -> TNode(analyzedLangs.Exp.addTag)), funEnv, null)

    val defPathAccess = parseExp("def x.lhs")
    assertResult(TBool)(typer.checkExp(ctx)(defPathAccess))

    val defCall = parseExp("def f(1)")
    assertResult(TBool)(typer.checkExp(ctx)(defCall))

    val defInvalid = parseExp("def x")
    assertThrows[TypeError](typer.checkExp(ctx)(defInvalid))
  }

  "checkExp" should "type instanceOf correctly" in {
    val ctx = TypeContext(Map("x" -> TNode(analyzedLangs.Exp.expTag)), Map(), null)

    val instanceOf = parseExp(s"x instanceOf ${TNode(analyzedLangs.Exp.addTag).prettyprint}")
    assertResult(TBool)(typer.checkExp(ctx)(instanceOf))

    val notInstanceOf = parseExp(s"x notInstanceOf ${TNode(analyzedLangs.Exp.addTag).prettyprint}")
    assertResult(TBool)(typer.checkExp(ctx)(notInstanceOf))

    val invalidInstanceOf = parseExp("x instanceOf TBool")
    assertThrows[TypeError](typer.checkExp(ctx)(invalidInstanceOf))

    val tupleInstanceOf = parseExp("(x, 1) instanceOf TBool")
    assertThrows[TypeError](typer.checkExp(ctx)(tupleInstanceOf))
  }


  "checkExp" should "type call correctly" in {
    val funEnv = Map("fun" -> PatternFunction(None, "fun", Seq(Param("x", TInt), Param("y", TInt)), Seq(AnnoParam(None, TBool)), Seq()))
    val ctx = TypeContext(Map(), funEnv, null)

    val call = parseExp("fun(1, 2)")
    assertResult(TBool)(typer.checkExp(ctx)(call))

    val wrongNumArgs = parseExp("fun(1, 2, 2)")
    assertThrows[TypeError](typer.checkExp(ctx)(wrongNumArgs))

    val wrongArgType = parseExp("fun(1, true)")
    assertThrows[TypeError](typer.checkExp(ctx)(wrongArgType))

    val undefinedCall = parseExp("other(1, x, 2L, y)")
    assertThrows[TypeError](typer.checkExp(ctx)(undefinedCall))

    val funEnv2 = Map("fun" -> PatternFunction(None, "fun", Seq(Param("x", TNode(analyzedLangs.Exp.expTag))), Seq(), Seq()))
    val ctx2 = TypeContext(Map("x" -> TNode(analyzedLangs.Exp.addTag)), funEnv2, null)
    val callWithNodeArgs = parseExp("fun(x)")
    assertResult(TUnit)(typer.checkExp(ctx2)(callWithNodeArgs))

    val callWithNodeArgWrongType = parseExp("fun(1)")
    assertThrows[TypeError](typer.checkExp(ctx2)(callWithNodeArgWrongType))

    val funEnv3 = Map("fun" -> PatternFunction(None, "fun", Seq(Param("x", TNode(analyzedLangs.Exp.addTag))), Seq(AnnoParam(None, TNode(analyzedLangs.Exp.expTag)), AnnoParam(None, TNode(analyzedLangs.Exp.expTag))), Seq()))
    val ctx3 = TypeContext(Map("x" -> TNode(analyzedLangs.Exp.intTag), "y" -> TNode(analyzedLangs.Exp.addTag)), funEnv3, null)

    val callWithNodeArgWrongType2 = parseExp("fun(x)")
    assertThrows[TypeError](typer.checkExp(ctx3)(callWithNodeArgWrongType2))

    val callMultipleOutputTypes = parseExp("fun(y)")
    assertResult(TTuple(Seq(TNode(analyzedLangs.Exp.expTag), TNode(analyzedLangs.Exp.expTag))))(typer.checkExp(ctx3)(callMultipleOutputTypes))
  }

  "checkExp" should "type count correctly" in {
    val funEnv = Map("fun" -> PatternFunction(None, "fun", Seq(Param("x", TInt), Param("y", TInt)), Seq(AnnoParam(None, TBool)), Seq()))
    val ctx = TypeContext(Map(), funEnv, null)

    val count = parseExp("count fun(1, 2)")
    assertResult(TInt)(typer.checkExp(ctx)(count))
  }

  "checkStatement" should "type assign correctly" in {
    val ctx = TypeContext(Map(), Map(), null)

    val assign = parseStatement("val x = 1")
    assertResult(TypeContext(Map("x" -> TInt), Map(), null))(typer.checkStatement(ctx)(assign))

    val tupleAssign = parseStatement("val (x, y) = (1, true)")
    assertResult(TypeContext(Map("x" -> TInt, "y" -> TBool), Map(), null))(typer.checkStatement(ctx)(tupleAssign))

    val differentSized = parseStatement("val (x, y) = 1")
    assertThrows[TypeError](typer.checkStatement(ctx)(differentSized))

    val differentSized2 = parseStatement("val (x, y) = (1)")
    assertThrows[TypeError](typer.checkStatement(ctx)(differentSized2))

    val reassignCtx = TypeContext(Map("x" -> TInt), Map(), null)
    val reassign = parseStatement("val x = true")
    assertThrows[TypeError](typer.checkStatement(reassignCtx)(reassign))
  }

  "checkStatement" should "type values correctly" in {
    val ctx = TypeContext(Map(), Map(), null)

    val single = parseStatement("vals x <- Int")
    assertResult(TypeContext(Map("x" -> TInt), Map(), null))(typer.checkStatement(ctx)(single))

    val reassignCtx = TypeContext(Map("x" -> TInt), Map(), null)
    val reassign = parseStatement("vals x <- Bool")
    assertThrows[TypeError](typer.checkStatement(reassignCtx)(reassign))
  }

  "checkStatement" should "type assert correctly" in {
    val ctx = TypeContext(Map("x" -> TNode(analyzedLangs.Exp.expTag)), Map(), null)

    val assertBool = parseStatement("assert true")
    assertResult(ctx)(typer.checkStatement(ctx)(assertBool))

    val assertInstanceOf = parseStatement(s"assert x instanceOf ${TNode(analyzedLangs.Exp.addTag).prettyprint}")
    assertResult(ctx.refineBinding("x", TNode(analyzedLangs.Exp.addTag)))(typer.checkStatement(ctx)(assertInstanceOf))

    val assertNotBool = parseStatement("assert 1")
    assertThrows[TypeError](typer.checkStatement(ctx)(assertNotBool))
  }

  "checkStatement" should "type yield correctly" in {
    val fun = PatternFunction(None, "fun", Seq(Param("y", TInt)), Seq(AnnoParam(None, TBool)), Seq())
    val funEnv = Map("fun" -> fun)
    val ctx = TypeContext(Map(), funEnv, fun)
    val yieldStmt = parseStatement("yield true")
    assertThrows[TypeError](typer.checkStatement(ctx)(yieldStmt))
  }

  "checkYield" should "type yield correctly" in {
    val fun = PatternFunction(None, "fun", Seq(Param("y", TInt)), Seq(AnnoParam(None, TDouble)), Seq())
    val ctx = TypeContext(Map(), Map("fun" -> fun), fun)
    val yieldStmt = parseStatement("yield 1.0")
    assertResult(())(typer.checkYield(ctx)(yieldStmt))

    val yieldInt = parseStatement("yield 1")
    assertThrows[TypeError](typer.checkYield(ctx)(yieldInt))

    val yieldUnit = parseStatement("yield unit")
    val fun2 = PatternFunction(None, "fun", Seq(Param("y", TInt)), Seq(), Seq())
    val ctx2 = TypeContext(Map(), Map("fun" -> fun2), fun2)
    assertResult(())(typer.checkYield(ctx2)(yieldUnit))

    val yieldTuple = parseStatement("yield (true, 1L)")
    val fun3 = PatternFunction(None, "fun", Seq(Param("y", TInt)), Seq(AnnoParam(None, TBool), AnnoParam(None, TLong)), Seq())
    val ctx3 = TypeContext(Map(), Map("fun" -> fun3), fun3)
    assertResult(())(typer.checkYield(ctx3)(yieldTuple))
  }

  "checkBody" should "type body correctly" in {
    val fun = PatternFunction(None, "fun", Seq(Param("y", TInt)), Seq(AnnoParam(None, TBool), AnnoParam(None, TLong)), Seq())
    val ctx = TypeContext(Map("x" -> TNode(analyzedLangs.Exp.expTag)), Map("fun" -> fun), fun)

    val body = parseBody(
      s"""{
        |assert x instanceOf ${TNode(analyzedLangs.Exp.addTag)}
        |val lhs = x.lhs
        |assert lhs instanceOf ${TNode(analyzedLangs.Exp.multTag)}
        |yield (false, 2L)
        |}
        |
        |""".stripMargin)
      assertResult(())(typer.checkBody(ctx)(body))

    val bodyYieldNotLast = parseBody(
      """{
        |yield (true, 2L)
        |val x = 1
        |}
        |""".stripMargin)
    assertThrows[TypeError](typer.checkBody(ctx)(bodyYieldNotLast))
  }

  "checkFun" should "type fun correctly" in {
    val expNode = TNode(analyzedLangs.Exp.expTag)
    val addNode = TNode(analyzedLangs.Exp.addTag)
    val multNode = TNode(analyzedLangs.Exp.multTag)
    val fun = parsePatternFunction(
      s"""
        |def lhs(x: ${expNode.prettyprint}): ${expNode.prettyprint} = {
        |  assert x instanceOf ${addNode.prettyprint}
        |  val lhs = x.lhs
        |  yield lhs
        |} union {
        |  assert x instanceOf ${multNode.prettyprint}
        |  val lhs = x.lhs
        |  yield lhs
        |}
        |""".stripMargin)

    val funSubtypeYield = parsePatternFunction(
      s"""
         |def lhs(x: ${expNode.prettyprint}): ${expNode.prettyprint} = {
         |  assert x instanceOf ${addNode.prettyprint}
         |  val lhs = x.lhs
         |  yield x
         |}
         |""".stripMargin)
    val funEnv = Map("lhs" -> fun)
    assertResult(())(typer.checkFun(funEnv)(funSubtypeYield))

    val funFailUnbound = parsePatternFunction(
      s"""
         |def lhs(y: ${expNode.prettyprint}): ${expNode.prettyprint} = {
         |  assert x instanceOf ${addNode.prettyprint}
         |  val lhs = x.lhs
         |  yield lhs
         |}
         |""".stripMargin)
    assertThrows[TypeError](typer.checkFun(funEnv)(funFailUnbound))

    val funFailWrongYield = parsePatternFunction(
      s"""
         |def lhs(x: ${expNode.prettyprint}): ${expNode.prettyprint} = {
         |  assert x instanceOf ${addNode.prettyprint}
         |  val lhs = x.lhs
         |  yield 1
         |}
         |""".stripMargin)
    assertThrows[TypeError](typer.checkFun(funEnv)(funFailWrongYield))
  }

  "checkModules" should "type modules correctly" in {
    val mod1 = parseModule(
      """module mod1
        |
        |def hello(): String = {
        |  yield "Hello World"
        |}
        |""".stripMargin
    )

    val mod2 = parseModule(
      """module mod2
        |import mod1
        |
        |def main(): Unit = {
        |  val x = hello()
        |  yield unit
        |}
        |""".stripMargin
    )
    assertResult(())(typer.checkModules(Seq(mod1, mod2)))

    val secondMod1 = parseModule(
      """module mod1
        |
        |def main(): Unit = {
        |  val x = hello()
        |  yield unit
        |}
        |
        |def hello(): String = {
        |  yield "x"
        |}
        |""".stripMargin
    )

    val mod3 = parseModule(
      """module mod3
        |import mod1
        |
        |def hello(): String = {
        |  yield "x"
        |}
        |""".stripMargin
    )

    val mod4 = parseModule(
      """module mod4
        |
        |private def hello(): String = {
        |  yield "x"
        |}
        |
        |def hello2(): String = {
        |  yield "x"
        |}
        |""".stripMargin
    )

    val mod5 = parseModule(
      """module mod5
        |import mod4
        |
        |def main(): String = {
        |  val str = hello()
        |  yield str
        |}
        |""".stripMargin
    )
    assertThrows[TypeError](typer.checkModules(Seq(mod4, mod5)))

    val mod6 = parseModule(
      """module mod6
        |import mod4
        |
        |private def main(): String = {
        |  val str = hello()
        |  yield str
        |}
        |
        |def hello2(): String = {
        |  yield "hello"
        |}
        |""".stripMargin
    )
    assertThrows[TypeError](typer.checkModules(Seq(mod4, mod6)))
  }
}
