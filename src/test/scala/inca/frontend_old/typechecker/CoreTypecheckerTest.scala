package inca.frontend_old.typechecker

import fastparse.Parsed.{Failure, Success}
import fastparse._
import inca.analyzedLangs
import inca.frontend_old.core.Frontend
import inca.frontend_old.core.tree._
import inca.runtime.context.LanguageMetaInfo
import inca.util.Meta.Scala
import org.scalatest.Assertion
import org.scalatest.flatspec.AnyFlatSpec

/**
  * Test class for the IncA core language typechecker.
  */
class CoreTypecheckerTest extends AnyFlatSpec {
  val parser = Frontend.Core(new LanguageMetaInfo())

  "subtype" should "work" in {
    val typer = Frontend.Core(new LanguageMetaInfo())
    def test_run(t1 : Type, t2 : Type) = assert(typer.subtype(t1, t2, null))

    test_run(TLiteral.Bool, TLiteral.Bool)
    test_run(TLiteral.Bool, TAny)
    test_run(TLiteral.String, TAny)
    test_run(TNode("apf3l"), TAnyLinked)
  }

  "typechecker" should "work" in {
    def test_run(cd: String) = {
      parse(cd, parser.module(_)) match {
        case Success(value, index) => {
          val typer = Frontend.Core(new LanguageMetaInfo())
          typer.typecheck(Seq(value))
          assert(typer.getErrors.isEmpty)
        }
        case Failure(label, index, extra) =>
          fail(s" ${cd.slice(index - 10, index + 10)} $label, $index, $extra")
      }
    }

    test_run{
      s"""module test
          |
          |def name() : `Int` = {
          |    val x = 5
          |    yield x
          |}""".stripMargin}
    test_run{
      s"""module test
          |
          |def name() : Unit = {
          |    val x = 5
          |    yield unit
          |}""".stripMargin}
    test_run{
      s"""module test
          |
          |def name() : `Int` = {
          |    val x = 5
          |    yield x
          |} union {
          |    yield 10
          |}""".stripMargin}
    test_run{
      s"""module test
          |
          |def name() : `Int` = {
          |    val x = 5
          |    yield x
          |}
          |
          |def another() : `Int` = {
          |    yield name()
          |} """.stripMargin}
    test_run{
      s"""module test
          |
          |def name() : `Int` = {
          |    val x = 4
          |    yield `x + 38`
          |} """.stripMargin}
    test_run{
      s"""module test
          |
          |def name() : Any = {
          |    val x = 5
          |    yield x
          |} """.stripMargin}
    test_run{
      s"""module test
          |
          |def name() : Any = {
          |    val x = true
          |    assert x.isInstanceOf[Any]
          |    yield x
          |} """.stripMargin}

    val code1 = s"""module test
                   |
                   |def name(x: Any): Any = {
                   |  assert x.isInstanceOf[Node]
                   |  yield x.parent
                   |}
                   |""".stripMargin
    test_run(code1)
  }


  "imports" should "work" in {
    val mod1Src =
      """module test1
        |
        |def hello(): `String` = {
        |  yield "Hello World"
        |}
        |""".stripMargin
    val mod1 = parse(mod1Src, parser.module(_)).get.value
    val src =
      """module main
        |import test1
        |
        |def main(): Unit = {
        |  val x = hello()
        |  yield unit
        |}
        |""".stripMargin

    val code = parse(src, parser.module(_)).get.value
    val prog = Seq(mod1, code)
    val typechecker = Frontend.Core(new LanguageMetaInfo())
    typechecker.typecheck(prog)
    assert(typechecker.getErrors.isEmpty)
  }

  def parseExp(str: String): Expression = {
    parse(str, parser.exp(_)).get.value
  }

  def parseStatement(str: String): Statement = {
    parse(str, parser.statement(_)).get.value
  }

  def parseBody(str: String): Body = {
    parse(str, parser.body(_)).get.value
  }

  def parsePatternFunction(str: String): PatternFunction = {
    parse(str, parser.patternFunction(_)).get.value.asInstanceOf[PatternFunction]
  }

  def parseModule(str: String): Module = {
    parse(str, parser.module(_), verboseFailures = true) match {
      case Success(value, index) => value
      case Failure(label, index, extra) =>
        throw new IllegalArgumentException(extra.trace(true).longMsg)
    }
  }

  def typecheckExp(exp: Expression, vars: Map[Name, Type] = Map(), funs: Seq[PatternFunction] = Seq()): Type = {
    val typer = Frontend.Core(analyzedLangs.Exp.languageMetaInfo)
    vars.foreach { case (name, ty) => typer.bindVar(name, new Var.Target {}, ty) }
    funs.foreach { fun => typer.bindFun(fun, Module(Name(fun.name.name + "-module"), Seq(), Seq(fun))) }
    typer.typecheck(exp)
  }

  def assertTypecheckExpFail(exp: Expression, vars: Map[Name, Type] = Map(), funs: Seq[PatternFunction] = Seq()): Assertion = {
    val typer = Frontend.Core(analyzedLangs.Exp.languageMetaInfo)
    vars.foreach { case (name, ty) => typer.bindVar(name, new Var.Target {}, ty) }
    funs.foreach { fun => typer.bindFun(fun, Module(Name(fun.name.name + "-module"), Seq(), Seq(fun))) }
    typer.typecheck(exp)
    assert(typer.getErrors.nonEmpty)
  }

  def assertTypecheckExpWarn(exp: Expression, vars: Map[Name, Type] = Map(), funs: Seq[PatternFunction] = Seq()): Assertion = {
    val typer = Frontend.Core(analyzedLangs.Exp.languageMetaInfo)
    vars.foreach { case (name, ty) => typer.bindVar(name, new Var.Target {}, ty) }
    funs.foreach { fun => typer.bindFun(fun, Module(Name(fun.name.name + "-module"), Seq(), Seq(fun))) }
    typer.typecheck(exp)
    assert(typer.getErrors.isEmpty)
    assert(typer.getWarnings.nonEmpty)
  }


  def typecheckStmBindings(stm: Statement, vars: Map[Name, Type] = Map(), funs: Seq[PatternFunction] = Seq()): Map[Name, Type] = {
    val typer = Frontend.Core(analyzedLangs.Exp.languageMetaInfo)
    vars.foreach { case (name, ty) => typer.bindVar(name, new Var.Target {}, ty) }
    funs.foreach { fun => typer.bindFun(fun, Module(Name(fun.name.name + "-module"), Seq(), Seq(fun))) }
    typer.typecheck(stm, mustYield = false, mayYield = true)
    typer.getBindings
  }

  def typecheckStm(stm: Statement, vars: Map[Name, Type] = Map(), funs: Seq[PatternFunction] = Seq(), mustTerminate: Boolean = false): StmType = {
    val typer = Frontend.Core(analyzedLangs.Exp.languageMetaInfo)
    vars.foreach { case (name, ty) => typer.bindVar(name, new Var.Target {}, ty) }
    funs.foreach { fun => typer.bindFun(fun, Module(Name(fun.name.name + "-module"), Seq(), Seq(fun))) }
    typer.typecheck(stm, mustTerminate, mayYield = true)
  }

  def assertTypecheckStmFail(stm: Statement, vars: Map[Name, Type] = Map(), funs: Seq[PatternFunction] = Seq()): Assertion = {
    val typer = Frontend.Core(analyzedLangs.Exp.languageMetaInfo)
    vars.foreach { case (name, ty) => typer.bindVar(name, new Var.Target {}, ty) }
    funs.foreach { fun => typer.bindFun(fun, Module(Name(fun.name.name + "-module"), Seq(), Seq(fun))) }
    typer.typecheck(stm, mustYield = false, mayYield = true)
    assert(typer.getErrors.nonEmpty)
  }

  def assertTypecheckStmWarn(stm: Statement, vars: Map[Name, Type] = Map(), funs: Seq[PatternFunction] = Seq()): Assertion = {
    val typer = Frontend.Core(analyzedLangs.Exp.languageMetaInfo)
    vars.foreach { case (name, ty) => typer.bindVar(name, new Var.Target {}, ty) }
    funs.foreach { fun => typer.bindFun(fun, Module(Name(fun.name.name + "-module"), Seq(), Seq(fun))) }
    typer.typecheck(stm, mustYield = false, mayYield = true)
    assert(typer.getErrors.isEmpty)
    assert(typer.getWarnings.nonEmpty)
  }

  def typecheckFun(fun: PatternFunction, vars: Map[Name, Type] = Map(), funs: Seq[PatternFunction] = Seq()): Unit = {
    val typer = Frontend.Core(analyzedLangs.Exp.languageMetaInfo)
    vars.foreach { case (name, ty) => typer.bindVar(name, new Var.Target {}, ty) }
    funs.foreach { fun => typer.bindFun(fun, Module(Name(fun.name.name + "-module"), Seq(), Seq(fun))) }
    typer.typecheck(fun)
  }

  def assertTypecheckFunFail(fun: PatternFunction, vars: Map[Name, Type] = Map(), funs: Seq[PatternFunction] = Seq()): Assertion = {
    val typer = Frontend.Core(analyzedLangs.Exp.languageMetaInfo)
    vars.foreach { case (name, ty) => typer.bindVar(name, new Var.Target {}, ty) }
    funs.foreach { fun => typer.bindFun(fun, Module(Name(fun.name.name + "-module"), Seq(), Seq(fun))) }
    typer.typecheck(fun)
    assert(typer.getErrors.nonEmpty)
  }

  def assertTypecheckFunWarn(fun: PatternFunction, vars: Map[Name, Type] = Map(), funs: Seq[PatternFunction] = Seq()): Assertion = {
    val typer = Frontend.Core(analyzedLangs.Exp.languageMetaInfo)
    vars.foreach { case (name, ty) => typer.bindVar(name, new Var.Target {}, ty) }
    funs.foreach { fun => typer.bindFun(fun, Module(Name(fun.name.name + "-module"), Seq(), Seq(fun))) }
    typer.typecheck(fun)
    assert(typer.getErrors.isEmpty)
    assert(typer.getWarnings.nonEmpty)
  }

  def assertTypecheckModulesSucceed(modules: Seq[Module], vars: Map[Name, Type] = Map(), funs: Seq[PatternFunction] = Seq()): Assertion = {
    val typer = Frontend.Core(analyzedLangs.Exp.languageMetaInfo)
    vars.foreach { case (name, ty) => typer.bindVar(name, new Var.Target {}, ty) }
    funs.foreach { fun => typer.bindFun(fun, Module(Name(fun.name.name + "-module"), Seq(), Seq(fun))) }
    typer.typecheck(modules)
    assert(typer.getErrors.isEmpty)
  }

  def assertTypecheckModulesFail(modules: Seq[Module], vars: Map[Name, Type] = Map(), funs: Seq[PatternFunction] = Seq()): Assertion = {
    val typer = Frontend.Core(analyzedLangs.Exp.languageMetaInfo)
    vars.foreach { case (name, ty) => typer.bindVar(name, new Var.Target {}, ty) }
    funs.foreach { fun => typer.bindFun(fun, Module(Name(fun.name.name + "-module"), Seq(), Seq(fun))) }
    typer.typecheck(modules)
    assert(typer.getErrors.nonEmpty)
  }


  "checkExp" should "type var correctly" in {
    val varExp = parseExp("x")
    assertResult(TLiteral.Bool)(typecheckExp(varExp, Map(Name("x") -> TLiteral.Bool)))
    assertTypecheckExpFail(varExp, Map())
  }

  "checkExp" should "type constant literals correctly" in {
    val boolConst = parseExp("true")
    val intConst = parseExp("12")
    val longConst = parseExp("12L")
    val doubleConst = parseExp("12.0")
    val stringConst = parseExp("\"str\"")

    assertResult(TScalaBoolean)(typecheckExp(boolConst))
    assertResult(TScalaInt)(typecheckExp(intConst))
    assertResult(TScalaLong)(typecheckExp(longConst))
    assertResult(TScalaDouble)(typecheckExp(doubleConst))
    assertResult(TScalaString)(typecheckExp(stringConst))
  }

  "checkExp" should "type wildcard correctly" in {
    val wildcard = parseExp("_")

    assertResult(typecheckExp(wildcard))(TAny)
  }

  private val manyVars = Map(Name("many") -> TNode(analyzedLangs.Exp.manyTag))

  "checkExp" should "type path access named link correctly" in {
    val pathAccess = parseExp("add.lhs")

    val vars = Map(Name("add") -> TNode(analyzedLangs.Exp.addTag))
    assertResult(TNode(analyzedLangs.Exp.expTag))(typecheckExp(pathAccess, vars))

    assertTypecheckExpFail(pathAccess)

    val invalidPathAccess = parseExp("add.vl")
    assertTypecheckExpFail(invalidPathAccess)

    val listPathAccess = parseExp("many.exps")
    assertResult(TList(TNode(analyzedLangs.Exp.expTag)))(typecheckExp(listPathAccess, manyVars))

    val listSize = parseExp("many.exps.size")
    assertResult(TScalaInt)(typecheckExp(listSize, manyVars))

    val listChilds = parseExp("many.exps.children")
    assertResult(TNode(analyzedLangs.Exp.expTag))(typecheckExp(listChilds, manyVars))
  }

  "checkExp" should "type path access parent link correctly" in {
    val parent = parseExp("many.parent")
    assertResult(TAny)(typecheckExp(parent, manyVars))
  }

  "checkExp" should "type path access size link correctly" in {
    val listSize = parseExp("many.exps.size")
    assertResult(TScalaInt)(typecheckExp(listSize, manyVars))
  }

  "checkExp" should "type path access children link correctly" in {
    val listChilds = parseExp("many.exps.children")
    assertResult(TNode(analyzedLangs.Exp.expTag))(typecheckExp(listChilds, manyVars))
  }

  "checkExp" should "type path access next link correctly" in {
    val listChildsNext = parseExp("many.exps.children.next")
    assertResult(TAny)(typecheckExp(listChildsNext, manyVars))
  }

  "checkExp" should "type path access previous link correctly" in {
    val listChildsPrev = parseExp("many.exps.children.previous")
    assertResult(TAny)(typecheckExp(listChildsPrev, manyVars))
  }

  "checkExp" should "type tuple correctly" in {
    val vars = Map(Name("x") -> TLiteral.String)

    val tuple = parseExp("(1, x)")
    assertResult(TTuple(Seq(TScalaInt, TLiteral.String)))(typecheckExp(tuple, vars))

    val tuple2 = parseExp("(1, x, 2L, true)")
    assertResult(TTuple(Seq(TScalaInt, TLiteral.String, TScalaLong, TScalaBoolean)))(typecheckExp(tuple2, vars))

    val tuple3 = parseExp("(1, x, 2L, y)")
    assertTypecheckExpFail(tuple3, vars)
  }

  "checkExp" should "type eq correctly" in {
    val vars = Map(Name("x") -> TLiteral.Int)

    val eq = parseExp("1 == x")
    assertResult(TScalaBoolean)(typecheckExp(eq, vars))

    val neq = parseExp("x != 1")
    assertResult(TScalaBoolean)(typecheckExp(neq, vars))

    val notCompatibleEq = parseExp("true == x")
    assertTypecheckExpFail(notCompatibleEq, vars)

    val notCompatibleNeq = parseExp("true != x")
    assertTypecheckExpFail(notCompatibleNeq, vars)
  }

  "checkExp" should "type def correctly" in {
    val funs = Seq(PatternFunction(None, Name("f"), Seq(Param(Name("x"), TLiteral.Int)), TUnit, Seq()))
    val vars = Map(Name("x") -> TNode(analyzedLangs.Exp.addTag))

    val defPathAccess = parseExp("def x.lhs")
    assertResult(TScalaBoolean)(typecheckExp(defPathAccess, vars, funs))

    val defCall = parseExp("def f(1)")
    assertResult(TScalaBoolean)(typecheckExp(defCall, vars, funs))

    val defInvalid = parseExp("def x")
    assertTypecheckExpFail(defInvalid, vars, funs)
  }

  "checkExp" should "type instanceOf correctly" in {
    val vars = Map(Name("x") -> TNode(analyzedLangs.Exp.expTag))

    val str = s"x.isInstanceOf[${TNode(analyzedLangs.Exp.addTag).prettyprint}]"
    val instanceOf = parseExp(str)
    val t = typecheckExp(instanceOf, vars)
    assertResult(TScalaBoolean)(t)

    val notInstanceOf = parseExp(s"x.notInstanceOf[${TNode(analyzedLangs.Exp.addTag).prettyprint}]")
    assertResult(TScalaBoolean)(typecheckExp(notInstanceOf, vars))

    val invalidInstanceOf = parseExp("x.isInstanceOf[TBool]")
    assertTypecheckExpWarn(invalidInstanceOf, vars)

    val tupleInstanceOf = parseExp("(x, 1).isInstanceOf[TBool]")
    assertTypecheckExpWarn(tupleInstanceOf, vars)
  }


  "checkExp" should "type call correctly" in {
    val funs = Seq(PatternFunction(None, Name("fun"), Seq(Param(Name("x"), TLiteral.Int), Param(Name("y"), TLiteral.Int)), TLiteral.Bool, Seq()))

    val call = parseExp("fun(1, 2)")
    assertResult(TLiteral.Bool)(typecheckExp(call, Map(), funs))

    val wrongNumArgs = parseExp("fun(1, 2, 2)")
    assertTypecheckExpFail(wrongNumArgs, Map(), funs)

    val wrongArgType = parseExp("fun(1, true)")
    assertTypecheckExpWarn(wrongArgType, Map(), funs)

    val undefinedCall = parseExp("other(1, x, 2L, y)")
    assertTypecheckExpFail(undefinedCall, Map(), funs)

    val funs2 = Seq(PatternFunction(None, Name("fun"), Seq(Param(Name("x"), TNode(analyzedLangs.Exp.expTag))), TUnit, Seq()))
    val vars2 = Map(Name("x") -> TNode(analyzedLangs.Exp.addTag))
    val callWithNodeArgs = parseExp("fun(x)")
    assertResult(TUnit)(typecheckExp(callWithNodeArgs, vars2, funs2))

    val callWithNodeArgWrongType = parseExp("fun(1)")
    assertTypecheckExpWarn(callWithNodeArgWrongType, vars2, funs2)

    val funs3 = Seq(PatternFunction(None, Name("fun"), Seq(Param(Name("x"), TNode(analyzedLangs.Exp.addTag))), TTuple(Seq(TNode(analyzedLangs.Exp.expTag), TNode(analyzedLangs.Exp.expTag))), Seq()))
    val vars3 = Map(Name("x") -> TNode(analyzedLangs.Exp.intTag), Name("y") -> TNode(analyzedLangs.Exp.addTag))

    val callWithNodeArgWrongType2 = parseExp("fun(x)")
    assertTypecheckExpWarn(callWithNodeArgWrongType2, vars3, funs3)

    val callMultipleOutputTypes = parseExp("fun(y)")
    assertResult(TTuple(Seq(TNode(analyzedLangs.Exp.expTag), TNode(analyzedLangs.Exp.expTag))))(typecheckExp(callMultipleOutputTypes, vars3, funs3))
  }

  "checkExp" should "type count correctly" in {
    val funs = Seq(PatternFunction(None, Name("fun"), Seq(Param(Name("x"), TLiteral.Int), Param(Name("y"), TLiteral.Int)), TLiteral.Bool, Seq()))

    val count = parseExp("count fun(1, 2)")
    assertResult(TScalaInt)(typecheckExp(count, Map(), funs))
  }

  "checkStatement" should "type assign correctly" in {
    val assign = parseStatement("val x = 1")
    assertResult(Map(Name("x") -> TScalaInt))(typecheckStmBindings(assign))

    val tupleAssign = parseStatement("val (x, y) = (1, true)")
    assertResult(Map(Name("x") -> TScalaInt, Name("y") -> TScalaBoolean))(typecheckStmBindings(tupleAssign))

    val differentSized = parseStatement("val (x, y) = 1")
    assertTypecheckStmFail(differentSized)

    val differentSized2 = parseStatement("val (x, y) = (1)")
    assertTypecheckStmFail(differentSized2)

    val vars = Map(Name("x") -> TLiteral.Int)
    val reassign = parseStatement("val x = true")
    assertTypecheckStmFail(reassign, vars)
  }

  "checkStatement" should "type values correctly" in {
    val single = parseStatement("vals x <- Int")
    assertResult(Map(Name("x") -> TLiteral.Int))(typecheckStmBindings(single))

    val vars = Map(Name("x") -> TLiteral.Int)
    val reassign = parseStatement("vals x <- Bool")
    assertTypecheckStmFail(reassign, vars)
  }

  "checkStatement" should "type assert correctly" in {
    val vars = Map(Name("x") -> TNode(analyzedLangs.Exp.expTag))

    val assertBool = parseStatement("assert true")
    assertResult(vars)(typecheckStmBindings(assertBool, vars))

    val str = s"x.isInstanceOf[${TNode(analyzedLangs.Exp.addTag).prettyprint}]"
    val assertInstanceOf = parseStatement(s"assert $str")
    assertResult(vars)(typecheckStmBindings(assertInstanceOf, vars))

    val assertNotBool = parseStatement("assert 1")
    assertTypecheckStmFail(assertNotBool, vars)
  }

  "checkStatement" should "type yield correctly" in {
    val yieldStmt = parseStatement("yield true")
    assertResult(Yields(TScalaBoolean))(typecheckStm(yieldStmt, mustTerminate = true))
  }

  "checkYield" should "type yield correctly" in {
    val fun = (y: Statement) => PatternFunction(None, Name("fun"), Seq(Param(Name("y"), TLiteral.Int)), TLiteral.Double, Seq(Body(y)))

    val yieldStmt = parseStatement("yield 1.0")
    assertResult(())(typecheckFun(fun(yieldStmt)))

    val yieldInt = parseStatement("yield true")
    assertTypecheckFunFail(fun(yieldInt))

    val yieldUnit = parseStatement("yield unit")
    val fun2 = (y: Statement) => PatternFunction(None, Name("fun"), Seq(Param(Name("y"), TLiteral.Int)), TUnit, Seq(Body(y)))
    assertResult(())(typecheckFun(fun2(yieldUnit)))

    val yieldTuple = parseStatement("yield (true, 1L)")
    val fun3 = (y: Statement) => PatternFunction(None, Name("fun"), Seq(Param(Name("y"), TLiteral.Int)), TTuple(Seq(TLiteral.Bool, TLiteral.Long)), Seq(Body(y)))
    assertResult(())(typecheckFun(fun3(yieldTuple)))
  }

  "checkBody" should "type body correctly" in {
    val fun = (body: Body) => PatternFunction(None, Name("fun"), Seq(Param(Name("y"), TLiteral.Int)), TTuple(Seq(TLiteral.Bool, TLiteral.Long)), Seq(body))
    val vars = Map(Name("x") -> TNode(analyzedLangs.Exp.expTag))

    val body = parseBody(
      s"""{
         |assert x.isInstanceOf[${TNode(analyzedLangs.Exp.addTag)}]
         |val lhs = x.lhs
         |assert lhs.isInstanceOf[${TNode(analyzedLangs.Exp.multTag)}]
         |yield (false, 2L)
         |}
         |
         |""".stripMargin)
    assertResult(())(typecheckFun(fun(body), vars))

    val bodyYieldNotLast = parseBody(
      """{
        |yield (true, 2L)
        |val x = 1
        |}
        |""".stripMargin)
    assertTypecheckFunFail(fun(bodyYieldNotLast))
  }

  "checkFun" should "type fun correctly" in {
    val expNode = TNode(analyzedLangs.Exp.expTag)
    val addNode = TNode(analyzedLangs.Exp.addTag)
    val multNode = TNode(analyzedLangs.Exp.multTag)
    val fun = parsePatternFunction(
      s"""
         |def lhs(x: ${expNode.prettyprint}): ${expNode.prettyprint} = {
         |  assert x.isInstanceOf[${addNode.prettyprint}]
         |  val lhs = x.lhs
         |  yield lhs
         |} union {
         |  assert x.isInstanceOf[${multNode.prettyprint}]
         |  val lhs = x.lhs
         |  yield lhs
         |}
         |""".stripMargin)

    val funSubtypeYield = parsePatternFunction(
      s"""
         |def lhs(x: ${expNode.prettyprint}): ${expNode.prettyprint} = {
         |  assert x.isInstanceOf[${addNode.prettyprint}]
         |  val lhs = x.lhs
         |  yield x
         |}
         |""".stripMargin)

    assertResult(())(typecheckFun(fun, Map(), Seq(fun)))
    assertResult(())(typecheckFun(funSubtypeYield, Map(), Seq(funSubtypeYield)))

    val funFailUnbound = parsePatternFunction(
      s"""
         |def lhs(y: ${expNode.prettyprint}): ${expNode.prettyprint} = {
         |  assert x.isInstanceOf[${addNode.prettyprint}]
         |  val lhs = x.lhs
         |  yield lhs
         |}
         |""".stripMargin)
    assertTypecheckFunFail(funFailUnbound, Map(), Seq(fun))

    val funFailWrongYield = parsePatternFunction(
      s"""
         |def lhs(x: ${expNode.prettyprint}): ${expNode.prettyprint} = {
         |  assert x.isInstanceOf[${addNode.prettyprint}]
         |  val lhs = x.lhs
         |  yield 1
         |}
         |""".stripMargin)
    assertTypecheckFunFail(funFailWrongYield, Map(), Seq(fun))
  }


  "checkModules" should "type modules correctly" in {
    val mod1 = parseModule(
      """module mod1
        |
        |def hello(): `String` = {
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
    assertTypecheckModulesSucceed(Seq(mod1, mod2))

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
    assertTypecheckModulesFail(Seq(mod4, mod5))

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
    assertTypecheckModulesFail(Seq(mod4, mod6))
  }

  "checkModules" should "type modules with scala metas correctly" in {
    def testModuleSucceeds(mod: Module) = assertTypecheckModulesSucceed(Seq(mod))
    def testModuleFail(mod: Module) = assertTypecheckModulesFail(Seq(mod))

    import meta.quasiquotes._
    val module1 = Module(Name("Test"), Seq(),
      Seq(
        ScalaModuleContent(Scala(q"import inca.analyzedData.Nat.{Nat, Zero}")),
        PatternFunction(None, Name("test"), Seq(), TScala("Nat"),
          Seq(Body(
            Yield(Eval(Scala(q"Zero")))
          ))
        )
      )
    )
    testModuleSucceeds(module1)

    val module2 = Module(Name("Test"), Seq(),
      Seq(
        ScalaModuleContent(Scala(q"trait Nat")),
        ScalaModuleContent(Scala(q"case object Zero extends Nat")),
        ScalaModuleContent(Scala(q"case class Succ(pred: Nat) extends Nat")),
        PatternFunction(None, Name("testTwo"), Seq(), TScala("Nat"),
          Seq(Body(
            Yield(Eval(Scala(q"Succ(Zero)")))
          ))
        )
      )
    )
    testModuleSucceeds(module2)

    val module3 = Module(Name("Test"), Seq(),
      Seq(
        PatternFunction(None, Name("testTwo"), Seq(), TScala("inca.analyzedData.Nat.Nat"),
          Seq(Body(
            Yield(Eval(Scala(q"inca.analyzedData.Nat.Succ(inca.analyzedData.Nat.Zero)")))
          ))
        )
      )
    )
    testModuleSucceeds(module3)

    val module4 = Module(Name("Test"), Seq(),
      Seq(
        ScalaModuleContent(Scala(q"trait Nat")),
        ScalaModuleContent(Scala(q"case object Zero extends Nat")),
        ScalaModuleContent(Scala(q"case class Succ(pred: Nat) extends Nat")),
        ScalaModuleContent(Scala(q"val succ: Nat = Succ(Zero)")),
        PatternFunction(None, Name("testTwo"), Seq(), TScala("Nat"),
          Seq(Body(
            Yield(Eval(Scala(q"succ")))
          ))
        )
      )
    )
    testModuleSucceeds(module4)

    val module5 = Module(Name("Test"), Seq(),
      Seq(
        ScalaModuleContent(Scala(q"trait Nat")),
        ScalaModuleContent(Scala(q"case object Zero extends Nat")),
        ScalaModuleContent(Scala(q"case class Succ(pred: Nat) extends Nat")),
        ScalaModuleContent(Scala(q"val (succ, succsucc) = (Succ(Zero), Succ(Succ(Zero)))")),
        PatternFunction(None, Name("testTwo"), Seq(), TScala("Succ"),
          Seq(Body(
            Yield(Eval(Scala(q"succsucc")))
          ))
        )
      )
    )
    testModuleSucceeds(module5)

    val moduleFail = Module(Name("Test"), Seq(),
      Seq(
        PatternFunction(None, Name("test"), Seq(), TScala("Nat"),
          Seq(Body(
            Yield(Eval(Scala(q"Zero")))
          ))
        )
      )
    )
    testModuleFail(moduleFail)
  }

  "checkModules" should "type modules with val defs correctly" in {
    def testModule(mod: Module) = assertTypecheckModulesSucceed(Seq(mod))
    def testModuleFail(mod: Module) = assertTypecheckModulesFail(Seq(mod))

    testModule(
      Module(
        Name("my"),
        Seq(),
        Seq(ValDef(None, Name("x"), None, Constant(IntLiteral(1))))
      )
    )
    testModule(
      Module(
        Name("my"),
        Seq(),
        Seq(ValDef(None, Name("x"), Some(TScalaInt), Constant(IntLiteral(1))))
      )
    )
    testModule(
      Module(
        Name("my"),
        Seq(),
        Seq(
          ValDef(None, Name("x"), Some(TScalaInt), Constant(IntLiteral(1))),
          ValDef(None, Name("y"), None, Var(Name("x")))
        )
      )
    )
    testModule(
      Module(
        Name("my"),
        Seq(),
        Seq(
          ValDef(None, Name("x"), Some(TScalaInt), Constant(IntLiteral(1))),
          ValDef(None, Name("y"), Some(TScalaInt), Var(Name("x")))
        )
      )
    )
    testModuleFail(
      Module(
        Name("my"),
        Seq(),
        Seq(
          ValDef(None, Name("x"), Some(TScalaInt), Constant(IntLiteral(1))),
          ValDef(None, Name("y"), Some(TScalaBoolean), Var(Name("x")))
        )
      )
    )
    testModuleFail(
      Module(
        Name("my"),
        Seq(),
        Seq(
          ValDef(None, Name("y"), None, Var(Name("x"))),
          ValDef(None, Name("x"), Some(TScalaInt), Constant(IntLiteral(1)))
        )
      )
    )
  }
}
