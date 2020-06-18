package inca.trans.fun

import inca.analyzedLangs.expLang._
import inca.lang.FunLang.{Alternative, AnnoParam, Assert, Assignment, BooleanLiteral, Call, Constant, Def, Eq, InstanceOf, Module, NotInstanceOf, Param, PathAccess, PatternCall, PatternFunction, Return, TBool, Undef, Var}
import inca.MetaElements.Node
import inca.print.GraphPatternLangPrinter
import inca.trans.fun.FunToGPTranslator
import inca.trans.gp.GPToPSystemTranslator
import inca.trans.ExpLangTestAnalyses._
import org.scalatest.funsuite.AnyFunSuite

class FunToGPTranslatorTest extends AnyFunSuite {

  test("simple function pattern with return constraint"){
    val result = FunToGPTranslator.transformModule(Module("test", Nil, Seq(idFun)))
    println(GraphPatternLangPrinter.prettyModule(result))
  }

  test("simple function pattern with return constraint containg path expression"){
    val result = FunToGPTranslator.transformModule(Module("test", Nil, Seq(lhChildFun)))
    println(GraphPatternLangPrinter.prettyModule(result))
  }

  test("simple function pattern with equality against literal"){
    val addType = Node(classOf[Add].getCanonicalName)
    val expType = Node(classOf[Exp].getCanonicalName)
    val lhsLink = addType("lhs")
    val fun = PatternFunction(
      None,
      "id",
      List(Param("add", Some(addType))),
      List(AnnoParam(None, expType)),
      List(
        Alternative(
          List(
            Assert(Eq(PathAccess(Var("add"), Seq(lhsLink, lhsLink, lhsLink)), Constant(BooleanLiteral(true)))),
            Return(Var("add"))))))
    val result = FunToGPTranslator.transformModule(Module("test", Nil, Seq(fun)))
    println(GraphPatternLangPrinter.prettyModule(result))
  }

  test("simple function pattern with return constraint containg path expression 2"){
    val addType = Node(classOf[Add].getCanonicalName)
    val expType = Node(classOf[Exp].getCanonicalName)
    val lhsLink = addType("lhs")
    val fun = PatternFunction(
      None,
      "id",
      List(Param("add", Some(addType))),
      List(AnnoParam(None, expType)),
      List(
        Alternative(
          List(
            Return(PathAccess(Var("add"), Seq(lhsLink, lhsLink, lhsLink)))))))
    val result = FunToGPTranslator.transformModule(Module("test", Nil, Seq(fun)))
    println(GraphPatternLangPrinter.prettyModule(result))
  }

  test("multiple bodies function pattern with return constraint containg path expression"){
    val result = FunToGPTranslator.transformModule(Module("test", Nil, Seq(childrenFun)))
    println(GraphPatternLangPrinter.prettyModule(result))
  }

  test("function pattern calling other function"){
    val addType = Node(classOf[Add].getCanonicalName)
    val expType = Node(classOf[Exp].getCanonicalName)
    val lhsLink = addType("lhs")
    val rhsLink = addType("rhs")
    val fun = PatternFunction(
      None,
      "test",
      List(Param("add", Some(addType))),
      List(AnnoParam(None, expType)),
      List(
        Alternative(
          List(
            Assignment(Seq("lhschild"), Call(PatternCall("lhChild", Seq(Var("add")), transitive = false), count = false)),
            Return(Var("lhschild"))))))
    val result = FunToGPTranslator.transformModule(Module("test", Nil, Seq(fun, lhChildFun)))
    println(GraphPatternLangPrinter.prettyModule(result))
  }

  test("function pattern using instance of"){
    val addType = Node(classOf[Add].getCanonicalName)
    val expType = Node(classOf[Exp].getCanonicalName)
    val lhsLink = addType("lhs")
    val rhsLink = addType("rhs")
    val fun = PatternFunction(
      None,
      "test",
      List(Param("add", Some(addType))),
      List(AnnoParam(Some("out"), expType)),
      List(
        Alternative(
          List(
            Assignment(Seq("lhschild"), PathAccess(Var("add"), Seq(lhsLink))),
            Assert(InstanceOf(Var("lhschild"), addType)),
            Return(Var("lhschild"))))))
    val result = FunToGPTranslator.transformModule(Module("test", Nil, Seq(fun)))
    println(GraphPatternLangPrinter.prettyModule(result))
  }

  test("function pattern using notinstance of"){
    val addType = Node(classOf[Add].getCanonicalName)
    val expType = Node(classOf[Exp].getCanonicalName)
    val lhsLink = addType("lhs")
    val rhsLink = addType("rhs")
    val fun = PatternFunction(
      None,
      "test",
      List(Param("add", Some(addType))),
      List(AnnoParam(None, expType)),
      List(
        Alternative(
          List(
            Assignment(Seq("lhschild"), PathAccess(Var("add"), Seq(lhsLink))),
            Assert(NotInstanceOf(Var("lhschild"), addType)),
            Return(Var("lhschild"))))))
    val result = FunToGPTranslator.transformModule(Module("test", Nil, Seq(fun)))
    println(GraphPatternLangPrinter.prettyModule(result))
  }

  test("function pattern def of call"){
    val addType = Node(classOf[Add].getCanonicalName)
    val expType = Node(classOf[Exp].getCanonicalName)
    val lhsLink = addType("lhs")
    val rhsLink = addType("rhs")
    val fun = PatternFunction(
      None,
      "test",
      List(Param("add", Some(addType))),
      List(AnnoParam(None, expType)),
      List(
        Alternative(
          List(
            Assignment(Seq("lhschild"), PathAccess(Var("add"), Seq(lhsLink))),
            Assert(Def(Call(PatternCall("lhChild", Seq(Var("lhschild")), transitive = false), count = false))),
            Return(Var("lhschild"))))))
    val result = FunToGPTranslator.transformModule(Module("test", Nil, Seq(fun, lhChildFun)))
    println(GraphPatternLangPrinter.prettyModule(result))
  }

  test("function pattern def of path"){
    val addType = Node(classOf[Add].getCanonicalName)
    val expType = Node(classOf[Exp].getCanonicalName)
    val lhsLink = addType("lhs")
    val rhsLink = addType("rhs")
    val fun = PatternFunction(
      None,
      "test",
      List(Param("add", Some(addType))),
      List(),
      List(
        Alternative(
          List(
            Assert(Def(PathAccess(Var("add"), Seq(lhsLink))))))))
    val result = FunToGPTranslator.transformModule(Module("test", Nil, Seq(fun)))
    println(GraphPatternLangPrinter.prettyModule(result))
  }

  test("function pattern undef of path"){
    val addType = Node(classOf[Add].getCanonicalName)
    val expType = Node(classOf[Exp].getCanonicalName)
    val lhsLink = addType("lhs")
    val rhsLink = addType("rhs")
    val fun = PatternFunction(
      None,
      "test",
      List(Param("add", Some(addType))),
      List(),
      List(
        Alternative(
          List(
            Assert(Undef(PathAccess(Var("add"), Seq(lhsLink))))))))
    val result = FunToGPTranslator.transformModule(Module("test", Nil, Seq(fun)))
    println(GraphPatternLangPrinter.prettyModule(result))
  }

  test("parameter without type"){
    val addType = Node(classOf[Add].getCanonicalName)
    val expType = Node(classOf[Exp].getCanonicalName)
    val lhsLink = addType("lhs")
    val rhsLink = addType("rhs")
    val result = FunToGPTranslator.transformModule(Module("test", Nil, Seq(noParamTypeFun)))
    println(GraphPatternLangPrinter.prettyModule(result))
  }

  test("parameter with primitive type"){
    val addType = Node(classOf[Add].getCanonicalName)
    val expType = Node(classOf[Exp].getCanonicalName)
    val lhsLink = addType("lhs")
    val rhsLink = addType("rhs")
    val result = FunToGPTranslator.transformModule(Module("test", Nil, Seq(primitiveParamFun)))
    println(GraphPatternLangPrinter.prettyModule(result))
  }
}
