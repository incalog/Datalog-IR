package org.inca.trans.fun

import org.inca.analyzedLangs.expLang.{Add, Exp}
import org.inca.lang.FunLang.{Alternative, AnnoParam, Assert, Assignment, BooleanLiteral, Call, Constant, Def, Eq, InstanceOf, Module, NotInstanceOf, Param, PathAccess, PatternCall, PatternFunction, Return, Undef, Var}
import org.inca.meta.MetaElements.NodeType
import org.inca.print.GraphPatternLangPrinter
import org.inca.trans.ExpLangTestAnalyses._
import org.inca.trans.gp.GPToPSystemTranslator
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
    val addType = NodeType(classOf[Add])
    val expType = NodeType(classOf[Exp])
    val lhsLink = addType("lhs")
    val fun = PatternFunction(
      None,
      "id",
      List(Param("add", Some(addType))),
      List(AnnoParam("out", expType)),
      List(
        Alternative(
          List(
            Assert(Eq(PathAccess(Var("add"), Seq(lhsLink, lhsLink, lhsLink)), Constant(BooleanLiteral(true)))),
            Return(Var("add"))))))
    val result = FunToGPTranslator.transformModule(Module("test", Nil, Seq(fun)))
    println(GraphPatternLangPrinter.prettyModule(result))
  }

  test("simple function pattern with return constraint containg path expression 2"){
    val addType = NodeType(classOf[Add])
    val expType = NodeType(classOf[Exp])
    val lhsLink = addType("lhs")
    val fun = PatternFunction(
      None,
      "id",
      List(Param("add", Some(addType))),
      List(AnnoParam("out", expType)),
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
    val addType = NodeType(classOf[Add])
    val expType = NodeType(classOf[Exp])
    val lhsLink = addType("lhs")
    val rhsLink = addType("rhs")
    val fun = PatternFunction(
      None,
      "test",
      List(Param("add", Some(addType))),
      List(AnnoParam("out", expType)),
      List(
        Alternative(
          List(
            Assignment(Seq("lhschild"), Call(PatternCall("lhChild", Seq(Var("add")), transitive = false), count = false)),
            Return(Var("lhschild"))))))
    val result = FunToGPTranslator.transformModule(Module("test", Nil, Seq(fun, lhChildFun)))
    println(GraphPatternLangPrinter.prettyModule(result))
  }

  test("function pattern using instance of"){
    val addType = NodeType(classOf[Add])
    val expType = NodeType(classOf[Exp])
    val lhsLink = addType("lhs")
    val rhsLink = addType("rhs")
    val fun = PatternFunction(
      None,
      "test",
      List(Param("add", Some(addType))),
      List(AnnoParam("out", expType)),
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
    val addType = NodeType(classOf[Add])
    val expType = NodeType(classOf[Exp])
    val lhsLink = addType("lhs")
    val rhsLink = addType("rhs")
    val fun = PatternFunction(
      None,
      "test",
      List(Param("add", Some(addType))),
      List(AnnoParam("out", expType)),
      List(
        Alternative(
          List(
            Assignment(Seq("lhschild"), PathAccess(Var("add"), Seq(lhsLink))),
            Assert(NotInstanceOf(Var("lhschild"), addType)),
            Return(Var("lhschild"))))))
    val result = FunToGPTranslator.transformModule(Module("test", Nil, Seq(fun)))
    val translator = new GPToPSystemTranslator(Seq(result))
    println(GraphPatternLangPrinter.prettyModule(result))
    println(translator.transAnalysis())
  }

  test("function pattern def of call"){
    val addType = NodeType(classOf[Add])
    val expType = NodeType(classOf[Exp])
    val lhsLink = addType("lhs")
    val rhsLink = addType("rhs")
    val fun = PatternFunction(
      None,
      "test",
      List(Param("add", Some(addType))),
      List(AnnoParam("out", expType)),
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
    val addType = NodeType(classOf[Add])
    val expType = NodeType(classOf[Exp])
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
    val addType = NodeType(classOf[Add])
    val expType = NodeType(classOf[Exp])
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
    val translator = new GPToPSystemTranslator(Seq(result))
    println(GraphPatternLangPrinter.prettyModule(result))
    println(translator.transAnalysis())
  }
}
