package inca.trans.fun

import inca.analyzedLangs.expLang._
import inca.lang.fun.CompileToGP
import inca.lang.fun.Fun.{Exp => _, _}
import inca.lang.gp.Printer
import inca.trans.ExpLangTestAnalyses._
import org.scalatest.funsuite.AnyFunSuite

class CompileToGPTest extends AnyFunSuite {

  test("simple function pattern with return constraint"){
    val result = CompileToGP.transformModule(Module("test", Nil, Seq(idFun)))
    println(Printer.prettyModule(result))
  }

  test("simple function pattern with return constraint containg path expression"){
    val result = CompileToGP.transformModule(Module("test", Nil, Seq(lhChildFun)))
    println(Printer.prettyModule(result))
  }

  test("simple function pattern with equality against literal"){
    val addType = TNode(classOf[Add].getCanonicalName)
    val expType = TNode(classOf[Exp].getCanonicalName)
    val lhsLink = addType("lhs")
    val fun = PatternFunction(
      None,
      "id",
      List(Param("add", Some(addType))),
      List(AnnoParam(None, expType)),
      List(
        Body(
          List(
            Assert(Eq(PathAccess(Var("add"), Seq(lhsLink, lhsLink, lhsLink)), Constant(BooleanLiteral(true)))),
            Return(Var("add"))))))
    val result = CompileToGP.transformModule(Module("test", Nil, Seq(fun)))
    println(Printer.prettyModule(result))
  }

  test("simple function pattern with return constraint containg path expression 2"){
    val addType = TNode(classOf[Add].getCanonicalName)
    val expType = TNode(classOf[Exp].getCanonicalName)
    val lhsLink = addType("lhs")
    val fun = PatternFunction(
      None,
      "id",
      List(Param("add", Some(addType))),
      List(AnnoParam(None, expType)),
      List(
        Body(
          List(
            Return(PathAccess(Var("add"), Seq(lhsLink, lhsLink, lhsLink)))))))
    val result = CompileToGP.transformModule(Module("test", Nil, Seq(fun)))
    println(Printer.prettyModule(result))
  }

  test("multiple bodies function pattern with return constraint containg path expression"){
    val result = CompileToGP.transformModule(Module("test", Nil, Seq(childrenFun)))
    println(Printer.prettyModule(result))
  }

  test("function pattern calling other function"){
    val addType = TNode(classOf[Add].getCanonicalName)
    val expType = TNode(classOf[Exp].getCanonicalName)
    val lhsLink = addType("lhs")
    val rhsLink = addType("rhs")
    val fun = PatternFunction(
      None,
      "test",
      List(Param("add", Some(addType))),
      List(AnnoParam(None, expType)),
      List(
        Body(
          List(
            Assignment(Seq("lhschild"), Call("lhChild", Seq(Var("add")), transitive = false, count = false)),
            Return(Var("lhschild"))))))
    val result = CompileToGP.transformModule(Module("test", Nil, Seq(fun, lhChildFun)))
    println(Printer.prettyModule(result))
  }

  test("function pattern using instance of"){
    val addType = TNode(classOf[Add].getCanonicalName)
    val expType = TNode(classOf[Exp].getCanonicalName)
    val lhsLink = addType("lhs")
    val rhsLink = addType("rhs")
    val fun = PatternFunction(
      None,
      "test",
      List(Param("add", Some(addType))),
      List(AnnoParam(Some("out"), expType)),
      List(
        Body(
          List(
            Assignment(Seq("lhschild"), PathAccess(Var("add"), Seq(lhsLink))),
            Assert(InstanceOf(Var("lhschild"), addType)),
            Return(Var("lhschild"))))))
    val result = CompileToGP.transformModule(Module("test", Nil, Seq(fun)))
    println(Printer.prettyModule(result))
  }

  test("function pattern using notinstance of"){
    val addType = TNode(classOf[Add].getCanonicalName)
    val expType = TNode(classOf[Exp].getCanonicalName)
    val lhsLink = addType("lhs")
    val rhsLink = addType("rhs")
    val fun = PatternFunction(
      None,
      "test",
      List(Param("add", Some(addType))),
      List(AnnoParam(None, expType)),
      List(
        Body(
          List(
            Assignment(Seq("lhschild"), PathAccess(Var("add"), Seq(lhsLink))),
            Assert(NotInstanceOf(Var("lhschild"), addType)),
            Return(Var("lhschild"))))))
    val result = CompileToGP.transformModule(Module("test", Nil, Seq(fun)))
    println(Printer.prettyModule(result))
  }

  test("function pattern def of call"){
    val addType = TNode(classOf[Add].getCanonicalName)
    val expType = TNode(classOf[Exp].getCanonicalName)
    val lhsLink = addType("lhs")
    val rhsLink = addType("rhs")
    val fun = PatternFunction(
      None,
      "test",
      List(Param("add", Some(addType))),
      List(AnnoParam(None, expType)),
      List(
        Body(
          List(
            Assignment(Seq("lhschild"), PathAccess(Var("add"), Seq(lhsLink))),
            Assert(Def(Call("lhChild", Seq(Var("lhschild")), transitive = false, count = false))),
            Return(Var("lhschild"))))))
    val result = CompileToGP.transformModule(Module("test", Nil, Seq(fun, lhChildFun)))
    println(Printer.prettyModule(result))
  }

  test("function pattern def of path"){
    val addType = TNode(classOf[Add].getCanonicalName)
    val expType = TNode(classOf[Exp].getCanonicalName)
    val lhsLink = addType("lhs")
    val rhsLink = addType("rhs")
    val fun = PatternFunction(
      None,
      "test",
      List(Param("add", Some(addType))),
      List(),
      List(
        Body(
          List(
            Assert(Def(PathAccess(Var("add"), Seq(lhsLink))))))))
    val result = CompileToGP.transformModule(Module("test", Nil, Seq(fun)))
    println(Printer.prettyModule(result))
  }

  test("function pattern undef of path"){
    val addType = TNode(classOf[Add].getCanonicalName)
    val expType = TNode(classOf[Exp].getCanonicalName)
    val lhsLink = addType("lhs")
    val rhsLink = addType("rhs")
    val fun = PatternFunction(
      None,
      "test",
      List(Param("add", Some(addType))),
      List(),
      List(
        Body(
          List(
            Assert(Undef(PathAccess(Var("add"), Seq(lhsLink))))))))
    val result = CompileToGP.transformModule(Module("test", Nil, Seq(fun)))
    println(Printer.prettyModule(result))
  }

  test("parameter without type"){
    val addType = TNode(classOf[Add].getCanonicalName)
    val expType = TNode(classOf[Exp].getCanonicalName)
    val lhsLink = addType("lhs")
    val rhsLink = addType("rhs")
    val result = CompileToGP.transformModule(Module("test", Nil, Seq(noParamTypeFun)))
    println(Printer.prettyModule(result))
  }

  test("parameter with primitive type"){
    val addType = TNode(classOf[Add].getCanonicalName)
    val expType = TNode(classOf[Exp].getCanonicalName)
    val lhsLink = addType("lhs")
    val rhsLink = addType("rhs")
    val result = CompileToGP.transformModule(Module("test", Nil, Seq(primitiveParamFun)))
    println(Printer.prettyModule(result))
  }
}
