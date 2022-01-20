package inca.frontend.constraint.core

import inca.analyzedLangs.Exp
import inca.analyzedLangs.Exp._
import inca.analyzedLangs.ExpLangTestAnalyses._
import inca.backend.ir.{Datalog, GPPrinter}
import inca.compiler.Compiler
import inca.frontend.constraint.compiler.ConstraintOptions
import org.scalatest.funsuite.AnyFunSuite

class GenerateDatalogTest extends AnyFunSuite {

  def compileToGP(module: Module): Datalog.Module =
    Compiler.compileConstraint(module, ConstraintOptions()).ir
  
  test("simple function pattern with return constraint"){
    val result = compileToGP(Module("test", Seq(DirectDataModel(Exp.model)), Nil, Nil, Seq(idFun)))
    println(GPPrinter.prettyModule(result))
  }

  test("simple function pattern with return constraint containg path expression"){
    val result = compileToGP(Module("test", Seq(DirectDataModel(Exp.model)), Nil, Nil, Seq(lhChildFun)))
    println(GPPrinter.prettyModule(result))
  }

  test("simple function pattern with equality against literal"){
    val addType = TNode(classOf[Add].getCanonicalName)
    val expType = TNode(classOf[Exp].getCanonicalName)
    val lhsLink = addType("lhs")
    val fun = PatternFunction(
      Seq(),
      None,
      "id",
      List(Param("add", addType)),
      expType,
      List(
        Body(
          List(
            Assert(Eq(
              PathAccess(
                Cast(PathAccess(
                  Cast(PathAccess(
                    Var("add"),
                    lhsLink), addType),
                  lhsLink), addType),
                lhsLink),
              Var("add"))),
            Yield(Var("add"))))))
    val result = compileToGP(Module("test", Seq(DirectDataModel(Exp.model)), Nil, Nil, Seq(fun)))
    println(GPPrinter.prettyModule(result))
  }

  test("simple function pattern with return constraint containg path expression 2"){
    val addType = TNode(classOf[Add].getCanonicalName)
    val expType = TNode(classOf[Exp].getCanonicalName)
    val lhsLink = addType("lhs")
    val fun = PatternFunction(
      Seq(),
      None,
      "id",
      List(Param("add", addType)),
      expType,
      List(
        Body(
          List(
            Yield(PathAccess(
              Cast(PathAccess(
                Cast(PathAccess(
                  Var("add"),
                  lhsLink), addType),
                lhsLink), addType),
              lhsLink))))))
    val result = compileToGP(Module("test", Seq(DirectDataModel(Exp.model)), Nil, Nil, Seq(fun)))
    println(GPPrinter.prettyModule(result))
  }

  test("multiple bodies function pattern with return constraint containg path expression"){
    val result = compileToGP(Module("test", Seq(DirectDataModel(Exp.model)), Nil, Nil, Seq(childrenFun)))
    println(GPPrinter.prettyModule(result))
  }

  test("function pattern calling other function"){
    val addType = TNode(classOf[Add].getCanonicalName)
    val expType = TNode(classOf[Exp].getCanonicalName)
    val lhsLink = addType("lhs")
    val rhsLink = addType("rhs")
    val fun = PatternFunction(
      Seq(),
      None,
      "test",
      List(Param("add", addType)),
      expType,
      List(
        Body(
          List(
            Assign(Seq("lhschild"), Call("lhChild", Seq(Var("add")))),
            Yield(Var("lhschild"))))))
    val result = compileToGP(Module("test", Seq(DirectDataModel(Exp.model)), Nil, Nil, Seq(fun, lhChildFun)))
    println(GPPrinter.prettyModule(result))
  }

  test("function pattern using instance of"){
    val addType = TNode(classOf[Add].getCanonicalName)
    val expType = TNode(classOf[Exp].getCanonicalName)
    val lhsLink = addType("lhs")
    val rhsLink = addType("rhs")
    val fun = PatternFunction(
      Seq(),
      None,
      "test",
      List(Param("add", addType)),
      expType,
      List(
        Body(
          List(
            Assign(Seq("lhschild"), PathAccess(Var("add"), lhsLink)),
            Assert(InstanceOf(Var("lhschild"), addType)),
            Yield(Var("lhschild"))))))
    val result = compileToGP(Module("test", Seq(DirectDataModel(Exp.model)), Nil, Nil, Seq(fun)))
    println(GPPrinter.prettyModule(result))
  }

  test("function pattern using notinstance of"){
    val addType = TNode(classOf[Add].getCanonicalName)
    val expType = TNode(classOf[Exp].getCanonicalName)
    val lhsLink = addType("lhs")
    val rhsLink = addType("rhs")
    val fun = PatternFunction(
      Seq(),
      None,
      "test",
      List(Param("add", addType)),
      expType,
      List(
        Body(
          List(
            Assign(Seq("lhschild"), PathAccess(Var("add"), lhsLink)),
            Assert(NotInstanceOf(Var("lhschild"), addType)),
            Yield(Var("lhschild"))))))
    val result = compileToGP(Module("test", Seq(DirectDataModel(Exp.model)), Nil, Nil, Seq(fun)))
    println(GPPrinter.prettyModule(result))
  }

  test("function pattern def of call"){
    val addType = TNode(classOf[Add].getCanonicalName)
    val expType = TNode(classOf[Exp].getCanonicalName)
    val lhsLink = addType("lhs")
    val rhsLink = addType("rhs")
    val fun = PatternFunction(
      Seq(),
      None,
      "test",
      List(Param("add", addType)),
      expType,
      List(
        Body(
          List(
            Assign(Seq("lhschild"), PathAccess(Var("add"), lhsLink)),
            Assert(Def(Call("lhChild", Seq(Var("lhschild"))))),
            Yield(Var("lhschild"))))))
    val result = compileToGP(Module("test", Seq(DirectDataModel(Exp.model)), Nil, Nil, Seq(fun, lhChildFun)))
    println(GPPrinter.prettyModule(result))
  }

  test("function pattern def of path"){
    val addType = TNode(classOf[Add].getCanonicalName)
    val expType = TNode(classOf[Exp].getCanonicalName)
    val lhsLink = addType("lhs")
    val rhsLink = addType("rhs")
    val fun = PatternFunction(
      Seq(),
      None,
      "test",
      List(Param("add", addType)),
      TUnit,
      List(
        Body(
          List(
            Assert(Def(PathAccess(Var("add"), lhsLink)))))))
    val result = compileToGP(Module("test", Seq(DirectDataModel(Exp.model)), Nil, Nil, Seq(fun)))
    println(GPPrinter.prettyModule(result))
  }

  test("function pattern undef of path"){
    val addType = TNode(classOf[Add].getCanonicalName)
    val lhsLink = addType("lhs")
    val fun = PatternFunction(
      Seq(),
      None,
      "test",
      List(Param("add", addType)),
      TUnit,
      List(
        Body(
          List(
            Assert(Undef(PathAccess(Var("add"), lhsLink)))))))
    val result = compileToGP(Module("test", Seq(DirectDataModel(Exp.model)), Nil, Nil, Seq(fun)))
    println(GPPrinter.prettyModule(result))
  }

  test("parameter without type"){
    val result = compileToGP(Module("test", Seq(DirectDataModel(Exp.model)), Nil, Nil, Seq(noParamTypeFun)))
    println(GPPrinter.prettyModule(result))
  }

  test("parameter with primitive type"){
    val result = compileToGP(Module("test", Seq(DirectDataModel(Exp.model)), Nil, Nil, Seq(primitiveParamFun)))
    println(GPPrinter.prettyModule(result))
  }

  test("modules with val defs") {
    def testModule(mod: Module) = println(compileToGP(mod))

    testModule(
      Module(
        Name("my"),
        Seq(DirectDataModel(Exp.model)),
        Seq(),
        Seq(),
        Seq(ValDef(None, Name("x"), None, Constant(IntLiteral(1))))
      )
    )
    testModule(
      Module(
        Name("my"),
        Seq(DirectDataModel(Exp.model)),
        Seq(),
        Seq(),
        Seq(ValDef(None, Name("x"), Some(TScalaInt), Constant(IntLiteral(1))))
      )
    )
    testModule(
      Module(
        Name("my"),
        Seq(DirectDataModel(Exp.model)),
        Seq(),
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
        Seq(DirectDataModel(Exp.model)),
        Seq(),
        Seq(),
        Seq(
          ValDef(None, Name("x"), Some(TScalaInt), Constant(IntLiteral(1))),
          PatternFunction(Seq(), None, Name("foo"), Seq(), TScalaInt, Seq(
            Body(Seq(Yield(Var("x"))))
          ))
        )
      )
    )
  }
}
