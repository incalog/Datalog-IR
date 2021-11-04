package inca.frontend.functionaxsouffle

import inca.frontend.functional.compiler.FunctionalOptions
import inca.frontend.functionalxsouffle.executor.FunctionalXSouffleExecutor
import org.scalatest.funsuite.AnyFunSuite

import scala.io.{BufferedSource, Source}
import scala.meta.{Lit, XtensionQuasiquoteTerm}

class CloneDetectionTest extends AnyFunSuite {

  def module(contents: String*): String = contents.mkString("\n")

  def readFile(path: String): String = {
    val src: BufferedSource = Source.fromFile(path)
    val code: String = src.getLines().mkString("\n")
    src.close()
    code
  }

  val baseDir = s"souffle-frontend/doop-context-insensitive"
  val souffleCode: String = readFile(s"$baseDir/souffle-input-schema.dl")
  val funCode: String = readFile(s"$baseDir/clone-detection.fun")

  val assignExpMain: String = module(funCode, "@main def main(v: String): Set[Exp] = { exp | exp in assignExp(v) }")
  val genStmMain: String = module(funCode, "@main def main(v: String): Set[Stm] = { stm | stm in genStm(v) }")
  val getStmListMain: String = module(funCode, "@main def main(meth: String): Set[StmList] = getStmList(meth)")

  def testAssignExp(dir: String, name: meta.Term, expected: meta.Term): Unit = {
    val fun = FunctionalXSouffleExecutor.loadFunction(assignExpMain, souffleCode)
    val res = fun.execute("main", Seq(name), s"$baseDir/$dir", false)
    assert(res == fun.result(expected))
  }

  def testGenStm(dir: String, name: meta.Term, expected: meta.Term): Unit = {
    val fun = FunctionalXSouffleExecutor.loadFunction(genStmMain, souffleCode)
    val res = fun.execute("main", Seq(name), s"$baseDir/$dir", false)
    assert(res == fun.result(expected))
  }

  def testGetStmList(dir: String, name: meta.Term, expected: Option[meta.Term]): Unit = {
    val opts = FunctionalOptions()
    val fun = FunctionalXSouffleExecutor.loadFunction(getStmListMain, souffleCode, opts)
    val res = fun.execute("main", Seq(name), s"$baseDir/$dir", false)
    expected match {
      case Some(t)  =>
        assert(res == fun.result(t))
      case None =>
        assert(res.res.nonEmpty)
    }
  }


  /**
   * Construct Expressions
   */
  test("simple addition example") {
    testAssignExp(
      "database-simple-add",
      q""""<Main: void main(java.lang.String[])>/y#_4"""",
      q"""BinOp("+", NumLit("3"), NumLit("5"))""")
  }

  test("nested addition example") {
    testAssignExp(
      "database-nested-add",
      q""""<Main: void main(java.lang.String[])>/z#_5"""",
      q"""BinOp("+", BinOp("+", NumLit("3"), NumLit("5")), BinOp("+", NumLit("3"), NumLit("5")))""")
  }

  test("cast example") {
    testAssignExp(
      "database-cast",
      q""""<Main: void main(java.lang.String[])>/l2#_4"""",
      q"""Cast(NumLit("12"), "long")""")
  }

  test("instanceof example") {
    testAssignExp(
      "database-instanceof",
      q""""<Main: void main(java.lang.String[])>/l2#_4"""",
      q"""InstanceOf(Alloc("12", "java.lang.String"), "java.lang.String")""")
  }

  test("simple unop")  {
    testAssignExp(
      "database-unop",
      q""""<Main: void main(java.lang.String[])>/l1#_3"""",
      q"""UnOp("len", Var("<Main: void main(java.lang.String[])>/@parameter0"))""")
  }

  test("array read with int") {
    testAssignExp(
      "database-array-read-int",
      q""""<Main: void main(java.lang.String[])>/l1#_3"""",
      q"""ArrayRead(Var("<Main: void main(java.lang.String[])>/@parameter0"), NumLit("12"))""")
  }

  test("array read with complex expression") {
    testAssignExp(
      "database-array-read-var",
      q""""<Main: void main(java.lang.String[])>/l4#_6"""",
      q"""ArrayRead(Var("<Main: void main(java.lang.String[])>/@parameter0"), BinOp("+", BinOp("+", NumLit("1"), NumLit("4")), NumLit("2")))""")
  }

  test("string length method call example") {
    testAssignExp(
      "database-string-length",
      q""""<Main: void main(java.lang.String[])>/l1#_3"""",
      q"""Invoke(ArrayRead(Var("<Main: void main(java.lang.String[])>/@parameter0"), NumLit("0")), "<java.lang.String: int length()>", NilArg())""")
  }

  test("method call example") {
    testAssignExp(
      "database-method-call",
      q""""<Main: void main(java.lang.String[])>/l3#_5"""",
      q"""BinOp("+", Invoke(Alloc("12", "java.lang.String"), "<java.lang.String: int length()>", NilArg()), NumLit("1"))""")
  }

  test("method call with multiple args example") {
    testAssignExp(
      "database-multiple-arg-method-call",
      q""""<Main: void main(java.lang.String[])>/l4#_7"""",
      q"""Invoke(SpecialInvoke("<Main: void main(java.lang.String[])>/new Point/0", "<Point: void <init>(int,int)>", ConsArg(NumLit("1"), ConsArg(NumLit("2"), NilArg()))), "<Point: Point add(int,int)>", ConsArg(NumLit("10"), ConsArg(NumLit("12"), NilArg())))""")
  }

  test("instance field read access ") {
    testAssignExp(
      "database-instance-field-read",
      q""""<Main: void main(java.lang.String[])>/l2#_5"""",
      q"""InstanceFieldRead(SpecialInvoke("<Main: void main(java.lang.String[])>/new Point/0", "<Point: void <init>(int,int)>", ConsArg(NumLit("1"), ConsArg(NumLit("2"), NilArg()))), "<Point: int x>")""")
  }

  test("static field read access ") {
    testAssignExp(
      "database-static-field-read",
      q""""<Main: void main(java.lang.String[])>/l1#_4"""",
      q"""StaticFieldRead("<Point: java.lang.String TY>")""")
  }

  test("static method call") {
    testAssignExp(
      "database-static-method-call",
      q""""<Main: void main(java.lang.String[])>/l1#_4"""",
      q"""StaticInvoke("<Point: Point genPoint(int,int)>", ConsArg(NumLit("1"), ConsArg(NumLit("2"), NilArg())))"""
    )
  }

  test("method call with null argument") {
    testAssignExp(
      "database-null-argument",
      q""""<Main: void main(java.lang.String[])>/l2#_5"""",
      q"""Invoke(StaticInvoke("<Point: Point genPoint(int,int)>", ConsArg(NumLit("1"), ConsArg(NumLit("2"), NilArg()))), "<Point: Point add(Point)>", ConsArg(Null(), NilArg()))""")
  }

  test("constructor call") {
    testAssignExp(
      "database-constructor-call",
      q""""<Main: void main(java.lang.String[])>/l1#_4"""",
      q"""SpecialInvoke("<Main: void main(java.lang.String[])>/new Point/0", "<Point: void <init>(int,int)>", ConsArg(NumLit("1"), ConsArg(NumLit("2"), NilArg())))""")
  }

  test("dynamic invocation (lambda)") {
    val varName = Lit.String("<Main: void main(java.lang.String[])>/$stack5")

    val bootMeth = Lit.String("<java.lang.invoke.LambdaMetafactory: java.lang.invoke.CallSite metafactory(java.lang.invoke.MethodHandles$Lookup,java.lang.String,java.lang.invoke.MethodType,java.lang.invoke.MethodType,java.lang.invoke.MethodHandle,java.lang.invoke.MethodType)>")
    val methName = Lit.String("accept")
    val expected = q"DynamicInvoke($bootMeth, $methName,  NilArg())"

    testAssignExp(
      "database-dynamic-invoke",
      varName,
      expected)
  }

  // PhantomInvoke(Exp, String) Phantom invocations are invocations of methods belonging to phantom classes. Phantom classes are classes not part of the analyzed jar
  // Phantom method calls cannot be recovered because the database does not store them
  // The database only lists phantom types and phantom methods that are being used in the program
  // test("phantom method call") {
  //   testAssignExp(
  //     "database-phantom-method-call",
  //     q""""<Main: void main(java.lang.String[])>/l2#_5"""",
  //     q"""Invoke("<Main: void main(java.lang.String[])>/new Point/0", "<Point: void <init>(int,int)>", ConsArg(NumLit("1"), ConsArg(NumLit("2"), NilArg())))""")
  // }

  /**
   * Construct Statements
   */

  test("array write num index") {
    testGenStm(
      "database-array-write-num",
      q""""<Main: void main(java.lang.String[])>/write-array-idx/0"""",
      q"""ArrayWrite(Alloc("<Main: void main(java.lang.String[])>/new int[]/0", "int[]"), NumLit("1"), NumLit("12"))""")
  }

  test("array write complex index") {
    testGenStm(
      "database-array-write-complex",
      q""""<Main: void main(java.lang.String[])>/write-array-idx/0"""",
      q"""ArrayWrite(Alloc("<Main: void main(java.lang.String[])>/new int[]/0", "int[]"), BinOp("+", NumLit("2"), NumLit("2")), NumLit("12"))""")
  }

  test("instance field write") {
    testGenStm(
      "database-instance-field-write",
      q""""<Main: void main(java.lang.String[])>/write-field-x/0"""",
      q"""InstanceFieldWrite(SpecialInvoke("<Main: void main(java.lang.String[])>/new Point/0", "<Point: void <init>(int,int)>", ConsArg(NumLit("1"), ConsArg(NumLit("2"), NilArg()))), "<Point: int x>", NumLit("2"))""")
  }

  test("static field write") {
    testGenStm(
      "database-static-field-write",
      q""""<Main: void main(java.lang.String[])>/write-field-TY/0"""",
      q"""StaticFieldWrite("<Point: java.lang.String TY>", Alloc("POINT", "java.lang.String"))""")
  }

  test("return void") {
    testGenStm(
      "database-static-field-write",
      q""""<Main: void main(java.lang.String[])>/return-void/0"""",
      q"""ReturnVoid()""")
  }

  test("return numlit") {
    testGenStm(
      "database-return",
      q""""<Point: int maxX()>/return/0"""",
      q"""Return(NumLit("10"))""")
  }

  test("return field") {
    testGenStm(
      "database-return",
      q""""<Point: int getX()>/return/0"""",
      q"""Return(InstanceFieldRead(This(), "<Point: int x>"))""")
  }

  test("return complex expression") {
    testGenStm(
      "database-return",
      q""""<Point: Point add(int,int)>/return/0"""",
      q"""Return(SpecialInvoke("<Point: Point add(int,int)>/new Point/0", "<Point: void <init>(int,int)>", ConsArg(BinOp("+", InstanceFieldRead(This(), "<Point: int x>"), Var("<Point: Point add(int,int)>/@parameter0")), ConsArg(BinOp("+", InstanceFieldRead(This(), "<Point: int y>"), Var("<Point: Point add(int,int)>/@parameter1")), NilArg()))))""")
  }

  test("invoke statement") {
    testGenStm(
      "database-invoke-stm",
      q""""<Main: void main(java.lang.String[])>/Point.print/0"""",
      q"""InvokeStm(SpecialInvoke("<Main: void main(java.lang.String[])>/new Point/0", "<Point: void <init>(int,int)>", ConsArg(NumLit("1"), ConsArg(NumLit("2"), NilArg()))), "<Point: void print()>" ,NilArg())""")
  }

  test("goto statement") {
    testGenStm(
      "database-if",
      q""""<Main: void main(java.lang.String[])>/goto/0"""",
      q"""Goto(8)""")
  }

  test("if statement") {
    testGenStm(
      "database-if",
      q""""<Main: void main(java.lang.String[])>/if/0"""",
      q"""If("<=", NumLit("1"), NumLit("2"), 7)""")
  }

  test("table switch statement") {
    testGenStm(
      "database-switch",
      q""""<Main: void main(java.lang.String[])>/table-switch/0"""",
      q"""TableSwitch(BinOp("+", NumLit("2"), NumLit("1")), ConsCase(1, 5, ConsCase(2, 6, ConsCase(3, 7, DefaultCase(11)))))""")
  }

  test("throw statement") {
    val name = Lit.String("<Main: void main(java.lang.String[])>/throw $stack1/0")
    testGenStm(
      "database-throw",
      name,
      q"""Throw(SpecialInvoke("<Main: void main(java.lang.String[])>/new java.lang.IllegalArgumentException/0", "<java.lang.IllegalArgumentException: void <init>(java.lang.String)>" ,ConsArg(Alloc("1", "java.lang.String"), NilArg())))""")
  }

  test("recursive phi assignment statement") {
    val name = Lit.String("<typechecking.TypeChecker: java.lang.String visit(syntaxtree.MethodDeclaration,java.lang.String)>/phi-assign/0")
    val phiTrgVar = Lit.String("<typechecking.TypeChecker: java.lang.String visit(syntaxtree.MethodDeclaration,java.lang.String)>/i_$$A_1#_300")
    testGenStm(
      "database-minijavac",
      name,
      q"""Phi($phiTrgVar, ConsArg(NumLit("0"), ConsArg(BinOp("+", Var($phiTrgVar), NumLit("1")), NilArg())))""")
  }

  test("test gen simple stm list") {
    testGetStmList(
      "database-stmt-list",
      q""""<Main: void main(java.lang.String[])>"""",
      Some(q"""ConsStm(If("==", NumLit("1"), NumLit("1"), 5), 2, ConsStm(ReturnVoid(), 4, ConsStm(ReturnVoid(), 6, NilStm())))"""))
  }

  // need to account for holes in the sequence of instruction indices
  test("test stmt list with phi") {
    val targetName = Lit.String("<Main: void main(java.lang.String[])>/l3_$$A_3#_11")
    testGetStmList(
      "database-if",
      q""""<Main: void main(java.lang.String[])>"""",
      Some(q"""ConsStm(If("<=", NumLit("1"), NumLit("2"), 7), 4, ConsStm(Goto(8), 6, ConsStm(Phi($targetName, ConsArg(NumLit("1"), ConsArg(NumLit("2"), NilArg()))), 9, ConsStm(ReturnVoid(), 11, NilStm()))))"""))
  }

  test("test for minijavac main method") {
    testGetStmList(
      "database-minijavac",
      q""""<Main: void main(java.lang.String[])>"""",
      None)
  }

  test("test for minijavac typechecker methoddeclaration method") {
    testGetStmList(
      "database-minijavac",
      q""""<typechecking.TypeChecker: java.lang.String visit(syntaxtree.MethodDeclaration,java.lang.String)>"""",
      None)
  }
}