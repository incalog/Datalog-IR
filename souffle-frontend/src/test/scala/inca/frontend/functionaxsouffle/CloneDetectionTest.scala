package inca.frontend.functionaxsouffle

import inca.frontend.functionalxsouffle.executor.FunctionalXSouffleExecutor
import org.scalatest.funsuite.AnyFunSuite

import scala.io.{BufferedSource, Source}
import scala.meta.XtensionQuasiquoteTerm

class CloneDetectionTest extends AnyFunSuite {

  val funCode: String =
    s"""module CloneDetection
       |
       |data ArgList = NoArg() | Arg(Exp, ArgList)
       |data Exp = NumLit(String)
       |         | Var(String)
       |         | BinOp(String, Exp, Exp)
       |         | UnOp(String, Exp)
       |         | Cast(Exp, String)
       |         | InstanceOf(Exp, String)
       |         | StringLit(String)
       |         | Alloc(String) // is a heap allocation, which is a value which is a symbol
       |         | ArrayRead(Exp, Exp)
       |         | InstanceFieldRead(Exp, String)
       |         | StaticFieldRead(String)
       |         | Invoke(Exp, String, ArgList)
       |         | StaticInvoke(String, ArgList)
       |         | SpecialInvoke(String, String, ArgList)
       |         | Null()
       |//          | CastNull(Exp)
       |//          | PhantomInvoke()
       |
       |
       |def assignExp(v: String): Set[Exp] = {
       |    NumLit(num) | (inst, idx, num, v, meth) in _AssignNumConstant
       |  } ++ {
       |    BinOp(op, left, right) |
       |      (inst, idx, v, meth) in _AssignBinop,
       |      (inst, op) in _OperatorAt,
       |      left in getOperand(inst, 1),
       |      right in getOperand(inst, 2)
       |  } ++ {
       |    UnOp(op, exp) |
       |      (inst, idx, v, meth) in _AssignUnop,
       |      (inst, op) in _OperatorAt,
       |      exp in getOperand(inst, 1)
       |  } ++ {
       |    exp | (inst, idx, from, v, meth) in _AssignLocal, exp in assignExp(from)
       |  } ++ {
       |    // TODO maybe remove the method prefix from the variable name
       |    Var(v) | (idx, meth, v) in _FormalParam
       |  } ++ {
       |    Cast(exp, ty) |
       |      (inst, idx, from, v, ty, meth) in _AssignCast,
       |      exp in assignExp(from)
       |  } ++ {
       |    InstanceOf(exp, ty) |
       |      (inst, idx, from, v, ty, meth) in _AssignInstanceOf,
       |      exp in assignExp(from)
       |  } ++ {
       |    Null() | (inst, idx, v, meth) in _AssignNull
       |  } ++ {
       |    Alloc(heap) |
       |      (inst1, idx1, heap, v, meth, line) in _AssignHeapAllocation,
       |      (inst2, idx2, specialmeth, v, meth) not in _SpecialMethodInvocation
       |  } ++ {
       |    SpecialInvoke(heap, specialmeth, args) |
       |      (inst1, idx1, heap, v, meth, line) in _AssignHeapAllocation,
       |      (inst2, idx2, specialmeth, v, callingMeth) in _SpecialMethodInvocation,
       |      args in getArgs(inst2, 0)
       |  } ++ {
       |    ArrayRead(exp, NumLit(`String.valueOf`(index))) |
       |       (inst, idx, v, from, meth) in _LoadArrayIndex,
       |       exp in assignExp(from),
       |       (inst, index) in _ArrayNumIndex
       |  } ++ {
       |    ArrayRead(exp, indexExp) |
       |       (inst, idx, v, from, meth) in _LoadArrayIndex,
       |       (inst, indexVar) in _ArrayInsnIndex,
       |       exp in assignExp(from),
       |       indexExp in assignExp(indexVar)
       |  } ++ {
       |    Invoke(recvExp, meth, args) |
       |      (inst, v) in _AssignReturnValue,
       |      (inst, idx, meth, recv, callingMeth) in _VirtualMethodInvocation,
       |      recvExp in assignExp(recv),
       |      args in getArgs(inst, 0)
       |  } ++ {
       |    StaticInvoke(meth, args) |
       |      (inst, v) in _AssignReturnValue,
       |      (inst, idx, meth, callingMeth) in _StaticMethodInvocation,
       |      args in getArgs(inst, 0)
       |  } ++ {
       |    InstanceFieldRead(recvExp, field) |
       |      (inst, idx, v, recv, field, meth) in _LoadInstanceField,
       |      recvExp in assignExp(recv)
       |  } ++ {
       |    StaticFieldRead(fieldsig) | (inst, idx, v, fieldsig, meth) in _LoadStaticField
       |  }
       |
       |def getOperand(inst: String, pos: Int): Set[Exp] =
       |  { NumLit(num) | (inst, pos, num) in _AssignOperFromConstant } ++
       |  { exp | (inst, pos, var) in _AssignOperFrom, exp in assignExp(var) }
       |  // { StringLit(str) | str
       |
       |def getArgs(inst: String, currentIdx: Int): Set[ArgList] = {
       |  Arg(exp, rest) |
       |    (currentIdx, inst, v) in _ActualParam,
       |    exp in assignExp(v),
       |    rest in getArgs(inst, currentIdx + 1)
       |} ++ {
       |  NoArg() | (currentIdx, inst, v) not in _ActualParam
       |}
       |
       |@main def main(v: String): Set[Exp] = { exp | exp in assignExp(v) }
       |""".stripMargin

  val baseDir = s"souffle-frontend/doop-context-insensitive"

  val souffleSrc: BufferedSource = Source.fromFile(s"$baseDir/souffle-input-schema.dl")
  val souffleCode: String = souffleSrc.getLines().mkString("\n")
  souffleSrc.close()

  def testAssignExp(dir: String, name: meta.Term, expected: meta.Term): Unit = {
    val fun = FunctionalXSouffleExecutor.loadFunction(funCode, souffleCode)
    val res = fun.execute("main", Seq(name), s"$baseDir/$dir", false)
    assert(res == fun.result(expected))
  }

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
      q"""InstanceOf(Alloc("12"), "java.lang.String")""")
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
      q"""Invoke(ArrayRead(Var("<Main: void main(java.lang.String[])>/@parameter0"), NumLit("0")), "<java.lang.String: int length()>", NoArg())""")
  }

  test("method call example") {
    testAssignExp(
      "database-method-call",
      q""""<Main: void main(java.lang.String[])>/l3#_5"""",
      q"""BinOp("+", Invoke(Alloc("12"), "<java.lang.String: int length()>", NoArg()), NumLit("1"))""")
  }

  test("method call with multiple args example") {
    testAssignExp(
      "database-multiple-arg-method-call",
      q""""<Main: void main(java.lang.String[])>/l4#_7"""",
      q"""Invoke(SpecialInvoke("<Main: void main(java.lang.String[])>/new Point/0", "<Point: void <init>(int,int)>", Arg(NumLit("1"), Arg(NumLit("2"), NoArg()))), "<Point: Point add(int,int)>", Arg(NumLit("10"), Arg(NumLit("12"), NoArg())))""")
  }

  test("instance field read access ") {
    testAssignExp(
      "database-instance-field-read",
      q""""<Main: void main(java.lang.String[])>/l2#_5"""",
      q"""InstanceFieldRead(SpecialInvoke("<Main: void main(java.lang.String[])>/new Point/0", "<Point: void <init>(int,int)>", Arg(NumLit("1"), Arg(NumLit("2"), NoArg()))), "<Point: int x>")""")
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
      q"""StaticInvoke("<Point: Point genPoint(int,int)>", Arg(NumLit("1"), Arg(NumLit("2"), NoArg())))"""
    )
  }

  test("method call with null argument") {
    testAssignExp(
      "database-null-argument",
      q""""<Main: void main(java.lang.String[])>/l2#_5"""",
      q"""Invoke(StaticInvoke("<Point: Point genPoint(int,int)>", Arg(NumLit("1"), Arg(NumLit("2"), NoArg()))), "<Point: Point add(Point)>", Arg(Null(), NoArg()))""")
  }

  test("constructor call") {
    testAssignExp(
      "database-constructor-call",
      q""""<Main: void main(java.lang.String[])>/l1#_4"""",
      q"""SpecialInvoke("<Main: void main(java.lang.String[])>/new Point/0", "<Point: void <init>(int,int)>", Arg(NumLit("1"), Arg(NumLit("2"), NoArg())))""")
  }
}
