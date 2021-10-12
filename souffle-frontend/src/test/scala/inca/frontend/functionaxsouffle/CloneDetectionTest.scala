package inca.frontend.functionaxsouffle

import inca.frontend.functionalxsouffle.executor.FunctionalXSouffleExecutor
import org.scalatest.funsuite.AnyFunSuite

import scala.io.{BufferedSource, Source}
import scala.meta.XtensionQuasiquoteTerm
import scala.meta.Term

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

  def testJimpleExample(dir: String, name: meta.Term): Unit = {
    val fun = FunctionalXSouffleExecutor.loadFunction(funCode, souffleCode)
    val res = fun.execute("main", Seq(name), s"$baseDir/$dir", false)
    println(res)
    assert(res.res.size == 1)
  }

  test("simple addition example") {
    testJimpleExample("database-simple-add",q""""<Main: void main(java.lang.String[])>/y#_4"""")
  }

  test("nested addition example") {
    testJimpleExample("database-nested-add",q""""<Main: void main(java.lang.String[])>/z#_5"""")
  }

  test("cast example") {
    testJimpleExample("database-cast",q""""<Main: void main(java.lang.String[])>/l2#_4"""")
  }

  test("instanceof example") {
    testJimpleExample("database-instanceof",q""""<Main: void main(java.lang.String[])>/l2#_4"""")
  }

  test("simple unop")  {
    testJimpleExample("database-unop",q""""<Main: void main(java.lang.String[])>/l1#_3"""")
  }

  test("array read with int") {
    testJimpleExample("database-array-read-int",q""""<Main: void main(java.lang.String[])>/l1#_3"""")
  }

  test("array read with complex expression") {
    testJimpleExample("database-array-read-var",q""""<Main: void main(java.lang.String[])>/l4#_6"""")
  }

  test("string length method call example") {
    testJimpleExample("database-string-length",q""""<Main: void main(java.lang.String[])>/l1#_3"""")
  }

  test("method call example") {
    testJimpleExample("database-method-call",q""""<Main: void main(java.lang.String[])>/l3#_5"""")
  }

  test("multiple method call example") {
    testJimpleExample("database-multiple-arg-method-call",q""""<Main: void main(java.lang.String[])>/l4#_7"""")
  }

  // TODO consider arguments and constructor that are being used for alloc
  test("instance field read access ") {
    testJimpleExample("database-instance-field-read",q""""<Main: void main(java.lang.String[])>/l2#_13"""")
  }

  test("static field read access ") {
    testJimpleExample("database-static-field-read",q""""<Main: void main(java.lang.String[])>/l1#_13"""")
  }

  test("static method call") {
    testJimpleExample("database-static-method-call",q""""<Main: void main(java.lang.String[])>/l1#_4"""")
  }

  test("method call with null argument") {
    testJimpleExample("database-null-argument",q""""<Main: void main(java.lang.String[])>/l2#_5"""")
  }

  test("constructor call") {
    testJimpleExample("database-constructor-call",q""""<Main: void main(java.lang.String[])>/l1#_4"""")
  }
}
