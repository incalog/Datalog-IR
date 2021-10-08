package inca.frontend.functionaxsouffle

import inca.frontend.functionalxsouffle.executor.FunctionalXSouffleExecutor
import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source
import scala.meta.XtensionQuasiquoteTerm
import scala.meta.Term

class CloneDetectionTest extends AnyFunSuite {
  // x = 1 +2
  // y = x +5

  /*

  def getOperand(inst: Inst, side: String): Set[Exp] =
    {NumLit(num) | assignNumFrom(inst, side)} ++
    {exp |
     (inst, side, var) in assignOpFrom,
     exp in assignExp(var)}

  def assingExp(v: Local): Set[Exp] =
      {BinOp(op, left, right) |
        (inst, _, v, _) in assignBinOp,
        (inst, op) in operatorAt,
        left in getOperand(inst, "1"),
        right in getOperand(inst, "2")
      }
      ++
      // unop
      // method call
      // phi
  (x, y, z) in assignBinOp
   */

  // BinOp: Instruction x

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
       |         | Invoke(Exp, String)
       |         | InstanceFieldRead(Exp, String)
       |         | StaticFieldRead(String)
       |//          | Null()
       |//          | CastNull(Exp)
       |//          | CastNum(Exp)
       |//          | PhantomInvoke()
       |//          | Return()
       |
       |
       |def assignExp(v: String): Set[Exp] =
       |  { NumLit(num) | (inst, idx, num, v, meth) in _AssignNumConstant } ++ {
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
       |    Alloc(heap) |
       |      (inst, idx, heap, v, meth, line) in _AssignHeapAllocation
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
       |    Invoke(recvExp, meth) |
       |      (inst, v) in _AssignReturnValue,
       |      (inst, idx, meth, recv, callingMeth) in _VirtualMethodInvocation,
       |      recvExp in assignExp(recv)
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
       |// def getArgList(): ArgList = {
       |// }
       |
       |@main def main(v: String): Set[Exp] = { exp | exp in assignExp(v) }
       |""".stripMargin

  val baseDir = s"souffle-frontend/doop-context-insensitive"

  val souffleSrc = Source.fromFile(s"$baseDir/souffle-input-schema.dl")
  val souffleCode = souffleSrc.getLines().mkString("\n")
  souffleSrc.close()
  val x = 1

  def testJimpleExample(dir: String, name: meta.Term): Unit = {
    val fun = FunctionalXSouffleExecutor.loadFunction(funCode, souffleCode)
    val res = fun.execute("main", Seq(name), s"$baseDir/$dir", false)
    // println(fun.compiled.optimized)
    // println(fun.compiled.psystemSource)
    println(res)
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
    testJimpleExample("database-multiple-method-call",q""""<Main: void main(java.lang.String[])>/l5#_7"""")
  }

  // TODO method call with arguments

  test("instance field read access ") {
    testJimpleExample("database-instance-field-read",q""""<Main: void main(java.lang.String[])>/l2#_13"""")
  }

  test("static field read access ") {
    testJimpleExample("database-static-field-read",q""""<Main: void main(java.lang.String[])>/l1#_13"""")
  }
}
