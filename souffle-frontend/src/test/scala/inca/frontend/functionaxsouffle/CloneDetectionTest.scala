package inca.frontend.functionaxsouffle

import inca.frontend.functionalxsouffle.executor.FunctionalXSouffleExecutor
import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source
import scala.meta.XtensionQuasiquoteTerm

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
       |data Exp = NumLit(String)
       |         | Var(String)
       |         | BinOp(String, Exp, Exp)
       |         | UnOp(String, Exp)
       |//          | Null()
       |//          | Cast(Exp, Type)
       |//          | InstanceOf(Exp, Type)
       |//          | CastNull(Exp)
       |//          | CastNum(Exp)
       |//          | Alloc(String) // is a heap allocation, which is a value which is a symbol
       |//          | OperFrom(Int, String) // what is the meaning?
       |//          | OperFromConst(Int, Int) // what is the meaning?
       |//          | PhantomInvoke()
       |//          | Return()
       |
       |
       |def assignExp(v: String): Set[Exp] =
       |  { BinOp(op, left, right) |
       |      (inst, idx, v, meth) in _AssignBinop,
       |      (inst, op) in _OperatorAt,
       |      left in getOperand(inst, 1),
       |      right in getOperand(inst, 2)
       |  } ++ {
       |    NumLit(num) | (inst, idx, num, v, meth) in _AssignNumConstant
       |  }
       |
       |def getOperand(inst: String, pos: Int): Set[Exp] =
       |  { NumLit(num) | (inst, pos, num) in _AssignOperFromConstant } ++
       |  { exp | (inst, pos, var) in _AssignOperFrom, exp in assignExp(var) }
       |
       |// @main def main(): Set[String] = { op | (ins, op) in _OperatorAt }
       |@main def main(v: String): Set[Exp] = { exp | exp in assignExp(v) }
       |""".stripMargin

  val baseDir = s"souffle-frontend/doop-context-insensitive"

  test("simple addition example") {
    val souffleSrc = Source.fromFile(s"$baseDir/souffle-input-schema.dl")
    val souffleCode = souffleSrc.getLines().mkString("\n")
    souffleSrc.close()


    val fun = FunctionalXSouffleExecutor.loadFunction(funCode, souffleCode)
//    val res = fun.execute("main", Seq(q""""<Main: void main(java.lang.String[])>/x#_3""""), s"$baseDir/database-simple-add", false)

    val res = fun.execute("main", Seq(q""""<Main: void main(java.lang.String[])>/y#_4""""), s"$baseDir/database-simple-add", false)
    println(res)
  }
}
