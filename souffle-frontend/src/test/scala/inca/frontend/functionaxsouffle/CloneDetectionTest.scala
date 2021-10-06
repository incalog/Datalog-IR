package inca.frontend.functionaxsouffle

import inca.frontend.functionalxsouffle.executor.FunctionalXSouffleExecutor
import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source

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

  val funCode: String =
    s"""module CloneDetection
       |
       |data Exp = Num(Int)
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
       |// def genExp(instr: Instruction): Exp = Num(1)
       |
       |@main def main(): Set[String] = { op | (ins, op) in _OperatorAt }
       |""".stripMargin

  val baseDir = s"souffle-frontend/doop-context-insensitive"

  test("simple addition example") {
    val souffleSrc = Source.fromFile(s"$baseDir/souffle-input-schema.dl")
    val souffleCode = souffleSrc.getLines().mkString("\n")
    souffleSrc.close()


    val fun = FunctionalXSouffleExecutor.loadFunction(funCode, souffleCode)
    val res = fun.execute("main", Seq(), s"$baseDir/database-simple-add", false)
    println(res)
  }
}
