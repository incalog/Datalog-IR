package org.inca.diff.reflect

import org.inca.diff.StructuralDiff
import org.inca.diff.reflect.Diff._
import org.inca.diff.reflect.DiffApply._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TestWhileLangDiffIndexSubtreesOracle extends TestWhileLangDiff(IndexSubtreesOracle)

class TestWhileLangDiffCryptoHashOracle extends TestWhileLangDiff(CryptoHashOracle)

class TestWhileLangDiff(mkOracle: MkOracle) extends AnyFlatSpec with Matchers {

  trait Exp extends StructuralDiff
  case class ScalarLit(d: Double) extends Exp
  case class MatrixLit(lit: List[List[Exp]]) extends Exp
  case class Add(e1: Exp, e2: Exp) extends Exp
  case class Mul(e1: Exp, e2: Exp) extends Exp
  case class Dot(e1: Exp, e2: Exp) extends Exp
  case class Trans(e: Exp) extends Exp
  case class Var(v: Symbol) extends Exp

  trait Stm extends StructuralDiff
  case class AssignVar(v: Symbol, e: Exp) extends Stm
  case class IfNotZero(cond: Exp, thn: Stm, els: Stm) extends Stm
  case class WhileNotZero(cond: Exp, body: Stm) extends Stm
  case class Block(content: List[Stm]) extends Stm

  val cScalarLit = classOf[ScalarLit]
  val cMatrixLit = classOf[MatrixLit]
  val cAdd = classOf[Add]
  val cMul = classOf[Mul]
  val cDot = classOf[Dot]
  val cTrans = classOf[Trans]
  val cVar = classOf[Var]
  
  val cAssignVar = classOf[AssignVar]
  val cIfNotZero = classOf[IfNotZero]
  val cWhileNotZero = classOf[WhileNotZero]
  val cBlock = classOf[Block]
  
  val cSeq = classOf[Seq[_]]

  implicit def intToScalar(i: Int): Exp = ScalarLit(i)
  implicit def doubleToScalar(d: Double): Exp = ScalarLit(d)
  implicit def stringToSymbol(s: String): Symbol = Symbol(s)
  implicit def stringToVar(s: String): Exp = Var(s)
  implicit def symbolToVar(s: Symbol): Exp = Var(s)

  implicit val implicit_mkOracle: MkOracle = mkOracle

  val ex1 = Block(List(
    AssignVar("A", MatrixLit(List(List(1,2,0), List(2,5,-1), List(4,10,-1)))),
    AssignVar("B", Trans("A")),
    AssignVar("C", Mul("A", "B")),
    IfNotZero("B",
      AssignVar("ans", Mul("C", "C")),
      AssignVar("ans", "C")
    )
  ))

  val ex2 = Block(List(
    AssignVar("m", MatrixLit(List(List(1,2,0), List(2,5,-1), List(4,10,-1)))),
    AssignVar("power", "m"),
    AssignVar("i", 0),
    AssignVar("untilVar", Add(-5, -5)),
    WhileNotZero(Add("i", "untilVar"), Block(List(
        AssignVar("power", Mul("power", "m")),
        AssignVar("i", Add("i", 1))
    )))
  ))

  "diff of identical trees" should "yield empty patch" in {
    val emptyPatch: PartialFunction[Any,_] = {
      case Hole(Change(Hole(i1), Hole(i2))) if i1==i2 =>
    }

    diffTree(ex1, ex1) should matchPattern (emptyPatch)
    diffTree(ex2, ex2) should matchPattern (emptyPatch)
  }

  "diff of renamed trees" should "yield copy patch" in {
    val renamed1_1 = Block(List(
      AssignVar("X", MatrixLit(List(List(1,2,0), List(2,5,-1), List(4,10,-1)))),
      AssignVar("B", Trans("X")),
      AssignVar("C", Mul("X", "B")),
      IfNotZero("B",
        AssignVar("ans", Mul("C", "C")),
        AssignVar("ans", "C")
      )
    ))
    diffTree(ex1, renamed1_1) should matchPattern { case
      NodeC(`cBlock`,Seq(NodeC(`cSeq`,List(
        NodeC(`cAssignVar`,Seq(
          Hole(Change(ValC(Symbol("A")),ValC(Symbol("X")))),
          Hole(Change(Hole(i1),Hole(i2))))),
        NodeC(`cAssignVar`,Seq(
          Hole(Change(Hole(j1),Hole(j2))),
          NodeC(`cTrans`,Seq(
            NodeC(`cVar`,Seq(Hole(Change(ValC(Symbol("A")),ValC(Symbol("X")))))))))),
        NodeC(`cAssignVar`,Seq(
          Hole(Change(Hole(k1),Hole(k2))),
          NodeC(`cMul`,Seq(
            NodeC(`cVar`,Seq(Hole(Change(ValC(Symbol("A")),ValC(Symbol("X")))))),
            Hole(Change(Hole(l1),Hole(l2))))))),
        Hole(Change(Hole(m1),Hole(m2)))))))
      if i1==i2 && j1==j2 && k1==k2 && l1==l2 && m1==m2 =>
    }
    applyPatch(diffTree(ex1, renamed1_1), ex1) should be (renamed1_1)

    val renamed1_2 = Block(List(
      AssignVar("A", MatrixLit(List(List(1,2,0), List(2,5,-1), List(4,10,-1)))),
      AssignVar("B", Trans("A")),
      AssignVar("X", Mul("A", "B")),
      IfNotZero("B",
        AssignVar("ans", Mul("X", "X")),
        AssignVar("ans", "X")
      )
    ))
    diffTree(ex1, renamed1_2) should matchPattern { case
      NodeC(`cBlock`,Seq(NodeC(`cSeq`,List(
        Hole(Change(Hole(i1),Hole(i2))),
        Hole(Change(Hole(j1),Hole(j2))),
        NodeC(`cAssignVar`,Seq(
          Hole(Change(ValC(Symbol("C")),ValC(Symbol("X")))),
          Hole(Change(Hole(k1),Hole(k2))))),
        NodeC(`cIfNotZero`,Seq(
          Hole(Change(Hole(m1),Hole(m2))),
          NodeC(`cAssignVar`, Seq(
            Hole(Change(Hole(l1),Hole(l2))),
            NodeC(`cMul`, Seq(
              NodeC(`cVar`, Seq(Hole(Change(ValC(Symbol("C")),ValC(Symbol("X")))))),
              NodeC(`cVar`, Seq(Hole(Change(ValC(Symbol("C")),ValC(Symbol("X"))))))
            ))
          )),
          NodeC(`cAssignVar`, Seq(
            Hole(Change(Hole(l3),Hole(l4))),
            NodeC(`cVar`, Seq(Hole(Change(ValC(Symbol("C")),ValC(Symbol("X"))))))
          ))
        ))
      ))))
      if i1==i2 && j1==j2 && k1==k2 && l1==l2 && l2==l3 && l3==l4 && m1==m2 =>
    }
  }

  "diff of swapped lines" should "yield move patch" in {
    val moved1_1 = Block(List(
      AssignVar("C", Mul("A", "B")),
      AssignVar("ans", Mul("C", "C")),
      AssignVar("B", Trans("A")),
      IfNotZero("B",
        AssignVar("A", MatrixLit(List(List(1,2,0), List(2,5,-1), List(4,10,-1)))),
        AssignVar("ans", "C")
      )
    ))

    println(diffTree(ex1, moved1_1))
  }
}
