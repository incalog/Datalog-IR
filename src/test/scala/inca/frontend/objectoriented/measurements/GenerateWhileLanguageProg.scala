package inca.frontend.objectoriented.measurements

import inca.frontend.objectoriented.core._
import inca.util.Scala

trait Exp {
  def toAst: Expression
}
trait Stm {
  def toAst: Expression
}


class BinOp(left: Exp, right: Exp) extends Exp {
  override def toString: String = {
    val clsName = this.getClass.getSimpleName
    s"new $clsName($left, $right)"
  }

  override def toAst: Expression = {
    val clsName = this.getClass.getSimpleName
    ConstructorExpr(ClassRef(Name(clsName)), Seq(left.toAst, right.toAst))
  }
}
case class Var(name: String) extends Exp {
  override def toString: String = s"""new Var("$name")"""

  override def toAst: Expression = {
    ConstructorExpr(ClassRef(Name("Var")), Seq(BaseLitExpr(Scala(scala.meta.Lit.String(name)))))
  }
}
case class Num(i: Int) extends Exp {
  override def toString: String = s"new Num($i)"

  override def toAst: Expression = {
    ConstructorExpr(ClassRef(Name("Num")), Seq(BaseLitExpr(Scala(scala.meta.Lit.Int(i)))))
  }
}
case class Add(left: Exp, right: Exp) extends BinOp(left, right)
case class Sub(left: Exp, right: Exp) extends BinOp(left, right)
case class Mul(left: Exp, right: Exp) extends BinOp(left, right)
case class GT(left: Exp, right: Exp) extends BinOp(left, right)


case class Sequence(s1: Stm, s2: Stm) extends Stm {
  override def toString: String = s"new Sequence($s1, $s2)"

  override def toAst: Expression = {
    ConstructorExpr(ClassRef(Name("Sequence")), Seq(s1.toAst, s2.toAst))
  }
}

case class VarDef(name: String, exp: Exp) extends Stm {
  override def toString: String = s"""new VarDef("$name", $exp)"""

  override def toAst: Expression = {
    ConstructorExpr(ClassRef(Name("VarDef")), Seq(BaseLitExpr(Scala(scala.meta.Lit.String(name))), exp.toAst))
  }
}

case object Skip extends Stm {
  override def toString: String = s"new Skip()"

  override def toAst: Expression = {
    ConstructorExpr(ClassRef(Name("Skip")), Seq())
  }
}

case class ValDef(name: String, exp: Exp) extends Stm {
  override def toString: String = s"""new ValDef("$name", $exp)"""

  override def toAst: Expression = {
    ConstructorExpr(ClassRef(Name("ValDef")), Seq(BaseLitExpr(Scala(scala.meta.Lit.String(name))), exp.toAst))
  }
}
case class Assign(name: String, exp: Exp) extends Stm {
  override def toString: String = s"""new Assign("$name", $exp)"""

  override def toAst: Expression = {
    ConstructorExpr(ClassRef(Name("Assign")), Seq(BaseLitExpr(Scala(scala.meta.Lit.String(name))), exp.toAst))
  }
}
case class While(cnd: Exp, body: Stm) extends Stm {
  override def toString: String = s"new While($cnd, $body)"

  override def toAst: Expression = {
    ConstructorExpr(ClassRef(Name("While")), Seq(cnd.toAst, body.toAst))
  }
}

object GenerateWhileLanguageProg {
  val firstLetter = "a"
  def nextLetter(x:String, off: Int = 1): String = (x(0) + off).toChar.toString

  // Parsing the program gets stuck if we generate an input with to many constructors...
  //  We create the AST directly and insert it into the program.
  def generateProgramAst(numSeq: Int, numWhile: Int): Expression = {
    generateWhile(numWhile, numWhile, numSeq, "a").toAst
  }

  def generateVarSetExr(numSeq: Int, numWhile: Int): Expression = {
    var varName = firstLetter
    val args = (0 until numWhile).map { _ =>
      val a = BaseLitExpr(Scala(scala.meta.Lit.String(varName)))
      varName = nextLetter(varName)
      a
    }
    SetExpr(args)
  }

  private def generateWhile(size: Int, cur: Int, numAssign: Int, varName: String): Stm = {
    if (cur == 0)
      return Assign(
        firstLetter,
        (0 until size-1).foldLeft[Exp](Var(firstLetter)) { case (acc, i) =>
          Add(acc, Var(nextLetter(firstLetter, i+1)))
        }
      )

    val vName = nextLetter(varName)
    val cnd = if (cur == size)
      Num(1)
    else
      GT(Var(varName), Num(size))

    While(
      cnd, generateAssignSequence(numAssign, numAssign, varName, generateWhile(size, cur-1, numAssign, vName))
    )
  }

  private def generateAssignSequence(size: Int, cur: Int, varName: String, last: Stm): Stm = {
    if (cur == 0)
      return last
    val first = if (cur == size)
      VarDef(varName, Num(1))
    else
      Assign(varName, Num(1))
    Sequence(first, generateAssignSequence(size, cur-1, varName, last))
  }
}
