package inca.ir.extension.bool

import inca.ir
import inca.ir.visitors.IRVisitor
import inca.ir.{Name, Term, Var}

enum Value:
  case True
  case False
  case Undetermined

  def &&(t1: Value): Value = (this, t1) match
    case (Value.False, _) => Value.False
    case (_, Value.False) => Value.False
    case (Value.True, Value.True) => Value.True
    case _ => Value.Undetermined

  def ||(t1: Value): Value = (this, t1) match
    case (Value.True, _) => Value.True
    case (_, Value.True) => Value.True
    case (Value.False, Value.False) => Value.False
    case _ => Value.Undetermined

  def unary_! : Value = this match
    case Value.True => Value.False
    case Value.False => Value.True
    case _ => Value.Undetermined


// optimize by brute fore, McCluskey algorithm is the better way to go
case class BoolTable(t: Term):
  lazy val results: Map[Map[Name, Value], Value] = {
    val varAssignment = generateVariableAssignments(t.vars.map(_.name))
    varAssignment.map(as => as -> eval(t, as)).toMap
  }

  def uniqueResult: Value = if results.values.toSet.size == 1 then results.values.head else Value.Undetermined

  private def generateVariableAssignments(names: Seq[Name]): Seq[Map[Name, Value]] =
    val numNames = names.length
    val binaryCombinations = (0 until math.pow(2, numNames).toInt).map { i =>
      val binaryString = i.toBinaryString.reverse.padTo(numNames, '0').reverse
      binaryString.map(_.asDigit).toList.map(i => if i == 1 then Value.True else Value.False)
    }
    binaryCombinations.map { combination => names.zip(combination).toMap }

  private def eval(t: Term, values: Map[Name, Value]): Value = t match
    case BoolNot(t1) => !eval(t1, values)
    case BoolOr(t1, t2) => eval(t1, values) || eval(t2, values)
    case BoolAnd(t1, t2) => eval(t1, values) && eval(t2, values)
    case BoolTrue => Value.True
    case BoolFalse => Value.False
    case Var(ref) => values(ref.name)
    case _ => Value.Undetermined


trait SyntacticOptimizer extends IRVisitor:
  override def name: String = "SyntacticBoolOptimizer"

  override def visitTerm(term: Term): Seq[Term] = term match
      case BoolAnd(t1, t2) => eval(term)
      case BoolOr(t1, t2) => eval(term)
      case BoolNot(t1) => eval(term)
      case _ => super.visitTerm(term)

  private def eval(t: Term): Seq[Term] =
    val boolTable = BoolTable(t)
    boolTable.uniqueResult match
      case Value.Undetermined => super.visitTerm(t)
      case Value.True => Seq(BoolTrue)
      case Value.False => Seq(BoolFalse)
