package inca.ir.extension.bool

import inca.ir
import inca.ir.visitors.IRVisitor
import inca.ir.{Name, Term, Var}


trait SyntacticOptimizer extends IRVisitor:
  override def name: String = "SyntacticBoolOptimizer"

  private def distinguishAndSort(ts: Seq[Term]): Seq[Term] =
    ts.distinct.sorted { (a, b) => a.toString.compareTo(b.toString) }

  def generateCombinations(names: Seq[Name]): Seq[Map[Name, Boolean]] = {
    val numNames = names.length
    val binaryCombinations = (0 until math.pow(2, numNames).toInt).map { i =>
      val binaryString = i.toBinaryString.reverse.padTo(numNames, '0').reverse
      binaryString.map(_.asDigit).toList.map(i => if i == 1 then true else false)
    }
    binaryCombinations.map { combination => names.zip(combination).toMap }
  }

  override def visitTerm(term: Term): Seq[Term] = term match
      // optimize by brute fore, McCluskey algorithm is the better way to go
      case BoolAnd(t1, t2) => eval(term) match
        case Some(res) => Seq(res)
        case _ => super.visitTerm(term)
      case BoolOr(t1, t2) => eval(term) match
        case Some(res) => Seq(res)
        case _ => super.visitTerm(term)
      case BoolNot(t1) => eval(term) match
        case Some(res) => Seq(res)
        case _ => super.visitTerm(term)
      case _ => super.visitTerm(term)


  private def eval(t: Term): Option[Term] = {
    val results = generateCombinations(t.vars.map(_.name)).map { comb =>
      eval(t, comb)
    }.toSet
    // if we find a single result for each variable assignment, then we can simplify the term
    if results.size != 1 then
      None
    else
      if results.head then
        Some(BoolTrue)
      else
        Some(BoolFalse)
  }

  private def eval(t: Term, values: Map[Name, Boolean]): Boolean = t match
      case BoolNot(t1) => !eval(t1, values)
      case BoolOr(t1, t2) => eval(t1, values) || eval(t2, values)
      case BoolAnd(t1, t2) => eval(t1, values) && eval(t2, values)
      case BoolTrue => true
      case BoolFalse => true
      case Var(ref) => values(ref.name)
