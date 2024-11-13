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


// Improvement: Use McCluskey algorithm to reduce dnf
case class BoolTable(t: Term):
  type VariableAssignment = Map[Name, Value]

  lazy val results: Map[VariableAssignment, Value] = {
    // We could limit the depth here to only optimize up to x variables
    val varAssignment = generateVariableAssignments(t.vars.map(_.name))
    varAssignment.map(as => as -> eval(t, as)).toMap
  }

  def uniqueResult: Value = if results.values.toSet.size == 1 then results.values.head else Value.Undetermined

  def dnf: Option[Term] =
    this.uniqueResult match
      case Value.True => Some(BoolTrue)
      case Value.False => Some(BoolFalse)
      // we can not create the DNF if any term is undetermined
      case Value.Undetermined if results.values.toSet.contains(Value.Undetermined) => None
      // the result is either true or false, that is create the DNF (not necessarily minimal)
      case _ =>
        val conjunctions = results.flatMap {
          case (varAssignment, Value.True) =>
            val terms = varAssignment.map {
              case (varName, Value.True) => Var(varName)
              case (varName, Value.False) => BoolNot(Var(varName))
            }
            val conjunction = terms.fold(BoolTrue) {
              case (BoolTrue, t) => t
              case (t, BoolTrue) => t
              case (acc, t) => BoolAnd(acc, t)
            }
            Some(conjunction)
          case _ => None
        }

        val disjunctions = conjunctions.fold(BoolFalse) {
          case (BoolFalse, t) => t
          case (t, BoolFalse) => t
          case (acc, t) => BoolOr(acc, t)
        }
        Some(disjunctions)

  private def generateVariableAssignments(names: Seq[Name]): Seq[VariableAssignment] =
    val numNames = names.length
    val binaryCombinations = (0 until math.pow(2, numNames).toInt).map { i =>
      val binaryString = i.toBinaryString.reverse.padTo(numNames, '0').reverse
      binaryString.map(_.asDigit).toList.map(i => if i == 1 then Value.True else Value.False)
    }
    binaryCombinations.map { combination => names.zip(combination).toMap }

  private def eval(t: Term, variableAssignment: VariableAssignment): Value = t match
    case BoolNot(t1) => !eval(t1, variableAssignment)
    case BoolOr(t1, t2) => eval(t1, variableAssignment) || eval(t2, variableAssignment)
    case BoolAnd(t1, t2) => eval(t1, variableAssignment) && eval(t2, variableAssignment)
    case BoolTrue => Value.True
    case BoolFalse => Value.False
    case Var(ref) => variableAssignment(ref.name)
    case _ => Value.Undetermined


trait SyntacticOptimizer extends IRVisitor:
  override def name: String = "SyntacticBoolOptimizer"

  // term -> DNF if exists or original visited Term
  var cache: Map[Term, Seq[Term]] = Map()

  override def visitTerm(term: Term): Seq[Term] = term match
    case BoolAnd(t1, t2) => eval(term)
    case BoolOr(t1, t2) => eval(term)
    case BoolNot(t1) => eval(term)
    case _ => super.visitTerm(term)

  private def eval(t: Term): Seq[Term] = cache.get(t) match
    case Some(cachedDnf) => cachedDnf
    case _ =>
      val boolTable = BoolTable(t)
      val dnf = boolTable.dnf match
        case Some(term) => Seq(term)
        // we use an unsupported feature, such as blocks inside a boolean
        case _ => super.visitTerm(t)
      cache += t -> dnf
      dnf