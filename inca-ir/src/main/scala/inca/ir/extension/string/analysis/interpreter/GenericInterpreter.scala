package inca.ir.extension.string.analysis.interpreter

import inca.ir
import inca.ir.Atom
import inca.ir.analysis.base.interpreter.{BaseGenericInterpreter, SupColumn}
import inca.ir.extension.string.{StringConcat, OrdinalNumber, RegexMatch, StringLength, StringLit, Substring, ToString}
import sturdy.data.MayJoin

trait StringOps[B, V]:
  def stringLit(s: String): V
  def toString(v: V): V
  def concat(v1: V, v2: V): V
  def substring(v: V, index: V, length: V): V
  def stringLength(v: V): V
  def ordinalNumber(v: V): V
  def matches(v: V, pattern: V): B
  def stringValue(v: V): String

trait GenericInterpreter[V, B, RV, ExcV, J[_] <: MayJoin[?]] extends BaseGenericInterpreter[V, B, RV, ExcV, J]:
  lazy val stringOps: StringOps[B, V]

  override protected def canDetermineValue(t: ir.Term): Boolean = t match
    case StringLit(_) => true
    case StringConcat(lhs, rhs) => canDetermineValue(lhs) && canDetermineValue(rhs)
    case ToString(t) => canDetermineValue(t)
    case Substring(t, index, length) => canDetermineValue(t) && canDetermineValue(index) && canDetermineValue(length)
    case StringLength(t) => canDetermineValue(t)
    case OrdinalNumber(t) => canDetermineValue(t)
    case _ => super.canDetermineValue(t)

  override def evalTermOpen(term: ir.Term)(using Fixed): SupColumn = term match
    case StringLit(s) => termResult(stringOps.stringLit(s))
    case ToString(t) => unaryOp(evalTerm(t))(stringOps.toString)
    case StringConcat(lhs, rhs) => binaryOp(evalTerm(lhs), evalTerm(rhs))(stringOps.concat)
    case Substring(t, index, length) => ternaryOp(evalTerm(t), evalTerm(index), evalTerm(length))(stringOps.substring)
    case StringLength(t) => unaryOp(evalTerm(t))(stringOps.stringLength)
    case OrdinalNumber(t) => unaryOp(evalTerm(t))(stringOps.ordinalNumber)
    case _ => super.evalTermOpen(term)

  override def evalAtomOpen(at: Atom)(using rec: Fixed): Unit = at match
    case RegexMatch(t, pattern, neg) =>
      val strCol = evalTerm(t)
      val patCol = evalTerm(pattern)
      updateSupplementaryUnchecked { sup =>
        val strIx = relationOps.columnIndex(sup, strCol)
        val patIx = relationOps.columnIndex(sup, patCol)
        relationOps.filter(sup) { row =>
          val matches = stringOps.matches(row(strIx), row(patIx))
          if (neg) boolOps.not(matches) else matches
        }
      }
    case _ => super.evalAtomOpen(at)