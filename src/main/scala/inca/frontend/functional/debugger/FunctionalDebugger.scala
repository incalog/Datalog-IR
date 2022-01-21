package inca.frontend.functional.debugger

import inca.backend.ir.Datalog
import inca.backend.ir.Datalog.EqComparator
import inca.compiler.source.{ExcerptAbsoluteRegion, ExcerptRelativeRegion}
import inca.debugger.{AtListElem, Debugger}
import inca.frontend.functional.core.FunctionDef

import scala.annotation.tailrec

final class FunctionalDebugger extends Debugger {
  override val frontend: FunctionalDebuggerFrontend = new FunctionalDebuggerFrontend


  def stepIntoFrontend(): Unit = {
    var fp: Option[FunctionalControlPoint] = None
    while (fp.isEmpty) {
      stepInto()
      if (callStack.isEmpty)
        return
      fp = frontend.frontendPoint(controlPointIR)
    }
    stepOverConditionalPoint(fp.get)
  }

  @tailrec
  def stepOverConditionalPoint(fp: FunctionalControlPoint): Unit = fp match {
    case _: FunctionPoint => // nothing
    case condp: ConditionalPoint =>
      val currentPat = condp.irPoint.point.pat.name
      val currentBody = condp.irPoint.point.bodyIndex
      stepInto()
      val cp = controlPointIR
      if (controlPointIR.point.pat.name == currentPat && cp.point.bodyIndex == currentBody && !frame.bodyTable.isEmpty || !condp.thenBranch) {
        // we're in the same body and didn't fail => condition succeeded
        // or condition failed, but we were already at the else branch
        frontend.frontendPoint(cp) match {
          case Some(fp2) => stepOverConditionalPoint(fp2)
          case None => stepIntoFrontend()
        }
      } else {
        // condition failed and we were at the then branch => step to else branch
        stepIntoUntil{ () =>
          callStack.isEmpty || (frontend.frontendPoint(controlPointIR) match {
            case Some(ConditionalPoint(cond, false, _)) if cond.sourceObject == condp.cond.sourceObject => true
            case _ => false
          })
        }
        frontend.frontendPoint(cp) match {
          case Some(fp2) => stepOverConditionalPoint(fp2)
          case None => stepIntoFrontend()
        }
      }
  }

  override def controlPointFrontend: FunctionPoint = frontend.frontendPoint(controlPointIR) match {
    case Some(fp: FunctionPoint) => fp
    case o => throw new MatchError(s"Expected function point but got $o")
  }
  
  def currentFunction: FunctionDef = controlPointFrontend.fun
  def currentCodeSurrounding: String =
    controlPointFrontend.point.loc.sourceExcerpt(ExcerptRelativeRegion(3, 3)).linesColored
  def currentCodeFunction: String = {
    val fp = controlPointFrontend
    fp.point.loc.sourceExcerpt(ExcerptAbsoluteRegion(fp.fun.startIndex, fp.fun.endIndex)).linesColored
  }
  def currentCallStack: String =
    callStack.toString
  def currentBindings: String = {
    val table = frontend.frontendTable(controlPointFrontend, varsIR)
    val rowStrings = table.rows.map { row =>
      val sb = new StringBuilder
      sb += '['
      table.columns.foreach { col =>
        val ix = table.columnIndex(col)
        val v = row(ix)
        if (v != null) {
          sb ++= col
          sb += '='
          sb ++= v.toString
          sb ++= ", "
        }
      }
      if (sb.length() > 2) {
        sb.deleteCharAt(sb.length() - 1)
        sb.deleteCharAt(sb.length() - 1)
      }
      sb += ']'
      sb.toString()
    }
    rowStrings.size match {
      case 0 => "[]"
      case 1 => rowStrings.head
      case _ => rowStrings.mkString("{", ", ", "}")
    }
  }
}