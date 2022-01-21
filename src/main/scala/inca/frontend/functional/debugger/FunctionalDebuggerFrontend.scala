package inca.frontend.functional.debugger

import inca.backend.hints.DebugHints.SourceConstruct
import inca.backend.ir.Datalog
import inca.compiler.source.{ExcerptAbsoluteRegion, ExcerptRelativeRegion, SourceLocation, SourceObject}
import inca.debugger.table.{SimpleTable, Table}
import inca.debugger.{AfterList, AtListElem, AtomPoint, BeforeList, BodyPoint, ControlPoint, Debugger, DebuggerFrontend, ScalaValue, Value}
import inca.frontend.functional.core.{BaseApply, BaseApplyInfix, BaseLit, Call, Expression, FunctionDef, If, Lambda, Let, Match, Module, NoneExp, SetComprehension, SetExp, SetFold, SetMember, SomeExp, Tuple, TypeCast, Var}

final class FunctionalDebugger extends Debugger {
  override val frontend: FunctionalDebuggerFrontend = new FunctionalDebuggerFrontend

  def currentFunction: FunctionDef = controlPointFrontend.fun
  def currentCodeSurrounding: String =
    controlPointFrontend.point.loc.sourceExcerpt(ExcerptRelativeRegion(3, 3)).linesColored
  def currentCodeFunction: String = {
    val fp = controlPointFrontend
    fp.point.loc.sourceExcerpt(ExcerptAbsoluteRegion(fp.fun.startIndex, fp.fun.endIndex)).linesColored
  }
}

class FunctionalDebuggerFrontend extends DebuggerFrontend {

  override type FrontendPoint = FunctionPoint
  override type FrontendValue = Value

  var module: Module = _

  override def initialize(mod: Datalog.Module): Unit = mod.getHint(SourceConstruct.key) match {
    case Some(SourceConstruct(m: Module)) => this.module = m
    case h => throw new IllegalArgumentException(s"Cannot create functional debugger frontend for ${mod.name}: No functional module source construct found $h")
  }

  def getFunction(pat: Datalog.Pattern): Option[FunctionDef] = pat.getHint(SourceConstruct.key) match {
    case Some(SourceConstruct(f: FunctionDef)) => Some(f)
    case _ => None
  }

  override def frontendPoint(cp: ControlPoint): Option[FunctionPoint] = {
    val patPoint = cp.point
    val fun = getFunction(patPoint.pat).getOrElse(return None)
    patPoint.bodies match {
      case BeforeList =>
        // start of function
        Some(FunctionPoint(fun, fun.sourceObject, cp))
      case AtListElem(_, _, BodyPoint(_, atoms)) => atoms match {
        case BeforeList => None
        case AtListElem(_, _, AtomPoint(atom)) =>
          atom.getHint(SourceConstruct.key) match {
            case Some(SourceConstruct(constr: Expression)) =>
              expressionPoint(constr).map(FunctionPoint(fun, _, cp))
            case Some(SourceConstruct((let: Let, v: String))) =>
              let.names.find(_.name == v).map(p => FunctionPoint(fun, p.sourceObject, cp))
            case Some(SourceConstruct((let: If, thenBranch: Boolean))) =>
              None
            case None =>
              None
          }
        case AfterList => None
      }
      case AfterList =>
        // end of function
        None
    }
  }

  private def expressionPoint(exp: Expression): Option[SourceObject] = exp match {
    case _: Var | _: Tuple | _: BaseLit | _: NoneExp | _: SomeExp | _: SetExp => None
    case _ => Some(exp.sourceObject)
  }

  override def frontendTable(fp: FunctionPoint, bound: Table[Value]): Table[Value] = {
    val vars = fp.vars.map(_.name).toList.sorted
    var myVars = Table.empty[Value](vars)
    for (row <- bound.rows) {
      val vals = vars.map { v =>
        val ix = bound.columnIndex(v)
        if (ix < 0)
          ScalaValue(null)
        else
          row(ix)
      }
      myVars = myVars.addRow(vals)
    }
    myVars
  }

}
