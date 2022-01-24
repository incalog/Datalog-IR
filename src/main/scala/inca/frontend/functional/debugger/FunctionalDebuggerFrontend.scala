package inca.frontend.functional.debugger

import inca.backend.hints.DebugHints.SourceConstruct
import inca.backend.ir.Datalog
import inca.compiler.CompiledModule
import inca.compiler.source.SourceObject
import inca.debugger.table.Table
import inca.debugger._
import inca.frontend.functional.compiler.CompiledFunctionalModule
import inca.frontend.functional.core.{BaseLit, Expression, FunctionDef, If, Let, Module, NoneExp, SetExp, SomeExp, Tuple, Var}


class FunctionalDebuggerFrontend(debugger: Debugger) extends DebuggerFrontend {

  override type FrontendPoint = FunctionalControlPoint
  override type FrontendValue = Value

  def getFunction(pat: Datalog.Pattern): Option[FunctionDef] = pat.getHint(SourceConstruct.key) match {
    case Some(SourceConstruct(f: FunctionDef)) => Some(f)
    case _ => None
  }

  override def frontendPoint(cp: ControlPoint): Option[FunctionalControlPoint] = {
    val patPoint = cp.point
    val fun = getFunction(patPoint.pat).getOrElse(return None)
    patPoint.bodies match {
      case BeforeList =>
        // start of function
        Some(FunctionPoint(fun, fun.name.sourceObject, cp))
      case AtListElem(_, _, BodyPoint(_, atoms)) => atoms match {
        case BeforeList => None
        case AtListElem(_, _, AtomPoint(atom)) =>
          atom.getHint(SourceConstruct.key) match {
            case Some(SourceConstruct(constr: Expression)) =>
              expressionPoint(constr).map(FunctionPoint(fun, _, cp))
            case Some(SourceConstruct((let: Let, v: String))) =>
              let.names.find(_.name == v).map(p => FunctionPoint(fun, p.sourceObject, cp))
            case Some(SourceConstruct((cond: If, thenBranch: Boolean))) =>
              Some(ConditionalPoint(cond, thenBranch, cp))
            case _ =>
              None
          }
        case AfterList => None
      }
      case AfterList =>
        // end of function
        Some(FunctionPoint(fun, fun.sourceObject, cp))
//        // => show call point once again
//        debugger.frames.lift(1) match {
//          case None => None
//          case Some(fr) => fr.cp.point.bodies match {
//            case BeforeList | AfterList => None
//            case AtListElem(_, _, BodyPoint(_, atoms)) => atoms match {
//              case BeforeList | AfterList => None
//              case AtListElem(_, _, AtomPoint(atom)) => atom.getHint(SourceConstruct.key) match {
//                case Some(SourceConstruct(call: Call)) => Some(FunctionPoint(fun, call.sourceObject, cp))
//                case _ => None
//              }
//            }
//          }
//        }
    }
  }

  private def expressionPoint(exp: Expression): Option[SourceObject] = exp match {
    case _: Var | _: Tuple | _: BaseLit | _: NoneExp | _: SomeExp | _: SetExp => None
    case _ => Some(exp.sourceObject)
  }

  override def frontendTable(fp: FunctionalControlPoint, bound: Table[Value]): Table[Value] = fp match {
    case fp: FunctionPoint =>
      var vars = fp.vars.map(_.name).toList.sorted.distinct
      if (fp.isFunctionExit)
        vars :+= fp.irPoint.point.pat.params.last.name
      var myVars = Table.empty[Value](vars)
      for (row <- bound.rows) {
        val vals = vars.map { v =>
          val ix = bound.columnIndex(v)
          if (ix < 0)
            null
          else
            row(ix)
        }
        myVars = myVars.addRow(vals)
      }
      myVars
    case _: ConditionalPoint =>
      Table.empty
  }
}
