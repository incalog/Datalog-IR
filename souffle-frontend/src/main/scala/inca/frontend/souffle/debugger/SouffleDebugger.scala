package inca.frontend.souffle.debugger

import inca.backend.hints.DebugHints.SourceConstruct
import inca.backend.ir.Datalog
import inca.compiler.source.{ExcerptAbsoluteRegion, SourceLocation, SourceLocationList, SourceObject}
import inca.debugger.table.Table
import inca.debugger.{AfterList, AtListElem, AtomPoint, BeforeList, ControlPoint, Debugger, Value}
import inca.frontend.souffle.Syntax.{Expression, Input, Name, RuleDefinition, RuleHead, RuleSignature, SouffleContent, Statement}
import inca.frontend.souffle.compiler.CompiledSouffleModule
import truechange.EditScript

sealed trait SouffleControlPoint {
  val rel: RuleSignature
  val point: SourceObject
  def region: SourceLocation
}
case class OutOfRulePoint(rel: RuleSignature, point: SourceObject, irPoint: ControlPoint) extends SouffleControlPoint {
  override def region: SourceLocation = rel
}
case class InRulePoint(rel: RuleSignature, rule: RuleDefinition, point: SourceObject, irPoint: ControlPoint) extends SouffleControlPoint {
  override def region: SourceLocation = rule
}
case class InputPoint(rel: RuleSignature, input: Input, point: SourceObject, irPoint: ControlPoint) extends SouffleControlPoint {
  override def region: SourceLocation = input
}

class SouffleDebugger(compiled: CompiledSouffleModule) extends Debugger {
  super.initialize(compiled)

  def entry(name: Datalog.Name, edits: EditScript, bindings: Table[Value]): Unit = {
    super.updateExtensionalData(edits)
    super.entry(name, bindings)
  }

  def souffleStepInto(): Unit = {
    while (true) {
      stepInto()
      if (callStack.isEmpty || soufflePoint(controlPointIR).isDefined)
        return
    }
  }


  def soufflePoint(cp: ControlPoint): Option[SouffleControlPoint] = {
    val rel = getRelationSignature(cp.point.pat).getOrElse(throw new IllegalArgumentException(s"Could not find signature for pattern ${cp.point.pat.name}"))
    cp.point.bodies match {
      case BeforeList =>
        Some(OutOfRulePoint(rel, rel.name.sourceObject, cp))
      case at@AtListElem(_, _, point) =>
        at.elem.getHint(SourceConstruct.key) match {
          case Some(SourceConstruct((ruleHead: RuleHead, rule: RuleDefinition))) =>
            // we're in a rule body
            point.atoms match {
              case BeforeList => Some(InRulePoint(rel, rule, ruleHead.sourceObject, cp))
              case AtListElem(_, _, AtomPoint(atom)) => atom.getHint(SourceConstruct.key) match {
                case Some(SourceConstruct(constr: Statement)) => Some(InRulePoint(rel, rule, constr.sourceObject, cp))
                case Some(SourceConstruct((_: RuleHead, _: Expression))) => None // param=argument equality constraint
                case constr => throw new IllegalArgumentException(s"Unexpected source construct $constr")
              }
              case AfterList => Some(InRulePoint(rel, rule, SourceLocationList(rule.body).sourceObject, cp))
            }
          case Some(SourceConstruct(in: Input)) =>
            Some(InputPoint(rel, in, in.sourceObject, cp))
          case _ => None
        }
      case AfterList =>
        Some(OutOfRulePoint(rel, rel.sourceObject, cp))
    }
  }

  def currentDebuggerInfo: String = {
    val sb = new StringBuilder
    sb ++= currentCallStack += '\n'
    sb ++= currentBindings += '\n'
    currentCodeFunction.lines().map("  |  " + _).forEach( line =>
      sb ++= line += '\n'
    )
    sb.toString()
  }

  def currentCallStack: String =
    getSouffleCallStack.mkString("[", ", ", "]")

  def getSouffleCallStack: List[Name] = callStack.frames.flatMap { fr =>
    getRelationSignature(fr.cp.point.pat).map(_.name)
  }

  def currentBindings: String =
    varsIR.bindingsToString(_.toString)

  def currentCodeFunction: String = {
    val sp = soufflePoint(controlPointIR).getOrElse(throw new IllegalStateException())
    val region = ExcerptAbsoluteRegion(sp.region.startIndex, sp.region.endIndex)
    sp.point.loc.sourceExcerpt(region).linesColored
  }

  def getRelationSignature(pat: Datalog.Pattern): Option[RuleSignature] = pat.getHint(SourceConstruct.key) match {
    case Some(SourceConstruct(r: RuleSignature)) => Some(r)
    case _ => None
  }

  def getRuleDefinition(body: Datalog.Body): Option[(RuleHead, RuleDefinition)] = body.getHint(SourceConstruct.key) match {
    case Some(SourceConstruct((rh: RuleHead, rd: RuleDefinition))) => Some(rh -> rd)
    case _ => None
  }
}
