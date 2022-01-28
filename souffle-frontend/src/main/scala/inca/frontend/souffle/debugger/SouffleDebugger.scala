package inca.frontend.souffle.debugger

import inca.backend.hints.DebugHints.SourceConstruct
import inca.backend.ir.Datalog
import inca.compiler.source.{SourceLocationList, SourceObject}
import inca.debugger.{AfterList, AtListElem, AtomPoint, BeforeList, ControlPoint, Debugger}
import inca.frontend.souffle.Syntax.{Expression, RuleDefinition, RuleHead, RuleSignature, Statement}
import inca.frontend.souffle.compiler.CompiledSouffleModule

case class SouffleControlPoint(rel: RuleSignature, rule: Option[RuleDefinition], point: SourceObject, irPoint: ControlPoint)

class SouffleDebugger(compiled: CompiledSouffleModule) extends Debugger {
  super.initialize(compiled)

  def soufflePoint(cp: ControlPoint): Option[SouffleControlPoint] = {
    val rel = getRelationSignature(cp.point.pat).getOrElse(throw new IllegalArgumentException(s"Could not find signature for pattern ${cp.point.pat.name}"))
    cp.point.bodies match {
      case BeforeList =>
        Some(SouffleControlPoint(rel, None, rel.name.sourceObject, cp))
      case at@AtListElem(elems, ix, point) =>
        val (ruleHead, rule) = getRuleDefinition(at.elem).getOrElse(throw new IllegalArgumentException(s"Could not find rule for body ${at.elem} in ${rel.name}"))
        point.atoms match {
          case BeforeList =>
            Some(SouffleControlPoint(rel, Some(rule), ruleHead.sourceObject, cp))
          case AtListElem(_, _, AtomPoint(atom)) => atom.getHint(SourceConstruct.key) match {
            case Some(SourceConstruct(constr: Statement)) =>
              Some(SouffleControlPoint(rel, Some(rule), constr.sourceObject, cp))
            case Some(SourceConstruct((_: RuleHead, _: Expression))) =>
              None
            case constr => throw new IllegalArgumentException(s"Unexpected source construct $constr")
          }
          case AfterList =>
            Some(SouffleControlPoint(rel, Some(rule), SourceLocationList(rule.body).sourceObject, cp))
        }
      case AfterList =>
        Some(SouffleControlPoint(rel, None, rel.sourceObject, cp))
    }
  }


  def getRelationSignature(pat: Datalog.Pattern): Option[RuleSignature] = pat.getHint(SourceConstruct.key) match {
    case Some(SourceConstruct(r: RuleSignature)) => Some(r)
    case _ => None
  }

  def getRuleDefinition(pat: Datalog.Body): Option[(RuleHead, RuleDefinition)] = pat.getHint(SourceConstruct.key) match {
    case Some(SourceConstruct((rh: RuleHead, rd: RuleDefinition))) => Some(rh -> rd)
    case _ => None
  }
}
