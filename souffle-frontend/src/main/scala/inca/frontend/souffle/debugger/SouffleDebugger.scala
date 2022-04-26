package inca.frontend.souffle.debugger

import inca.backend.hints.DebugHints.SourceConstruct
import inca.backend.ir.Datalog
import inca.compiler.source.ExcerptAbsoluteRegion
import inca.compiler.source.PaddedRegion
import inca.compiler.source.SourceLocation
import inca.compiler.source.SourceLocationList
import inca.compiler.source.SourceObject
import inca.debugger.table.ImmutableTable
import inca.debugger.AfterList
import inca.debugger.AtListElem
import inca.debugger.AtomPoint
import inca.debugger.BeforeList
import inca.debugger.CallStack
import inca.debugger.ControlPoint
import inca.debugger.Debugger
import inca.debugger.Value
import inca.frontend.souffle.compiler.CompiledSouffleModule
import inca.frontend.souffle.Syntax.Expression
import inca.frontend.souffle.Syntax.Input
import inca.frontend.souffle.Syntax.Name
import inca.frontend.souffle.Syntax.RuleDefinition
import inca.frontend.souffle.Syntax.RuleHead
import inca.frontend.souffle.Syntax.RuleSignature
import inca.frontend.souffle.Syntax.SouffleContent
import inca.frontend.souffle.Syntax.Statement
import inca.util.Derivative
import truechange.EditScript

sealed trait SouffleControlPoint {
  val rel: RuleSignature
  val point: SourceObject
  def region: SourceLocation
}
case class PatternEndPoint(rel: RuleSignature, point: SourceObject, irPoint: ControlPoint)
    extends SouffleControlPoint {
  override def region: SourceLocation = point.loc
}
case class InputPoint(rel: RuleSignature, in: Input, point: SourceObject, irPoint: ControlPoint)
    extends SouffleControlPoint {
  override def region: SourceLocation = in
}
case class InRulePoint(
    rel: RuleSignature,
    rule: RuleDefinition,
    point: SourceObject,
    irPoint: ControlPoint)
    extends SouffleControlPoint {
  override def region: SourceLocation = rule
}

class SouffleDebugger(compiled: CompiledSouffleModule) extends Debugger {
  super.initialize(compiled)

  override def entry(name: Datalog.Name, bindings: ImmutableTable[Value]): Unit = {
    // super.updateExtensionalData(edits)
    super.entry(name, bindings)
    soufflePoint.getOrElse(stepInto())
  }

  def stepInto(): Unit = {
    while (true) {
      stepIntoIR()
      if (callStack.isEmpty || soufflePoint.isDefined)
        return
    }
  }

  override def stepOver(): Unit = ???
  override def stepOut(): Unit = ???

  override type Breakpoint = Nothing
  override def addBreakpoint(bp: Breakpoint): Unit = ???
  override def removeBreakpoint(bp: Breakpoint): Unit = ???

  private val soufflePointDeriv: Derivative[CallStack, Option[SouffleControlPoint]] =
    callStack.addDerivative[Option[SouffleControlPoint]](_ => None) { stack =>
      if (stack.isEmpty)
        None
      else
        computeSoufflePoint(stack.top.cp)
    }

  def soufflePoint: Option[SouffleControlPoint] = soufflePointDeriv.value

  private def computeSoufflePoint(cp: ControlPoint): Option[SouffleControlPoint] = {
    val rel = getRelationSignature(cp.point.pat).getOrElse(
      throw new IllegalArgumentException(
        s"Could not find signature for pattern ${cp.point.pat.name}"
      )
    )
    cp.point.bodies match {
      case BeforeList =>
        None
      case at @ AtListElem(_, _, point) =>
        at.elem.getHint(SourceConstruct.key) match {
          case Some(SourceConstruct((ruleHead: RuleHead, rule: RuleDefinition))) =>
            // we're in a rule body
            point.atoms match {
              case BeforeList => Some(InRulePoint(rel, rule, ruleHead.sourceObject, cp))
              case AtListElem(_, _, AtomPoint(atom)) =>
                atom.getHint(SourceConstruct.key) match {
                  case Some(SourceConstruct(constr: Statement)) =>
                    Some(InRulePoint(rel, rule, constr.sourceObject, cp))
                  case Some(SourceConstruct((_: RuleHead, _: Expression))) =>
                    None // param=argument equality constraint
                  case Some(SourceConstruct(exp: Expression)) =>
                    None // result of expression such as calling built-in function
                  case constr =>
                    throw new IllegalArgumentException(s"Unexpected source construct $constr")
                }
              case AfterList =>
                Some(InRulePoint(rel, rule, SourceLocationList(rule.body.ss).sourceObject, cp))
            }
          case Some(SourceConstruct(in: Input)) =>
            at.point.atoms match {
              case BeforeList =>
                val inKeyword = new SourceLocation {}
                inKeyword.sourceLocFrom(in)
                inKeyword.endIndex = inKeyword.startIndex + ".input".length
                val padRight = in.sourceCode.substring(".input".length)
                Some(InputPoint(rel, in, inKeyword.sourceObject, cp))
              case AfterList => Some(InputPoint(rel, in, in.sourceObject, cp))
              case _ => None
            }
          case _ => None
        }
      case AfterList =>
        compiled.inputs.get(rel.name.name) match {
          case Some(_) => None
          case None =>
            val rules = compiled.souffle.rules(rel.name.name)
            val sobj = SourceLocationList(rules.map(_._2)).sourceObject
            Some(PatternEndPoint(rel, sobj, cp))
        }
    }
  }

  def currentDebuggerInfo(numOfRowsShown: Int = Int.MaxValue): String = {
    val sb = new StringBuilder
    sb ++= currentCallStack += '\n'
    sb ++= currentBindings(numOfRowsShown) += '\n'
    currentCodeFunction.lines().map("  |  " + _).forEach(line => sb ++= line += '\n')
    sb.toString()
  }

  def currentCallStack: String =
    getSouffleCallStack.mkString("[", ", ", "]")

  def getSouffleCallStack: List[Name] = callStack.frames.flatMap { fr =>
    getRelationSignature(fr.cp.point.pat).map(_.name)
  }

  def currentBindings(numOfRowsShown: Int): String =
    varsIR.bindingsToString(_.toString, numOfRowsShown)

  def currentCodeFunction: String = {
    val sp = soufflePoint.getOrElse(throw new IllegalStateException())
    val excerptRegion = ExcerptAbsoluteRegion(sp.region.startIndex, sp.region.endIndex)
    val contextualRegion = sp match {
      case InRulePoint(rel, rule, _, _) =>
        val rules = compiled.souffle.rules(rel.name.name)
        val ix = rules.indexWhere(_._2.sourceObject == rule.sourceObject)
        val (prior, thisAfter) = rules.splitAt(ix)
        val after = thisAfter.tail
        val sbPrior = new StringBuilder
        sbPrior ++= rel.sourceCode.stripTrailing() += '\n'
        for ((_, rule) <- prior) {
          sbPrior ++= rule.sourceCode.stripTrailing()
          sbPrior += '\n'
        }
        val sbAfter = new StringBuilder
        for ((_, rule) <- after) {
          sbAfter ++= rule.sourceCode.stripTrailing()
          sbAfter += '\n'
        }
        PaddedRegion(sbPrior.toString(), excerptRegion, "\n" + sbAfter.toString())
      case InputPoint(rel, _, _, _) =>
        PaddedRegion(rel.sourceCode.stripTrailing() + '\n', excerptRegion, "")
      case PatternEndPoint(rel, _, _) =>
        PaddedRegion(rel.sourceCode.stripTrailing() + '\n', excerptRegion, "")
    }
    sp.point.loc.sourceExcerpt(contextualRegion).linesColored
  }

  def getRelationSignature(pat: Datalog.Pattern): Option[RuleSignature] =
    pat.getHint(SourceConstruct.key) match {
      case Some(SourceConstruct(r: RuleSignature)) => Some(r)
      case _ => None
    }

  def getRuleDefinition(body: Datalog.Body): Option[(RuleHead, RuleDefinition)] =
    body.getHint(SourceConstruct.key) match {
      case Some(SourceConstruct((rh: RuleHead, rd: RuleDefinition))) => Some(rh -> rd)
      case _ => None
    }
}
