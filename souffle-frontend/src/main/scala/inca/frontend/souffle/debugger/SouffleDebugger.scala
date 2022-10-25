package inca.frontend.souffle.debugger

import inca.backend.hints.DebugHints.SourceConstruct
import inca.backend.ir.Datalog
import inca.compiler.source.{ExcerptAbsoluteRegion, PaddedRegion, SourceLocation, SourceLocationList}
import inca.debugger.Value
import inca.debugger.redesign._
import inca.debugger.table.ImmutableTable
import inca.frontend.souffle.Syntax._
import inca.frontend.souffle.compiler.CompiledSouffleModule
import inca.runtime.db.DatabaseInput
import inca.util.Derivative

import scala.collection.mutable


class SouffleDebugger(compiled: CompiledSouffleModule, input: DatabaseInput) extends Debugger {
  super.initialize(compiled)
  super.initializeDatabaseRuntime(input)

  override def entry(name: Datalog.Name, bindings: ImmutableTable[Value]): Unit = {
    // super.updateExtensionalData(edits)
    super.entry(name, bindings)
    soufflePoint.getOrElse(stepInto())
  }

  protected def stepToSoufflePoint(step: () => Boolean): Boolean = {
    var b = step()
    while (b && !isFinished && !isAtBreakpoint) {
      if (soufflePoint.isDefined)
        return b
      b = step()
    }
    b
  }

  override def doStepInto(): Boolean = stepToSoufflePoint(() => stepIntoIR())
  override def doStepOver(): Boolean = stepToSoufflePoint(() => stepOverIR())
  override def doStepOut(): Boolean = stepToSoufflePoint(() => stepOutIR())

  override type Breakpoint = SouffleBreakPoint

  protected def convertBreakpoint(bp: SouffleBreakPoint): IRBreakpoint = bp match {
    case PatternEndBreakPoint(pred) => IRBreakpoint(EvaluationResult(pred, null))
    case InputBreakPoint(pred, before) =>
      val pattern = predicates(pred)
      val rule = pattern.bodies.head
      if (before) {
        IRBreakpoint(InRule(pred, null, null, RuleEvaluation(null, 0, rule.atoms), Nil))
      } else {
        IRBreakpoint(InRule(pred, null, null, RuleEvaluation(null, 0, Nil), Nil))
      }
    case InRuleBreakPoint(pred, ruleIdx, stm) =>
      val pattern = predicates(pred)
      val rule = pattern.bodies(ruleIdx)
      val rules = pattern.bodies.drop(ruleIdx + 1)
      if (stm.isEmpty) {
        IRBreakpoint(InRule(pred, null, null, RuleEvaluation(null, ruleIdx, rule.atoms), rules))
      } else {
        val atoms = rule.atoms.dropWhile(at => at.getHint(SourceConstruct.key) match {
          case Some(SourceConstruct(constr: Statement)) =>
            val found = constr.sourceObject == stm.get.sourceObject
            !found
          case _ => true
        })
        IRBreakpoint(InRule(pred, null, null, RuleEvaluation(null, ruleIdx, atoms), rules))
      }
  }

  override def addBreakpoint(bp: Breakpoint): Unit = breakpointHandler.addBreakpoint(convertBreakpoint(bp))
  override def removeBreakpoint(bp: Breakpoint): Unit = breakpointHandler.removeBreakpoint(convertBreakpoint(bp))

  private val soufflePointDeriv: Derivative[CallStack, Option[SouffleControlPoint]] =
    callStack.addDerivative[Option[SouffleControlPoint]](_ => None) { stack =>
      if (stack.isEmpty)
        None
      else
        computeSoufflePoint(stack.top)
    }

  def soufflePoint: Option[SouffleControlPoint] = soufflePointDeriv.value

  private def computeSoufflePoint(cp: EvaluationPoint): Option[SouffleControlPoint] = {
    val pattern = predicates(cp.pred)
    val rel = getRelationSignature(pattern).getOrElse(
      throw new IllegalArgumentException(
        s"Could not find signature for pattern ${cp.pred}"
      )
    )
    cp match {
      case InRule(_, _, _, RuleEvaluation(res, ruleIdx, atoms), _) if !res.isEmpty =>
        val body = pattern.bodies(ruleIdx)
        body.getHint(SourceConstruct.key) match {
          case Some(SourceConstruct((_: RuleHead, rule: RuleDefinition))) =>
            // we're in a rule body
            atoms match {
              case atom::_ =>
                atom.getHint(SourceConstruct.key) match {
                  case Some(SourceConstruct(constr: Statement)) =>
                    Some(InRulePoint(rel, rule, constr.sourceObject, cp))
                  case Some(SourceConstruct((_: RuleHead, _: Expression))) =>
                    None // param=argument equality constraint
                  case Some(SourceConstruct(_: Expression)) =>
                    None // result of expression such as calling built-in function
                  case constr =>
                    throw new IllegalArgumentException(s"Unexpected source construct $constr")
                }
              case Nil =>
                // rule end point
                Some(InRulePoint(rel, rule, SourceLocationList(rule.body.ss).sourceObject, cp))
            }
          case Some(SourceConstruct(in: Input)) =>
            if (atoms.size == body.atoms.size) {
              val inKeyword = new SourceLocation {}
              inKeyword.sourceLocFrom(in)
              inKeyword.endIndex = inKeyword.startIndex + ".input".length
//              val padRight = in.sourceCode.substring(".input".length)
              Some(InputPoint(rel, in, inKeyword.sourceObject, cp))
            } else if (atoms.isEmpty) {
              Some(InputPoint(rel, in, in.sourceObject, cp))
            } else {
              None
            }
          case _ => None
        }
      case EvaluationResult(_, _) =>
        compiled.inputs.get(rel.name.name) match {
          case Some(_) => None
          case None =>
            val rules = compiled.souffle.rules(rel.name.name)
            val sobj = SourceLocationList(rules.map(_._2)).sourceObject
            Some(PatternEndPoint(rel, sobj, cp))
        }
      case _ => None
    }
  }

  def currentDebuggerInfo(numOfRowsShown: Int = Int.MaxValue): String = {
    val sb = new mutable.StringBuilder
    sb ++= currentCallStack += '\n'
    sb ++= currentBindings(numOfRowsShown) += '\n'
    currentCodeFunction.lines().map("  |  " + _).forEach(line => sb ++= line += '\n')
    sb.toString()
  }

  def currentCallStack: String =
    getSouffleCallStack.mkString("[", ", ", "]")

  def getSouffleCallStack: List[Name] = callStack.frames.flatMap { fr =>
    predicates.get(fr.pred).flatMap(getRelationSignature).map(_.name)
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
        val sbPrior = new mutable.StringBuilder
        sbPrior ++= rel.sourceCode.stripTrailing() += '\n'
        for ((_, rule) <- prior) {
          sbPrior ++= rule.sourceCode.stripTrailing()
          sbPrior += '\n'
        }
        val sbAfter = new mutable.StringBuilder
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
