package inca.frontend.souffle.debugger

import inca.backend.hints.DebugHints.SourceConstruct
import inca.backend.ir.Datalog
import inca.compiler.source.ExcerptAbsoluteRegion
import inca.compiler.source.PaddedRegion
import inca.compiler.source.SourceLocation
import inca.compiler.source.SourceLocationList
import inca.debugger
import inca.debugger._
import inca.frontend.souffle.compiler.CompiledSouffleModule
import inca.frontend.souffle.Syntax._
import inca.runtime.db.DatabaseInput
import inca.util.Derivative
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class SouffleDebugger(compiled: CompiledSouffleModule, input: DatabaseInput) extends Debugger {
  super.initialize(compiled)
  super.initializeDatabaseRuntime(input)

  override def entry(pred: Predicate, args: ValueTable): Unit = {
    // super.updateExtensionalData(edits)
    super.entry(pred, args)
    // soufflePoint.getOrElse(stepInto())
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

  private val _souffleControlTrace: ListBuffer[SouffleControlPoint] = ListBuffer.empty
  def souffleControlTrace: Seq[SouffleControlPoint] = _souffleControlTrace.toSeq
  def stepped(): Unit = soufflePoint.foreach(_souffleControlTrace += _)

  override def doStepInto(shortCircuit: Boolean = false): Boolean =
    stepToSoufflePoint(() => doStepIntoIR(shortCircuit))
  override def doStepOver(shortCircuit: Boolean = false): Boolean =
    stepToSoufflePoint(() => doStepOverIR(shortCircuit))
  override def doStepOut(shortCircuit: Boolean = false): Boolean =
    stepToSoufflePoint(() => doStepOutIR(shortCircuit))

  override type Breakpoint = SouffleBreakPoint

  protected def lowerBreakpoint(bp: SouffleBreakPoint): IRBreakpoint = bp match {
    case PatternEndBreakPoint(pred) => IRBreakpoint(QueryResult(pred, null, null))
    case InputBreakPoint(pred, before) =>
      val pattern = preds(pred)
      val rule = pattern.bodies.head
      val params = predParams(pred)
      if (before) {
        IRBreakpoint(
          Subquery(pred, null, null, null, Seq(Rule(pred, params, rule.atoms.map(Atom)))))
      } else {
        IRBreakpoint(
          debugger.Subquery(pred, null, null, null, Seq(debugger.Rule(pred, params, Nil))))
      }
    case InRuleBreakPoint(pred, ruleIdx, stm) =>
      val pattern = preds(pred)
      val rule = pattern.bodies(ruleIdx)
      val params = predParams(pred)
      val ruleEvals = pattern.bodies.drop(ruleIdx + 1).map { r =>
        debugger.Rule(pred, params, r.atoms.map(Atom))
      }
      if (stm.isEmpty) {
        IRBreakpoint(
          debugger.Subquery(
            pred,
            null,
            null,
            null,
            debugger.Rule(pred, params, rule.atoms.map(Atom)) +: ruleEvals))
      } else {
        val atoms = rule.atoms.dropWhile(at =>
          at.getHint(SourceConstruct.key) match {
            case Some(SourceConstruct(constr: Statement)) =>
              val found = constr.sourceObject == stm.get.sourceObject
              !found
            case _ => true
          })
        IRBreakpoint(
          debugger.Subquery(
            pred,
            null,
            null,
            null,
            debugger.Rule(pred, params, rule.atoms.map(Atom)) +: ruleEvals))
      }
  }

  override def addBreakpoint(bp: Breakpoint): Unit =
    breakpointHandler.addBreakpoint(lowerBreakpoint(bp))
  override def removeBreakpoint(bp: Breakpoint): Unit =
    breakpointHandler.removeBreakpoint(lowerBreakpoint(bp))
  override def clearBreakpoints(): Unit = breakpointHandler.clearBreakpoints()

  private val soufflePointDeriv: Derivative[QueryStack, Option[SouffleControlPoint]] =
    queryStack.addDerivative[Option[SouffleControlPoint]](_ => None) { stack =>
      if (stack.isEmpty)
        None
      else
        lift(stack.top)
    }

  def soufflePoint: Option[SouffleControlPoint] = soufflePointDeriv.value

  private def lift(query: Query): Option[SouffleControlPoint] = {
    val pattern = preds(query.pred)
    val rel = getRelationSignature(pattern).getOrElse(
      throw new IllegalArgumentException(
        s"Could not find signature for pattern ${query.pred}"
      )
    )
    query match {
      case Subquery(_, _, _, sup, Rule(_, _, atoms) +: rules) if !sup.isEmpty =>
        val body = pattern.bodies(pattern.bodies.size - rules.size - 1)
        body.getHint(SourceConstruct.key) match {
          case Some(SourceConstruct((_: RuleHead, rule: RuleDefinition))) =>
            // we're in a rule body
            atoms match {
              case Atom(atom) +: _ =>
                atom.getHint(SourceConstruct.key) match {
                  case Some(SourceConstruct(constr: Statement)) =>
                    Some(InRulePoint(rel, rule, constr.sourceObject, query))
                  case Some(SourceConstruct((_: RuleHead, _: Expression))) =>
                    None // param=argument equality constraint
                  case Some(SourceConstruct(_: Expression)) =>
                    None // result of expression such as calling built-in function
                  case constr =>
                    throw new IllegalArgumentException(s"Unexpected source construct $constr")
                }
              case _ => None
            }
          case Some(SourceConstruct(in: Input)) =>
            if (atoms.size == body.atoms.size) {
              atoms.head match {
                case Atom(Datalog.ExtensionalCall(_, _, _)) =>
                  val inKeyword = new SourceLocation {}
                  inKeyword.sourceLocFrom(in)
                  inKeyword.endIndex = inKeyword.startIndex + ".input".length
                  //              val padRight = in.sourceCode.substring(".input".length)
                  Some(InputPoint(rel, in, inKeyword.sourceObject, query))
                case AtomResult(_, _) => None
              }
            } else if (atoms.isEmpty) {
              Some(InputPoint(rel, in, in.sourceObject, query))
            } else {
              None
            }
          case _ => None
        }
      case Subquery(_, _, _, sup, RuleResult(_) +: rules) =>
        val body = pattern.bodies(pattern.bodies.size - rules.size - 1)
        getRuleDefinition(body) match {
          case Some((_, rule)) =>
            Some(InRulePoint(rel, rule, SourceLocationList(Seq(rule)).sourceObject, query))
          case None => None
        }
      case Subquery(_, _, _, _, Nil) =>
        compiled.inputs.get(rel.name.name) match {
          case Some(_) => None
          case None =>
            val rules = compiled.souffle.rules(rel.name.name)
            val sobj = SourceLocationList(rules.map(_._2)).sourceObject
            Some(PatternEndPoint(rel, sobj, query))
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

  def getSouffleCallStack: List[Name] = queryStack.frames.flatMap { fr =>
    preds.get(fr.pred).flatMap(getRelationSignature).map(_.name)
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
