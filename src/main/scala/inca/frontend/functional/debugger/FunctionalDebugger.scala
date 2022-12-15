package inca.frontend.functional.debugger

import inca.backend.hints.DataHints
import inca.backend.hints.DebugHints.SourceConstruct
import inca.backend.hints.MagicSetHints
import inca.backend.hints.OptimizationHints.KeepPattern
import inca.backend.ir.Datalog
import inca.backend.transform.magic.demand.DemandTransformation.demandPatternExtensionalPrefix
import inca.compiler.source.ExcerptAbsoluteRegion
import inca.compiler.source.ExcerptRelativeRegion
import inca.compiler.source.SourceObject
import inca.compiler.CompiledDatalogModule
import inca.compiler.CompiledModule
import inca.debugger._
import inca.debugger.redesign_old.BeforeRule
import inca.debugger.redesign_old.CallStack
import inca.debugger.redesign_old.Debugger
import inca.debugger.redesign_old.EvaluationPoint
import inca.debugger.redesign_old.EvaluationResult
import inca.debugger.redesign_old.InRule
import inca.debugger.redesign_old.PredicateEntry
import inca.debugger.redesign_old.RuleEvaluation
import inca.debugger.table.ImmutableTable
import inca.frontend.functional.compiler.CompiledFunctionalModule
import inca.frontend.functional.core.BaseLit
import inca.frontend.functional.core.Expression
import inca.frontend.functional.core.FunctionDef
import inca.frontend.functional.core.If
import inca.frontend.functional.core.Let
import inca.frontend.functional.core.Match
import inca.frontend.functional.core.Name
import inca.frontend.functional.core.NoneExp
import inca.frontend.functional.core.Pattern
import inca.frontend.functional.core.SetExp
import inca.frontend.functional.core.SomeExp
import inca.frontend.functional.core.Tuple
import inca.frontend.functional.core.Var
import inca.runtime.data.MockURI
import inca.runtime.data.WrappedURI
import inca.runtime.db.DatabaseInput
import inca.util.Derivative
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuples
import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import truechange.JVMURI
import truechange.URI
import truediff.Diffable

object FunctionalDebugger {
  def prepareModule(module: CompiledFunctionalModule): CompiledModule = {
    val pats = module.ir.pats.map { pat =>
      val p = pat.copy().withHints(pat)
      // constructors and selectors may not be inlined, so that we can read values from the database
      if (p.hasHint(DataHints.ConstructorKey) || p.hasHint(DataHints.SelectorKey))
        p.addHint(KeepPattern)
      p
    }
    val m = module.ir.copy(pats = pats)
    CompiledDatalogModule(m, module.dataModel, module.options)
  }

}

final class FunctionalDebugger(val funmodule: CompiledFunctionalModule) extends Debugger {

  super.initialize(FunctionalDebugger.prepareModule(funmodule))
  super.initializeDatabaseRuntime(DatabaseInput.empty)

  // skip bodies representing else-branches when corresponding then-branch was chosen
  private var skipElseBranches: List[mutable.Set[SourceObject]] = List()
  // skip everything until the atoms corresponding to the body of the else-branch/case when the else-branch/case was chosen
  private var skipAheadTo: List[Option[SkipAhead]] = List()
  // skip the bodies that handle the other patterns of a pattern match if we found a matching pattern
  private var skipAlternativePatterns: List[mutable.Map[SourceObject, Set[SourceObject]]] = List()

  sealed trait SkipAhead {
    def stop(s: SourceConstruct[_]): Boolean
  }
  case class SkipToElse(ifSource: SourceObject) extends SkipAhead {
    override def stop(s: SourceConstruct[_]): Boolean = s match {
      case SourceConstruct((cond: If, false)) => cond.sourceObject == ifSource
      case _ => false
    }
  }
  case class SkipToPat(matchSource: SourceObject, patSource: SourceObject) extends SkipAhead {
    override def stop(s: SourceConstruct[_]): Boolean = s match {
      case SourceConstruct((m: Match, p: Pattern)) =>
        m.sourceObject == matchSource && p.sourceObject == patSource
      case _ => false
    }
  }

  private var uris: Map[URI, Diffable] = Map()

  private val _functionalControlTrace: ListBuffer[FunctionalControlPoint] = ListBuffer.empty
  def functionalControlTrace: Seq[FunctionalControlPoint] = _functionalControlTrace.toSeq
  def stepped(): Unit = currentFunctionalPoint.foreach(_functionalControlTrace += _)

  def getFunction(pat: String): Option[FunctionDef] =
    predicates.get(pat).flatMap(getFunction)
  def getFunction(pat: Datalog.Pattern): Option[FunctionDef] =
    pat.getHint(SourceConstruct.key) match {
      case Some(SourceConstruct(f: FunctionDef)) => Some(f)
      case _ => None
    }

  private val functionalPointDeriv: Derivative[CallStack, Option[FunctionalControlPoint]] =
    callStack.addDerivative[Option[FunctionalControlPoint]](_ => None) { stack =>
      if (stack.isEmpty)
        None
      else {
        val cp = stack.top
        val fp = getFunction(cp.pred) match {
          case Some(fun) =>
            cp match {
              case PredicateEntry(pred, argBindings, predResult) => None
              case BeforeRule(pred, argBindings, predResult, rules) =>
                if (predicates(cp.pred).bodies.size == rules.size)
                  Some(FunctionPoint(fun, fun.name.sourceObject, cp))
                else
                  None
              case cp @ InRule(pred, argBindings, predResult, current, remainingRules)
                  if current.ruleResult.isEmpty =>
                None
              case cp @ InRule(pred, argBindings, predResult, current, remainingRules) =>
                current.atoms match {
                  case Nil => None
                  case atom :: _ =>
                    atom.getHint(SourceConstruct.key) match {
                      case Some(SourceConstruct(constr: Expression)) =>
                        expressionPoint(constr).map(FunctionPoint(fun, _, cp))
                      case Some(SourceConstruct((let: Let, v: String))) =>
                        let.names.find(_.name == v).map(p => FunctionPoint(fun, p.sourceObject, cp))
                      case Some(SourceConstruct((m: Match, constr: Pattern))) =>
                        Some(MatchPoint(fun, m, constr, cp))
                      case Some(SourceConstruct((cond: If, thenBranch: Boolean))) =>
                        Some(ConditionPoint(fun, cond, thenBranch, cp))
                      case _ =>
                        None
                    }
                }
              case EvaluationResult(pred, predResult) =>
                Some(FunctionPoint(fun, fun.sourceObject, cp))
            }
          case None => None
        }
        fp
      }
    }

  @inline
  def currentFunctionalPoint: Option[FunctionalControlPoint] = functionalPointDeriv.value

  private def expressionPoint(exp: Expression): Option[SourceObject] = exp match {
    case _: Var | _: Tuple | _: BaseLit | _: NoneExp | _: SomeExp | _: SetExp => None
    case _ => Some(exp.sourceObject)
  }

  def frontendTable(
      fp: FunctionalControlPoint,
      bound: ImmutableTable[Value]
    ): ImmutableTable[Value] = {
    var vars = fp.vars.map(_.name).toList.sorted.distinct
    if (fp.isFunctionExit)
      vars :+= predicates(fp.irPoint.pred).params.last.name
    val rows = for (row <- bound.entries) yield {
      vars.map { v =>
        val ix = bound.columnIndex(v)
        if (ix < 0)
          null
        else
          row.lift(ix).orNull
      }
    }
    ImmutableTable[Value](vars, rows)
  }

  def varsFrontEnd: ImmutableTable[Value] =
    frontendTable(controlPointFrontend, varsIR)

  def entry(mainFun: String, args: meta.Term*): Unit = {
    val (vals, debugVals) = args.map { t =>
      val syntax = s"{import ${atomOps.getDefinitionObjSym}.${module.name}._; ${t.syntax}}"
      atomOps.compileAndLoadScala[Any](syntax) match {
        case diff: Diffable =>
          atomOps.runtime.engine.delayUpdatePropagation { () =>
            atomOps.runtime.db.processDatabaseInput(DatabaseInput(diff.loadEdits, Map(), Map()))
          }
          diff.foreachTree(t => uris += t.uri -> t)
          (diff.uri, URIValue(diff.uri))
        case v =>
          (v, ScalaValue(v))
      }
    }.unzip
    atomOps.runtime.db.insert(
      demandPatternExtensionalPrefix + mainFun,
      Tuples.flatTupleOf(vals: _*))

    val pattern = predicates(mainFun)
    val adorn = pattern.hints(MagicSetHints.Main.key).asInstanceOf[MagicSetHints.Main].adorn
    val inputParams = pattern.params.zip(adorn).filter(_._2).map(_._1.name)
    val inputTable = ImmutableTable[Value](inputParams, Seq(debugVals))
    super.entry(mainFun, inputTable)
    if (currentFunctionalPoint.isEmpty)
      stepInto() // step to BeforeRule
  }

  def getFunctionalCallStack: List[Name] = callStack.frames.flatMap { ep =>
    getFunction(ep.pred).map(_.name)
  }

  protected def stepToFunctionalPoint(step: () => Boolean): Boolean = {
    var b = step()
    while (b && !isFinished && !isAtBreakpoint) {
      if (currentFunctionalPoint.isDefined) {
        stepOverConditionPoint(currentFunctionalPoint.get)
        return b
      }
      b = step()
    }
    b
  }

  override protected def doStepInto(): Boolean =
    stepToFunctionalPoint(() => stepIntoIR())

  override protected def doStepOver(): Boolean =
    stepToFunctionalPoint(() => stepOverIR())

  override protected def doStepOut(): Boolean =
    stepToFunctionalPoint(() => stepOutIR())

  override type Breakpoint = FunctionalBreakpoint
  override def addBreakpoint(bp: Breakpoint): Unit = {
    val irBPs = FunctionalBreakpoint.convert(bp)(predicates)
    irBPs.foreach(breakpointHandler.addBreakpoint)
  }
  override def removeBreakpoint(bp: Breakpoint): Unit = {
    val irBPs = FunctionalBreakpoint.convert(bp)(predicates)
    irBPs.foreach(breakpointHandler.removeBreakpoint)
  }

  @tailrec
  def stepOverConditionPoint(fp: FunctionalControlPoint): Unit = fp match {
    case _: FunctionPoint | _: MatchPoint => // nothing
    case condp: ConditionPoint =>
      val conditionedPattern = condp.irPoint.pred
      val conditionedBody = condp.irPoint.current.ruleIdx
      stepIntoIR()

      if (!condp.thenBranch) {
        // we're at the else branch, continue
      } else {
        callStack.top match {
          case InRule(`conditionedPattern`, _, _, RuleEvaluation(res, `conditionedBody`, _), _)
              if !res.isEmpty =>
            // we're in the same body and didn't fail => condition succeeded
            if (!condp.fun.isRelation)
              skipElseBranches.head += condp.cond.sourceObject
          case _ =>
            // condition failed and we were at the then branch => step to else branch
            if (!condp.fun.isRelation)
              skipAheadTo = Some(SkipToElse(condp.point)) :: skipAheadTo.tail
        }
      }
      currentFunctionalPoint match {
        case Some(fp2) => stepOverConditionPoint(fp2)
        case None => doStepInto()
      }
  }

  private def skipBody(body: Datalog.Body): Boolean = {
    val skipElse = skipElseBranches.head
    val skipMatches = skipAlternativePatterns.head
    if (skipElse.isEmpty && skipMatches.isEmpty)
      return false
    body.atoms.exists { a =>
      a.getHint(SourceConstruct.key) match {
        case Some(SourceConstruct((cond: If, false))) => skipElse.contains(cond.sourceObject)
        case Some(SourceConstruct((ma: Match, pat: Pattern))) =>
          val skipPats = skipMatches.getOrElse(ma.sourceObject, Set())
          skipPats.contains(pat.sourceObject)
        case _ => false
      }
    }
  }

  @tailrec
  private def doSkipAheadTo(skip: SkipAhead): Unit = {
    val top = callStack.top
    val atom = top match {
      case InRule(_, _, _, RuleEvaluation(_, _, atom :: _), _) => Some(atom)
      case _ => None
    }
    val stop = atom.exists(
      _.getHint(SourceConstruct.key).exists(h => skip.stop(h.asInstanceOf[SourceConstruct[_]])))
    if (!stop && !isFinished && atom.isDefined) {
      stepOverIR()
      doSkipAheadTo(skip)
    }
  }

  override def stepIntoIR(): Boolean = {
    val top = callStack.top
    top match {
      case PredicateEntry(_, _, _) =>
        fpIntoPredicate(top)
      case br @ BeforeRule(_, _, _, rules) =>
        if (rules.nonEmpty) fpIntoFirstRule(br)
        else outofPredicate(top)
      case ir @ InRule(_, _, _, RuleEvaluation(_, _, atoms), rules) =>
        if (atoms.nonEmpty) fpNextAtom(top)
        else if (rules.nonEmpty) fpNextRule(ir)
        else lastRule(top)
      case EvaluationResult(_, _) =>
        fpEvalResult(top)
    }
    true
  }

  private def fpIntoFirstRule(ep: BeforeRule): Unit = {
    val BeforeRule(p, argBindings, predResult, rule :: rules) = ep
    val skip = skipBody(rule)
    if (skip) {
      val next =
        InRule(p, argBindings, predResult, RuleEvaluation(argBindings, 0, rule.atoms), rules)
      callStack.update(next)
      stepIntoIR()
      if (rules.isEmpty)
        lastRule(next)
      else
        nextRule(next)
    } else {
      super.intoFirstRule(ep)
      skipAheadTo.head.foreach {
        // needed? stepOverIR()
        doSkipAheadTo
      }
    }
  }

  @tailrec
  private def fpNextRule(ep: InRule): Unit = {
    val InRule(_, _, _, _, rule :: rules) = ep
    val skip = skipBody(rule)
    if (skip) {
      val next = ep.copy(remainingRules = rules)
      if (rules.isEmpty)
        lastRule(next)
      else
        fpNextRule(next)
    } else {
      super.nextRule(ep)
      breakpointHandler.withBreakpoints(Seq.empty) {
        skipAheadTo.head.foreach {
          doSkipAheadTo
        }
      }
    }
  }

  private def fpNextAtom(cp: EvaluationPoint): Unit = {
    val InRule(p, argBindings, predResult, RuleEvaluation(ruleResult, ruleIdx, atoms), rules) = cp
    val atomsHead = atoms.head
    val atomsTail = atoms.tail
    atomsHead match {
      case call: Datalog.Call =>
        val pattern = predicates(call.name)
        if (pattern.hasHint(DataHints.ConstructorKey) || pattern.hasHint(DataHints.SelectorKey)) {
          // constructor or selector call
          val argsTable = prepareArgBindings(ruleResult, call.name, call.args)
          val calleeResult = state.readBottomUp(call.name, argsTable)
          val params = predicates(call.name).params.map(_.name)
          val nextTable =
            opJoinBodyAndPred(ruleResult, call.args, params, calleeResult, (x, y) => x.join(y))

          currentFunctionalPoint match {
            case Some(MatchPoint(fun, ma, pat, _)) if !fun.isRelation =>
              val patObj = pat.sourceObject
              val nextPats = ma.cases.dropWhile(_._1.sourceObject != patObj).tail
              if (nextTable.isEmpty) {
                // pattern failed => go to next pattern
                nextPats.headOption.foreach { next =>
                  skipAheadTo =
                    Some(SkipToPat(ma.sourceObject, next._1.sourceObject)) :: skipAheadTo.tail
                }
              } else {
                // pattern succeeded => skip other patterns
                if (nextPats.nonEmpty)
                  skipAlternativePatterns.head += ma.sourceObject -> nextPats.map(
                    _._1.sourceObject
                  ).toSet
              }
            case _ => // nothing
          }
          val nextRuleEval = RuleEvaluation(nextTable, ruleIdx, atomsTail)
          val next = InRule(p, argBindings, predResult, nextRuleEval, rules)
          callStack.update(next)
        } else {
          super.nextAtom(cp)
        }
      case _ =>
        super.nextAtom(cp)
    }
  }

  private def fpIntoPredicate(cp: EvaluationPoint): Unit = {
    skipElseBranches = mutable.Set[SourceObject]() :: skipElseBranches
    skipAheadTo = None :: skipAheadTo
    skipAlternativePatterns =
      mutable.Map[SourceObject, Set[SourceObject]]() :: skipAlternativePatterns
    super.intoPredicate(cp)
  }

  private def fpEvalResult(cp: EvaluationPoint): Unit = {
    skipElseBranches = skipElseBranches.tail
    skipAheadTo = skipAheadTo.tail
    skipAlternativePatterns = skipAlternativePatterns.tail
    super.evalResult(cp)
  }

  def controlPointFrontend: FunctionalControlPoint =
    currentFunctionalPoint.get

  def currentDebuggerInfo(numOfRowsShown: Int = Int.MaxValue): String = {
    val sb = new mutable.StringBuilder
    sb ++= currentCallStack += '\n'
    sb ++= currentBindings(numOfRowsShown) += '\n'
    currentCodeFunction.lines().map("  |  " + _).forEach(line => sb ++= line += '\n')
    sb.toString()
  }

  def currentFunction: FunctionDef =
    controlPointFrontend.fun

  def currentCodeSurrounding: String =
    controlPointFrontend.point.loc.sourceExcerpt(ExcerptRelativeRegion(3, 3)).linesColored

  def currentCodeFunction: String = {
    val fp = controlPointFrontend
    fp.point.loc.sourceExcerpt(
      ExcerptAbsoluteRegion(fp.fun.startIndex, fp.fun.endIndex)
    ).linesColored
  }

  def currentCallStack: String =
    getFunctionalCallStack.mkString("[", ", ", "]")

  def prettyPrint(uri: URI): String = uri match {
    case i: JVMURI => uris(i).toString
    case MockURI(constr, args) =>
      val argsS = args.map {
        case a: URI => prettyPrint(a)
        case v => v.toString
      }
      s"$constr(${argsS.mkString(", ")})"
    case i: WrappedURI => uris(i.uri).toString
    case _ => uri.toString
  }

  def prettyPrint(v: Value): String = v match {
    case URIValue(uri) => prettyPrint(uri)
    case ScalaValue(v) => v.toString
    case TopValue => "⊤"
    case BotValue => "⊥"
  }

  def currentBindings(numOfRowsShown: Int): String = {
    val table = frontendTable(controlPointFrontend, varsIR)
    table.bindingsToString(prettyPrint, numOfRowsShown)
  }
}
