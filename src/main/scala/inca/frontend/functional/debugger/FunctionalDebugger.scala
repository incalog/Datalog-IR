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
import inca.debugger.redesign_new.Atom
import inca.debugger.redesign_new.AtomEval
import inca.debugger.redesign_new.AtomResult
import inca.debugger.redesign_new.Debugger
import inca.debugger.redesign_new.PositiveTable
import inca.debugger.redesign_new.Predicate
import inca.debugger.redesign_new.Query
import inca.debugger.redesign_new.QueryResult
import inca.debugger.redesign_new.QueryStack
import inca.debugger.redesign_new.Rule
import inca.debugger.redesign_new.RuleResult
import inca.debugger.redesign_new.Subquery
import inca.debugger.redesign_new.ValueTable
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

  private var lastConditionOrMatch: List[Option[Either[ConditionPoint, MatchPoint]]] = List()
  private var isFirstRule: List[Boolean] = List()
  private var oneRuleFired: List[Boolean] = List()
  private var showCurrentRule: List[Boolean] = List()
  private var determinesSet: List[Boolean] = List()

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
    preds.get(pat).flatMap(getFunction)
  def getFunction(pat: Datalog.Pattern): Option[FunctionDef] =
    pat.getHint(SourceConstruct.key) match {
      case Some(SourceConstruct(f: FunctionDef)) => Some(f)
      case _ => None
    }

  private val functionalPointDeriv: Derivative[QueryStack, Option[FunctionalControlPoint]] =
    queryStack.addDerivative[Option[FunctionalControlPoint]](_ => None) { stack =>
      if (stack.isEmpty)
        None
      else {
        val lifted = lift(stack.top)
        lifted
      }
    }

  private def lift(query: Query): Option[FunctionalControlPoint] = {
    val lastShowCurrentRule = showCurrentRule.head
    showNextRule(query)
    if (!lastShowCurrentRule) {
      return None
    }
    getFunction(query.pred) match {
      case Some(fun) =>
        query match {
          case Subquery(pred, _, _, sup, rules @ Rule(_, _, atoms) +: _) =>
            if (sup.isEmpty)
              None
            else
              atoms match {
                case Nil => None
                case Atom(atom) :: _ =>
                  // need to determine if we should skip
                  if (shouldSkipPrefixAtoms()) {
                    skipAheadTo.head match {
                      case Some(skip) =>
                        // TODO should stop be false for the inner if?
                        val stop = atom.getHint(SourceConstruct.key).exists(h =>
                          skip.stop(h.asInstanceOf[SourceConstruct[_]]))
                        if (!stop)
                          return None
                        else {
                          skipAheadTo = None :: skipAheadTo.tail
                        }
                      case None => // do nothing
                    }
                  }
                  liftAtAtom(query)
                case AtomResult(table, _) :: _ =>
                  lastConditionOrMatch.head match {
                    case Some(Left(condition)) =>
                      if (determinesSet.head)
                        showCurrentRule = table.nonEmpty :: showCurrentRule.tail
                      else
                        showCurrentRule =
                          (!oneRuleFired.head && table.nonEmpty) :: showCurrentRule.tail
                      if (table.isEmpty && condition.thenBranch && !determinesSet.head) {
                        // Is this correct?
                        skipAheadTo =
                          Some(SkipToElse(condition.cond.sourceObject)) :: skipAheadTo.tail
                      }
                    case Some(Right(mtch)) =>
                    // TODO
                    case None => // do nothing
                  }
                  None
              }
          case QueryResult(_, _) =>
            None
          // Some(FunctionPoint(fun, fun.sourceObject, query))
          case _ => None
        }
      case None => None
    }
  }

  private def shouldSkipPrefixAtoms(): Boolean = lastConditionOrMatch.head match {
    case Some(Left(condition)) => !isFirstRule.head
    case Some(Right(mtch)) =>
      // TODO
      !isFirstRule.head
    case None => false
  }

  private def showNextRule(query: Query): Unit = query match {
    case Subquery(_, _, _, _, RuleResult(t) +: _) =>
      if (isFirstRule.head)
        isFirstRule = false :: isFirstRule.tail

      oneRuleFired = (oneRuleFired.head || t.nonEmpty) :: oneRuleFired.tail
      lastConditionOrMatch.head match {
        case None =>
          showCurrentRule = true :: showCurrentRule.tail
        case _ =>
          if (determinesSet.head)
            showCurrentRule = true :: showCurrentRule.tail
          else
            showCurrentRule = (!oneRuleFired.head && t.isEmpty) :: showCurrentRule.tail
      }
    case _ => // do nothing
  }

  private def liftAtAtom(query: Query): Option[FunctionalControlPoint] = {
    val Subquery(pred, _, _, sup, rules @ Rule(_, _, Atom(atom) +: _) +: _) = query
    val fun = getFunction(pred).get
    atom.getHint(SourceConstruct.key) match {
      case Some(SourceConstruct(constr: Expression)) =>
        expressionPoint(constr).map(FunctionPoint(fun, _, query))
      case Some(SourceConstruct((let: Let, v: String))) =>
        let.names.find(_.name == v).map(p => FunctionPoint(fun, p.sourceObject, query))
      case Some(SourceConstruct((m: Match, constr: Pattern))) =>
        val point = MatchPoint(fun, m, constr, query)
        lastConditionOrMatch = Some(Right(point)) :: lastConditionOrMatch.tail
        Some(point)
      case Some(SourceConstruct((cond: If, thenBranch: Boolean))) =>
        val point = ConditionPoint(fun, cond, thenBranch, query)
        lastConditionOrMatch = Some(Left(point)) :: lastConditionOrMatch.tail
        None
      case _ =>
        None
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
      vars :+= predParams(fp.irQuery.pred).last
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

    val pattern = preds(mainFun)
    val adorn = pattern.hints(MagicSetHints.Main.key).asInstanceOf[MagicSetHints.Main].adorn
    val inputParams = predParams(mainFun).zip(adorn).filter(_._2).map(_._1)
    val inputTable = ImmutableTable[Value](inputParams, Seq(debugVals))
    super.entry(mainFun, inputTable)
    // addNewSkipInfo()

    // TODO could be that it is not needed
    if (currentFunctionalPoint.isEmpty)
      stepInto()
  }

  override protected def atomInto(atom: Datalog.Atom, args: ValueTable): AtomEval = {
    val Subquery(_, _, _, sup, _) = queryStack.top
    val Datalog.Call(pred, argTerms, _, _) = atom
    val pat = preds(pred)
    val isConstr = pat.hasHint(DataHints.ConstructorKey)
    val isSelector = pat.hasHint(DataHints.SelectorKey)
    if (isConstr || isSelector) {
      val argsTable = prepareArgTable(sup, pred, argTerms)
      val calleeResult = state.readBottomUp(pred, argsTable)
      val table = fitToSupplementary(pred, argTerms, calleeResult, sup)
      AtomResult(table, PositiveTable)
    } else super.atomInto(atom, args)
  }

  def getFunctionalCallStack: List[Name] = queryStack.frames.flatMap { query =>
    getFunction(query.pred).map(_.name)
  }

  protected def stepToFunctionalPoint(step: () => Boolean): Boolean = {
    var b = step()
    while (b && !isFinished && !isAtBreakpoint) {
      if (currentFunctionalPoint.isDefined) {
        // stepOverConditionPoint(currentFunctionalPoint.get)
        return b
      }
      b = step()
    }
    b
  }

  override protected def doStepInto(shortCircuit: Boolean): Boolean =
    stepToFunctionalPoint(() => super.doStepIntoIR(shortCircuit))

  override protected def doStepOver(shortCircuit: Boolean): Boolean =
    stepToFunctionalPoint(() => super.doStepOverIR(shortCircuit))

  override protected def doStepOut(shortCircuit: Boolean): Boolean =
    stepToFunctionalPoint(() => super.doStepOutIR(shortCircuit))

  override type Breakpoint = FunctionalBreakpoint
  override def addBreakpoint(bp: Breakpoint): Unit = {
    val irBPs = FunctionalBreakpoint.lower(bp)(preds)
    irBPs.foreach(breakpointHandler.addBreakpoint)
  }
  override def removeBreakpoint(bp: Breakpoint): Unit = {
    val irBPs = FunctionalBreakpoint.lower(bp)(preds)
    irBPs.foreach(breakpointHandler.removeBreakpoint)
  }

  override def clearBreakpoints(): Unit = {
    breakpointHandler.clearBreakpoints()
  }
  // what has the lifting to consider
  // 1. skipping over rules that mark the non taken branch (then or else branch, patterns that were not triggered)j
  // 2. not showing already executed atoms again in the other branch rule (when the condition failed)
  // we need to store atoms already executed in the previous rule.
  // we need to mark that we want to skip the following rules.

  // when we decide a condition point is true, we skip the remaining rules
  // when we decided a condition point is false, we skip the remainder of the current rule, skip the prefix atoms and condition atoms of the else branch rule
  // when we decide a matchpoint is true we skip the following patterns rules
  // when we decide a matchpoint is false we skip the prefix atoms of the following pattern rules

//  @tailrec
//  def stepOverConditionPoint(fp: FunctionalControlPoint): Unit = fp match {
//    case _: FunctionPoint | _: MatchPoint => // nothing
//    case condp: ConditionPoint =>
//      val conditionedPattern = condp.irQuery.pred
//      val conditionedBody = condp.irQuery match {
//        case Subquery(_, _, _, _, rules @ Rule(_, _, _) :+ _) =>
//          // preds(conditionedPattern).bodies.size - rules.size - 1
//          rules.size
//        case _ => throw new IllegalStateException("Has to be a rule that is currently executed")
//      }
//      // TODO probably not required
//      doStepIntoIR()
//
//      if (!condp.thenBranch) {
//        // we're at the else branch, continue
//      } else {
//        queryStack.top match {
//          case Subquery(`conditionedPattern`, _, _, sup, rules @ Rule(_, _, _) +: _)
//              if !sup.isEmpty && rules.size == conditionedBody =>
//            // we're in the same body and didn't fail => condition succeeded
//            if (!condp.fun.isRelation)
//              skipElseBranches.head += condp.cond.sourceObject
//          case _ =>
//            // condition failed and we were at the then branch => step to else branch
//            if (!condp.fun.isRelation)
//              skipAheadTo = Some(SkipToElse(condp.point)) :: skipAheadTo.tail
//        }
//      }
//      currentFunctionalPoint match {
//        case Some(fp2) => stepOverConditionPoint(fp2)
//        case None => doStepInto(false)
//      }
//  }
//
//  private def skipRule(rule: Rule): Boolean = {
//    val skipElse = skipElseBranches.head
//    val skipMatches = skipAlternativePatterns.head
//    if (skipElse.isEmpty && skipMatches.isEmpty)
//      return false
//    rule.atoms.exists {
//      case Atom(a) => a.getHint(SourceConstruct.key) match {
//        case Some(SourceConstruct((cond: If, false))) => skipElse.contains(cond.sourceObject)
//        case Some(SourceConstruct((ma: Match, pat: Pattern))) =>
//          val skipPats = skipMatches.getOrElse(ma.sourceObject, Set())
//          skipPats.contains(pat.sourceObject)
//        case _ => false
//      }
//      case _ => false
//    }
//  }
//
//  @tailrec
//  private def doSkipAheadTo(skip: SkipAhead): Unit = {
//    val top = queryStack.top
//    val atom = top match {
//      case Subquery(_, _, _, _, Rule(_, _, Atom(atom) +: _) +: _) => Some(atom)
//      case _ => None
//    }
//    val stop = atom.exists(
//      _.getHint(SourceConstruct.key).exists(h => skip.stop(h.asInstanceOf[SourceConstruct[_]])))
//    if (!stop && !isFinished && atom.isDefined) {
//      doStepOverIR(false)
//      doSkipAheadTo(skip)
//    }
//  }
//
//  override def doStepIntoIR(): Boolean = {
//    val top = queryStack.top
//    top match {
//      // Query End
//      case br @ Subquery(_, _, _, _, Nil) =>
//        outofPredicate(top)
//        // Q-Step Q-Rule A-Something
//      case ir @ Subquery(_, _, _, _, Rule(_, _, Atom(atom) +: _) +: _) =>
//        fpNextAtom(top)
//      case ir @ Subquery(_, _, _, _, Rule(_, _, Atom(atom) +: _) +: _) =>
//      // case ir @ Subquery(_, _, _, _, Rule(_, _, Atom(atom) +: _) +: _) =>
//        if (atoms.nonEmpty)
//        else if (rules.nonEmpty) fpNextRule(ir)
//        else lastRule(top)
//      case QueryResult(_, result) =>
//        skipElseBranches = skipElseBranches.tail
//        skipAheadTo = skipAheadTo.tail
//        skipAlternativePatterns = skipAlternativePatterns.tail
//        queryStack.pop()
//        replaceCallWithAtomResult(queryStack.top, result)
//    }
//    true
//  }
//
//  // TODO into rule does not exist anymore
//  private def fpIntoFirstRule(query: Query): Unit = {
//    val Subquery(_, _, _, _, rule +: rules) = query
//    val skip = skipRule(rule)
//    if (skip) {
//      val next =
//        InRule(p, argBindings, predResult, RuleEvaluation(argBindings, 0, rule.atoms), rules)
//      callStack.update(next)
//      stepIntoIR()
//      if (rules.isEmpty)
//        lastRule(next)
//      else
//        nextRule(next)
//    } else {
//      super.intoFirstRule(ep)
//      skipAheadTo.head.foreach {
//        // needed? stepOverIR()
//        doSkipAheadTo
//      }
//    }
//  }
//
//  @tailrec
//  private def fpNextRule(query: Query): Unit = {
//    val Subquery(pred, args, result, sup, rule +: rules) = query
//    val skip = skipRule(rule)
//    if (skip) {
//      val next = Subquery(pred, args, result, sup, rules)
//      if (rules.isEmpty) lastRule(next)
//      else fpNextRule(next)
//    } else {
//      super.nextRule(ep)
//      breakpointHandler.withBreakpoints(Seq.empty) {
//        skipAheadTo.head.foreach {
//          doSkipAheadTo
//        }
//      }
//    }
//  }
//
//  private def fpAtomReduction(query: Query): Query = {
////  private def fpNextAtom(query: Query): Unit = {
//    val Subquery(pred, args, result, sup, Rule(_, _, atoms) +: rules) = query
//    val atomsHead = atoms.head
//    val atomsTail = atoms.tail
//    atomsHead match {
//      case Atom(call: Datalog.Call) =>
//        val pattern = preds(call.name)
//        if (pattern.hasHint(DataHints.ConstructorKey) || pattern.hasHint(DataHints.SelectorKey)) {
//          // constructor or selector call
//          val argsTable = prepareArgTable(sup, call.name, call.args)
//          val calleeResult = state.readBottomUp(call.name, argsTable)
//          val params = predParams(call.name)
//          // this should be done somewhere eslse
////          currentFunctionalPoint match {
////            case Some(MatchPoint(fun, ma, pat, _)) if !fun.isRelation =>
////              val patObj = pat.sourceObject
////              val nextPats = ma.cases.dropWhile(_._1.sourceObject != patObj).tail
////              if (nextTable.isEmpty) {
////                // pattern failed => go to next pattern
////                nextPats.headOption.foreach { next =>
////                  skipAheadTo =
////                    Some(SkipToPat(ma.sourceObject, next._1.sourceObject)) :: skipAheadTo.tail
////                }
////              } else {
////                // pattern succeeded => skip other patterns
////                if (nextPats.nonEmpty)
////                  skipAlternativePatterns.head += ma.sourceObject -> nextPats.map(
////                    _._1.sourceObject
////                  ).toSet
////              }
////            case _ => // nothing
////          }
//          val nextRuleEval = RuleEvaluation(nextTable, ruleIdx, atomsTail)
//          val next = InRule(p, argBindings, predResult, nextRuleEval, rules)
//          callStack.update(next)
//        } else {
//          super.nextAtom(cp)
//        }
//      case _ =>
//        super.nextAtom(cp)
//    }
//  }
//
//  private def addNewSkipInfo(): Unit = {
//  }

  override def pushSubqueryHook(pred: Predicate, args: ValueTable): Unit = {
    skipElseBranches = mutable.Set[SourceObject]() :: skipElseBranches
    skipAheadTo = None :: skipAheadTo
    skipAlternativePatterns =
      mutable.Map[SourceObject, Set[SourceObject]]() :: skipAlternativePatterns
    lastConditionOrMatch = None :: lastConditionOrMatch
    oneRuleFired = false :: oneRuleFired
    isFirstRule = true :: isFirstRule
    showCurrentRule = true :: showCurrentRule
    determinesSet = getFunction(pred).get.isRelation :: determinesSet
  }

  override def popSubqueryHook(pred: Predicate, result: ValueTable): Unit = {
    skipElseBranches = skipElseBranches.tail
    skipAheadTo = skipAheadTo.tail
    skipAlternativePatterns = skipAlternativePatterns.tail
    lastConditionOrMatch = lastConditionOrMatch.tail
    oneRuleFired = oneRuleFired.tail
    isFirstRule = isFirstRule.tail
    showCurrentRule = showCurrentRule.tail
    determinesSet = determinesSet.tail
  }
//  // TODO Into Predicate does not exist anymore, instead do this when we produce new subquery
//  private def fpIntoPredicate(query: Query): Unit = {
//    super.intoPredicate(query)
//  }

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
    currentFunctionalPoint match {
      case Some(fp) =>
        val table = frontendTable(fp, varsIR)
        table.bindingsToString(prettyPrint, numOfRowsShown)
      case None => "Nil"
    }
  }
}
