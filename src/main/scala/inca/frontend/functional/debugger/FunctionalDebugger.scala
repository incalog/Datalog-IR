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

  // stores the last condition of match point
  private var lastConditionOrMatch: List[Option[Either[ConditionPoint, MatchPoint]]] = List()
  // stores if the current function determines a set or not
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
      case SourceConstruct((cond: If, false)) =>
        cond.sourceObject == ifSource
      case _ =>
        false
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
    if (skipRule(query)) {
      return None
    }
    getFunction(query.pred) match {
      case Some(fun) =>
        query match {
          case Subquery(_, _, _, sup, Rule(_, _, atoms) +: _) if sup.nonEmpty =>
            atoms match {
              case Nil => None
              case Atom(atom) :: _ =>
                if (skipAtom(atom)) {
                  return None
                } else {
                  // we skipped all already shown points hence, we can reset
                  skipAheadTo = None :: skipAheadTo.tail
                }
                liftAtAtom(query)
              case AtomResult(table, _) :: _ =>
                determineSkips(table)
                None
            }
          case QueryResult(_, _, _) =>
            Some(FunctionPoint(fun, fun.sourceObject, query))
          case _ => None
        }
      case None => None
    }
  }

  private def skipRule(query: Query): Boolean = query match {
    case Subquery(_, _, _, _, Rule(_, _, atoms) +: _) =>
      val skipElses = skipElseBranches.head
      val skipMatches = skipAlternativePatterns.head
      if (skipElses.isEmpty && skipMatches.isEmpty)
        return false
      atoms.exists {
        case Atom(a) =>
          a.getHint(SourceConstruct.key) match {
            case Some(SourceConstruct((cond: If, false))) =>
              val res = skipElses.contains(cond.sourceObject)
              if (res)
                return true
              else
                false
            case Some(SourceConstruct((ma: Match, pat: Pattern))) =>
              val skipPats = skipMatches.getOrElse(ma.sourceObject, Set())
              val res = skipPats.contains(pat.sourceObject)
              if (res)
                return true
              else
                false
            case _ => false
          }
        case _ => false
      }
    case _ => false
  }

  private def skipAtom(atom: Datalog.Atom): Boolean = {
    skipAheadTo.head match {
      case Some(skip) =>
        val stop = atom.getHint(SourceConstruct.key).exists(h =>
          skip.stop(h.asInstanceOf[SourceConstruct[_]]))
        if (!stop) {
          return true
        }
      case None => // do nothing
    }
    false
  }

  private def determineSkips(table: ValueTable): Unit = lastConditionOrMatch.head match {
    case Some(Left(condition)) if !determinesSet.head =>
      if (condition.thenBranch && table.nonEmpty) {
        // condition succeeded in then branch
        skipElseBranches.head += condition.cond.sourceObject
      }
      if (condition.thenBranch && table.isEmpty) {
        // condition failed in then branch
        skipAheadTo = Some(SkipToElse(condition.point)) :: skipAheadTo.tail
      }
      // if (!condition.thenBranch && table.isEmpty) {}
      lastConditionOrMatch = None :: lastConditionOrMatch.tail
    case Some(Right(mtch)) if !determinesSet.head =>
      val patObj = mtch.pat.sourceObject
      val nextPats = mtch.ma.cases.dropWhile(_._1.sourceObject != patObj).tail
      if (table.isEmpty) {
        // pattern failed
        nextPats.headOption.foreach { next =>
          val skip = SkipToPat(mtch.ma.sourceObject, next._1.sourceObject)
          skipAheadTo = Some(skip) :: skipAheadTo.tail
        }
      } else {
        // pattern succeded
        if (nextPats.nonEmpty) {
          val patsToSkip = nextPats.map(_._1.sourceObject).toSet
          skipAlternativePatterns.head += mtch.ma.sourceObject -> patsToSkip
        }
      }
      lastConditionOrMatch = None :: lastConditionOrMatch.tail
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

    if (currentFunctionalPoint.isEmpty)
      stepInto()
  }

  override protected def atomInto(
      atom: Datalog.Atom,
      args: ValueTable,
      calleeArgs: ValueTable
    ): AtomEval = {
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
    } else super.atomInto(atom, args, calleeArgs)
  }

  override def pushSubqueryHook(pred: Predicate, args: ValueTable): Unit = {
    skipElseBranches = mutable.Set[SourceObject]() :: skipElseBranches
    skipAheadTo = None :: skipAheadTo
    skipAlternativePatterns =
      mutable.Map[SourceObject, Set[SourceObject]]() :: skipAlternativePatterns
    lastConditionOrMatch = None :: lastConditionOrMatch
    determinesSet = getFunction(pred).get.isRelation :: determinesSet
  }

  override def popSubqueryHook(pred: Predicate, result: ValueTable): Unit = {
    skipElseBranches = skipElseBranches.tail
    skipAheadTo = skipAheadTo.tail
    skipAlternativePatterns = skipAlternativePatterns.tail
    lastConditionOrMatch = lastConditionOrMatch.tail
    determinesSet = determinesSet.tail
  }

  protected def stepToFunctionalPoint(step: () => Boolean): Boolean = {
    var b = step()
    while (b && !isFinished) {
      if (currentFunctionalPoint.isDefined) {
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

  def getFunctionalCallStack: List[Name] = queryStack.frames.flatMap { query =>
    getFunction(query.pred).map(_.name)
  }

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
