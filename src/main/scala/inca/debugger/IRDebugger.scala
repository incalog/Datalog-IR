package inca.debugger

import inca.backend.ir.Datalog
import inca.backend.optimize.Optimization
import inca.backend.transform.Transformation
import inca.compiler.options.Options
import inca.compiler.CompiledDatalogModule
import inca.debugger.ControlPoint.{AtAtom, AtBody}
import inca.runtime.EnginePool
import inca.runtime.Query.ChangeFeed
import inca.runtime.context.{DataModel, QueryScope}
import org.eclipse.viatra.query.runtime.api.AdvancedViatraQueryEngine
import org.eclipse.viatra.query.runtime.rete.matcher.TimelyReteBackendFactory

case class FixpointState(derivedRels: Set[Relation])

trait IRDebugger {
  private var module: Datalog.Module = _
  private lazy val patterns: Map[Datalog.Name, Datalog.Pattern] = {
    module.pats.map { p => p.name -> p }.toMap
  }

  private var engine: AdvancedViatraQueryEngine = _
  private var feed: ChangeFeed = _
  private var scope: QueryScope = _

  private val callStack: CallStack = new CallStack()
  // private var fixpointState: FixpointState = FixpointState(Set())

  def isFinished: Boolean = callStack.isFinished


  def initialize(mod: Datalog.Module): Unit = {
    // TODO how to make this better?
    module = mod
    val options = new Options {
      override def optimizations: Seq[Optimization] = Seq()
      override def transformations: Seq[Transformation] = Seq()
      override def stopOnError: Boolean = true
      override def stopOnWarning: Boolean = false
    }
    val dataModel = new DataModel()
    val compiled = CompiledDatalogModule(module, dataModel, options)
    scope = new QueryScope(compiled.dataModel)
    val (_engine, _feed) = EnginePool.loadEngineAndDatabase(scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)
    engine = _engine
    feed = _feed
  }


  def entry(name: Datalog.Name, args: PartialTuple): Unit = {
    val cp = ControlPoint.patternEntryPoint(patterns(name))
    callStack.push(cp)
  }

  // def visualizeCurrentPosition(): String = {
  //   val ControlPoint(pat, bodyPoint) = callStack.top
  //   bodyPoint match {
  //     case Point.Before =>
  //       s"↓${}"
  //     case Point.After =>

  //     case Point.At(AtBody())
  //   }
  // }

  def stepOver(): Unit = {
    val ControlPoint(pat, bodyPos) = callStack.pop()
    bodyPos match {
      case Point.Before =>
        val cp = ControlPoint.patternEndPoint(pat)
        callStack.push(cp)
      case Point.After =>
        stepPatternEndPoint(bodyPos)
      case Point.At(AtBody(ix, Point.Before)) =>
        val cp = ControlPoint.bodyEndPoint(pat, ix)
        callStack.push(cp)
      case Point.At(AtBody(bix, Point.At(AtAtom(aix, true)))) =>
        val cp = ControlPoint.atomEndPoint(pat, bix, aix)
        callStack.push(cp)
      case Point.At(AtBody(bix, Point.At(AtAtom(aix, false)))) =>
        val cp =
          if (isLastAtom(pat, bix, aix))
            ControlPoint.bodyEndPoint(pat, bix)
          else
            ControlPoint.atomEntryPoint(pat, bix, aix + 1)
        callStack.push(cp)
      case Point.At(AtBody(ix, Point.After)) =>
        val cp =
          if (isLastBody(pat, ix))
            ControlPoint.patternEndPoint(pat)
          else
            ControlPoint.bodyEntryPoint(pat, ix + 1)
        callStack.push(cp)
    }
  }

  def stepPatternEndPoint(bp: ControlPoint.BodyPoint): Unit = {
    if (callStack.isEmpty)
      throw EndOfTraversalReachedException("Program terminated")

    if (callStack.top.isAtomPoint) {
      val ControlPoint(p, Point.At(AtBody(bix, Point.At(AtAtom(aix, true))))) = callStack.pop()
      val cp =
        if (isLastAtom(p, bix, aix))
          ControlPoint.bodyEndPoint(p, bix)
        else
          ControlPoint.atomEndPoint(p, bix, aix)
      callStack.push(cp)
    } else throw IllegalDebugStateException(s"${callStack.top} cannot occur after end of pattern ${bp}")
  }

  def stepInto(): Unit = {
    val ControlPoint(pat, bodyPos) = callStack.top
    bodyPos match {
      case Point.Before =>
        val cp = ControlPoint.bodyEntryPoint(pat, 0)
        callStack.pop()
        callStack.push(cp)
      case Point.After =>
        callStack.pop()
        stepPatternEndPoint(bodyPos)
      case Point.At(AtBody(ix, Point.Before)) =>
        val cp = ControlPoint.atomEntryPoint(pat, ix, 0)
        callStack.pop()
        callStack.push(cp)
      case Point.At(AtBody(bix, Point.At(AtAtom(aix, true)))) =>
        val atom = pat.bodies(bix).atoms(aix)
        atom match {
          case Datalog.Call(name, _, _, _) =>
            val cp = ControlPoint.patternEntryPoint(patterns(name))
            callStack.push(cp)
          case Datalog.Computed(lhs, Datalog.CountAggregation(name, _)) =>
            val cp = ControlPoint.patternEntryPoint(patterns(name))
            callStack.push(cp)
          case Datalog.Computed(lhs, Datalog.CustomAggregation(_, _, _, name, _, _)) =>
            val cp = ControlPoint.patternEntryPoint(patterns(name))
            callStack.push(cp)
          case _ =>
            val cp = ControlPoint.atomEndPoint(pat, bix, aix)
            callStack.pop()
            callStack.push(cp)
        }
      case Point.At(AtBody(bix, Point.At(AtAtom(aix, false)))) =>
        callStack.pop()
        val cp =
          if (isLastAtom(pat, bix, aix)) {
            callStack.pop()
            ControlPoint.bodyEndPoint(pat, bix)
          } else
            ControlPoint.atomEntryPoint(pat, bix, aix + 1)
        callStack.push(cp)
      case Point.At(AtBody(ix, Point.After)) =>
        val cp =
          if (isLastBody(pat, ix))
            ControlPoint.patternEndPoint(pat)
          else
            ControlPoint.bodyEntryPoint(pat, ix + 1)

        callStack.pop()
        callStack.push(cp)
    }
  }

  def stepOut(): Unit = {
    val ControlPoint(pat, bodyPoint) = callStack.pop()
    bodyPoint match {
      case Point.After =>
        stepPatternEndPoint(bodyPoint)
      case Point.Before =>
        val cp = ControlPoint.patternEndPoint(pat)
        callStack.push(cp)
      case Point.At(AtBody(bix, Point.Before)) =>
        val cp = ControlPoint.bodyEndPoint(pat, bix)
        callStack.push(cp)
      case Point.At(AtBody(bix, Point.After)) =>
        val cp = ControlPoint.patternEndPoint(pat)
        callStack.push(cp)
      case Point.At(AtBody(bix, Point.At(_))) =>
        val cp = ControlPoint.bodyEndPoint(pat, bix)
        callStack.push(cp)
    }
  }

  private def isLastAtom(pat: Datalog.Pattern, bodyIdx: Int, atomIdx: Int): Boolean =
    pat.bodies(bodyIdx).atoms.size <= atomIdx + 1

  private def isLastBody(pat: Datalog.Pattern, bodyIdx: Int): Boolean =
    pat.bodies.size <= bodyIdx + 1
}
