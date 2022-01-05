package inca.debugger

import inca.backend.ir.Datalog
import inca.backend.optimize.Optimization
import inca.backend.transform.Transformation
import inca.compiler.options.Options
import inca.compiler.CompiledDatalogModule
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
  private var fixpointState: FixpointState = FixpointState(Set())

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
    val frame = PatternFrame(patterns(name), args)
    callStack.push(frame)
  }


  def stepOver(): Unit = callStack.frame match {
    case PatternFrame(pat, _) =>
      callStack.pop()
      val newFrame = PatternEndFrame(pat)
      callStack.push(newFrame)
    case BodyFrame(body) =>
      callStack.pop()
      val newFrame = BodyEndFrame(body)
      callStack.push(newFrame)
    case AtomFrame(atom) =>
      // TODO we currently consider non-recursive programs
      // TODO when we step over a recursive call we of the current scc we need to change the fixpoint state
      callStack.pop()
      val body = callStack.enclosingBody.getOrElse(throw new IllegalArgumentException("CANNOT HAPPEN"))
      val frame = nextAtom(body, atom) match {
        case Some(next) =>
          AtomFrame(next)
        case None =>
          callStack.pop()
          BodyEndFrame(body)
      }
      callStack.push(frame)
      // TODO change substitution
    case BodyEndFrame(body) =>
      callStack.pop()
      val pat = callStack.enclosingPattern.getOrElse(throw new IllegalArgumentException("CANNOT HAPPEN"))
      val frame = nextBody(pat, body) match {
        case Some(next) =>
          BodyFrame(next)
        case None =>
          callStack.pop()
          PatternEndFrame(pat)
      }
      callStack.push(frame)
    case pt@PatternEndFrame(pat) =>
      callStack.pop()
      callStack.frame match {
        case AtomFrame(atom) =>
          // this means we did a step into a call
          // TODO do we skip to the next atom?
          val body = callStack.enclosingBody.get
          nextAtom(body, atom) match {
            case Some(next) =>
              val nextFrame = AtomFrame(next)
              callStack.pop()
              callStack.push(nextFrame)
            case None =>
              callStack.pop()
              callStack.push(BodyEndFrame(body))
          }
        case BodyFrame(body) => ???
          // does not make sense
        case BodyEndFrame(body) => ???
          // does not make sense
        case PatternFrame(pat, args) => ???
          // this means we are
        case f => throw new IllegalStateException(s"Inconsistent debugging state: $f cannot occur after $pt")
      }
  }

  // TODO these functions currently only work correctly if bodies and atoms are unique within a pattern/body
  private def isLastAtom(body: Datalog.Body, atom: Datalog.Atom): Boolean =
    body.atoms.last == atom

  private def nextAtom(body: Datalog.Body, atom: Datalog.Atom): Option[Datalog.Atom] = {
    val idx = body.atoms.indexOf(atom)
    val nextIdx = idx + 1
    if (nextIdx > 0 && nextIdx < body.atoms.size) Some(body.atoms(nextIdx))
    else None
  }

  private def isLastBody(pat: Datalog.Pattern, body: Datalog.Body): Boolean =
    pat.bodies.last == body

  private def nextBody(pat: Datalog.Pattern, body: Datalog.Body): Option[Datalog.Body] = {
    val idx = pat.bodies.indexOf(body)
    val nextIdx = idx + 1
    if (nextIdx > 0 && nextIdx < pat.bodies.size) Some(pat.bodies(nextIdx))
    else None
  }


  private def isPatternRecursive(pat: Datalog.Pattern): Boolean = true

  def stepInto(): Unit = callStack.frame match {
    case PatternFrame(pat, _) =>
      // callStack.pop()
      val newFrame = BodyFrame(pat.bodies.head)
      callStack.push(newFrame)

    case BodyFrame(body) =>
      // callStack.pop()
      val newFrame = AtomFrame(body.atoms.head)
      callStack.push(newFrame)

    case AtomFrame(atom) => atom match {
      case Datalog.Call(name, args, transitive, neg) =>
        // callStack.pop()
        // TODO fill bound arguments
        val newFrame = PatternFrame(patterns(name), Map())
        callStack.push(newFrame)
      case _ =>
        val body = callStack.enclosingBody.get
        val frame = nextAtom(body, atom) match {
          case Some(next) =>
            AtomFrame(next)
          case None =>
            BodyEndFrame(body)
        }
        callStack.push(frame)
    }
    case BodyEndFrame(body) =>
      callStack.pop()
      val pat = callStack.enclosingPattern.get
      val frame = nextBody(pat, body) match {
        case Some(next) =>
          BodyFrame(next)
        case None =>
          PatternEndFrame(pat)
      }
      callStack.push(frame)
    case PatternEndFrame(pat) =>
      ???
  }

  def stepOut(): Unit = callStack.frame match {
    case PatternFrame(pat, args) => ???
    case BodyFrame(_) =>
      val pat = callStack.enclosingPattern.get
      callStack.pop()
      callStack.pop()
      val nextFrame = PatternEndFrame(pat)
      callStack.push(nextFrame)
    case AtomFrame(_) =>
      val body = callStack.enclosingBody.get
      callStack.pop()
      callStack.pop()
      val nextFrame = BodyEndFrame(body)
      callStack.push(nextFrame)
    case BodyEndFrame(_) =>
      val pat = callStack.enclosingPattern.get
      callStack.pop()
      callStack.pop()
      val nextFrame = PatternEndFrame(pat)
      callStack.push(nextFrame)
    case PatternEndFrame(pat) => ???
  }
}
