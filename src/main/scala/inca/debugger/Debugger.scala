package inca.debugger

import inca.backend.ir.Datalog
import inca.debugger.table.Table
import inca.util.Meta.Scala
import inca.util.TupleOps

import scala.collection.mutable.ListBuffer
import scala.meta.Term
import scala.reflect.runtime.universe
import scala.tools.reflect.ToolBox

trait Debugger {
  val frontend: DebuggerFrontend

  private var module: Datalog.Module = _
  implicit lazy val patterns: Map[String, Datalog.Pattern] = {
    module.pats.map { pat => pat.name -> pat }.toMap
  }

  private var fixpointState: FixpointState = FixpointState(Map())
  private val callStack: CallStack = new CallStack()
  private val _controlTrace: ListBuffer[ControlPoint] = ListBuffer.empty
  def controlTrace: Seq[ControlPoint] = _controlTrace.toSeq

  def relation(name: String): Table = fixpointState.derived(name)

  private val toolBox: ToolBox[universe.type] =
    universe.runtimeMirror(getClass.getClassLoader).mkToolBox()

  def isFinished: Boolean = callStack.isFinished

  def initialize(mod: Datalog.Module): Unit = {
    module = mod
    frontend.initialize(mod)
  }

  def entry(name: Datalog.Name, bindings: Table): Unit = {
    val pat = patterns(name)
    val cp = ControlPoint.patternEntryPoint(pat)
    val frame = Frame(cp, bindings, Table.empty, Table(pat.params.map(_.name), Seq()))
    callStack.push(frame)
    _controlTrace += cp
  }

  def runUntil(cp: ControlPoint): Unit = {
    while (callStack.top.cp != cp) {
      val top = callStack.top
      top.cp.into match {
        case Some(nextCP) =>
          if (!nextCP.isPatternPoint) {
            callStack.pop()
          }
          val (args, bodySubst, patternSubst) = process(top)
          if (nextCP.isPatternEndPoint) {
            fixpointState = fixpointState.extendRelation(nextCP.point.name, patternSubst)
          }
          callStack.push(Frame(nextCP, args, bodySubst, patternSubst))
        case None =>
          stepOutOfPattern()
      }
    }
    _controlTrace += cp
  }

  def stepInto(): Unit = {
    val frame = callStack.top
    val cp = frame.cp
    cp.point.atom match {
      case Some(call: Datalog.Call) =>
        // TODO step into the call
        val tables = processAtom(frame, call)
        val callee = ControlPoint(PatternPoint(patterns(call.name), BeforeList))
        callStack.push(Frame(callee, ???, ???, ???))
      case Some(undef: Datalog.Undef) =>
        // TODO step into the call
      case Some(comp@Datalog.Computed(_, countAgg: Datalog.CountAggregation)) =>
        // TODO step into the call
      case Some(comp@Datalog.Computed(_, custAgg: Datalog.CustomAggregation)) =>
        // TODO step into the call
      case Some(atom) =>
        // TODO step inside the current call frame
        val tables = processAtom(frame, atom)
        val next = cp.stepIntra.get // yields next atom
        callStack.update(Frame(next, ???, ???, ???))
      case None =>
        // TODO no atom to execute, perform a step along the current body/pattern boundary
        if (cp.point.isPatternEntry) {
          // TODO step to first body
          val next = cp.stepIntra.get // yields first body of this pattern
          callStack.update(Frame(next, ???, ???, ???))
        } else if (cp.point.isPatternExit) {
          // TODO return from pattern
          callStack.pop()
        } else if (cp.point.isBodyEntry) {
          // TODO step into this body
          val next = cp.stepIntra.get // yields first atom of this body
          callStack.update(Frame(next, ???, ???, ???))
        } else if (cp.point.isBodyExit) {
          // TODO step into next body
          val next = cp.stepIntra.get // yields entry of next body
          callStack.update(Frame(next, ???, ???, ???))
        } else {
          throw new IllegalStateException(s"Unexpected control point $cp")
        }
    }
  }

  private def process(frame: Frame): (Table, Table, Table) = frame.cp.body match {
    case Enterable.Before =>
      (frame.arguments, frame.arguments, frame.patternSubst)
    case Enterable.After =>
      val pat = frame.cp.point
      val columns = pat.params.map(_.name)
      val projectedBodySubst = frame.bodySubst.project(columns)
      val patternSubst = frame.patternSubst.addRows(projectedBodySubst)
      (frame.arguments, Table.empty, patternSubst)
    case Enterable.At(AtBody(_, Enterable.Before)) =>
      (frame.arguments, frame.arguments, frame.patternSubst)
    case Enterable.At(AtBody(_, Enterable.After)) =>
      val pat = frame.cp.point
      val columns = pat.params.map(_.name).filter(frame.bodySubst.columns.contains)
      val projectedBodySubst = frame.bodySubst.project(columns)
      val patternSubst = frame.patternSubst.addRows(projectedBodySubst)
      (frame.arguments, frame.arguments, patternSubst)
    case Enterable.At(AtBody(bix, Enterable.At(AtAtom(aix, true)))) =>
      val atom = frame.cp.point.bodies(bix).atoms(aix)
      processAtom(frame, atom)
    case Enterable.At(AtBody(_, Enterable.At(AtAtom(_, false)))) =>
      (frame.arguments, frame.bodySubst, frame.patternSubst)
  }

  private def processAtom(frame: Frame, atom: Datalog.Atom): (Table, Table, Table) = atom match {
    case Datalog.Call(name, args, _, _) =>
      val callingPat = patterns(name)

      val paramSubst = callingPat.params.zip(args)
      val (varsBindings, constBindings) = paramSubst.partition(_._2.isInstanceOf[Datalog.Var])
      val varsBindingsCast = varsBindings.map { case (p, v) => (p, v.asInstanceOf[Datalog.Var]) }
      val constBindingsCast = constBindings.map { case (p, v) => (p, v.asInstanceOf[Datalog.Constant]) }

      val columnsSubst = varsBindingsCast.map { case (p, v) => (v.name, p.name) }.toMap
      var argsSubst = frame.bodySubst.project(varsBindingsCast.map(_._2.name)).renameColumns(columnsSubst)
      constBindingsCast.foreach { case (p, c) =>
        argsSubst = argsSubst.bind(p.name, transLiteral(c.lit))
      }

      val bodySubst = argsSubst

      val patternSubst = Table(callingPat.params.map(_.name), Seq())
      (argsSubst, bodySubst, patternSubst)
    case comp@Datalog.Computed(_, _) =>
      processComputed(frame, comp)
    case Datalog.ExtensionalCall(name, args, neg) => ???
    case Datalog.Compare(comp, lhs, rhs) => ???
    case Datalog.HasType(t, typ) => ???
    case Datalog.NotHasType(t, typ) => ???
    case Datalog.Path(src, srcTy, link, trg, trgTy) => ???
    case Datalog.NoPath(t, ty, link, termIsSource) => ???
    case Datalog.Undef(t) => ???
  }

  private def transLiteral(c: Datalog.Literal): Value = c match {
    case Datalog.IntLiteral(v) => ScalaValue(v)
    case Datalog.LongLiteral(v) => ScalaValue(v)
    case Datalog.DoubleLiteral(v) => ScalaValue(v)
    case Datalog.StringLiteral(v) => ScalaValue(v)
    case Datalog.BooleanLiteral(v) => ScalaValue(v)
  }

  private def processComputed(frame: Frame, computed: Datalog.Computed): (Table, Table, Table) = computed match {
    case Datalog.Computed(lhs, Datalog.Evaluation(evalArgs, resultType, code)) =>
      // TODO everything has to be bound otherwise it is not executable
      val (evalVarArgs, evalConstArgs) = evalArgs.map(_._1).zip(code.tree.params).partitionMap {
        case (Datalog.Var(v), p) => Left((p.name, v))
        case (Datalog.Constant(l), p) => Right((p.name, transLiteral(l)))
      }
      var argsTable = frame.bodySubst.project(evalVarArgs.map(_._2))
      evalConstArgs.foreach { case(p, v) =>
        argsTable = argsTable.bind(p.value, v)
      }
      argsTable = argsTable.rearrangeColumns(code.tree.params.map(_.name.value))
      // val argsData = evalArgs.map {
      //   case (Datalog.Var(vname), _) =>
      //     frame.bodySubst.project(vname)
      //   case (Datalog.Constant(l), _) => Seq(transLiteral(l))
      // }
      // val cartProduct = TupleOps.cartesianProduct(argsData).map(_.toVector).toVector

      val results =
        if (argsTable.isEmpty)
          Seq(processScala(Seq(), code))
        else argsTable.data.map { tuple =>
          processScala(tuple, code)
        }
      val multipleBodySubsts = results.map { result =>
        lhs match {
          case Datalog.Var(name) =>
            frame.bodySubst.bind(name, result)
          case Datalog.Constant(lit) =>
            throw IllegalDebugStateException("Not supported yet")
        }
      }
      // TODO merge all the bodies
      val resBodySubst = multipleBodySubsts.head
      (frame.arguments, resBodySubst, frame.patternSubst)
    case Datalog.Computed(lhs, Datalog.CountAggregation(patName, args)) => ???
    case Datalog.Computed(lhs, Datalog.CustomAggregation(typ, description, agg, patName, args, aggregatedColumn)) => ???
  }

  private def processScala(tuple: Seq[Value], code: Scala[Term.Function]): ScalaValue = {
    val funCode = s"(${code.syntax})(${tuple.mkString(", ")})"
    val parsed = toolBox.parse(funCode)
    ScalaValue(toolBox.eval(parsed))
  }

  def stepOutOfPattern(): Unit = {
    val patternEnd = callStack.pop()
    // fixpointState = fixpointState.extendRelation(patternEnd.patternSubst)
    // top is definitely now at an atom
    val nextCP = callStack.top.cp.over.getOrElse(throw IllegalDebugStateException(""))

    val arguments = callStack.top.arguments

    val call = callStack.top.cp.atom.asCall.getOrElse(throw IllegalDebugStateException("Control point below pattern end has to be an atom control point"))
    val calledPat = patternEnd.cp.point
    val callArgVars = call._2.collect { case Datalog.Var(name) => name }
    val columnsSubst = calledPat.params.map(_.name).zip(callArgVars).toMap
    val renamedPatternSubst = patternEnd.patternSubst.renameColumns(columnsSubst)
    val bodySubst = callStack.top.bodySubst.join(renamedPatternSubst)

    val patternSubst = callStack.top.patternSubst

    // pop atom before control point and push atom end control point
    callStack.pop()
    callStack.push(Frame(nextCP, arguments, bodySubst, patternSubst))
  }

  def stepInto(): Unit = {
    callStack.top.cp.into match {
      case Some(next) => runUntil(next)
      case None =>
    }
  }

  def stepOut(): Unit = {
    callStack.top.cp.out match {
      case Some(next) => runUntil(next)
      case None =>
        callStack.pop()
        callStack.top.cp.over match {
          case Some(next) => runUntil(next)
          case None =>
        }
    }
  }
}