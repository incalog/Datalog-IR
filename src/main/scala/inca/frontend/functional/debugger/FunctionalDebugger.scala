package inca.frontend.functional.debugger

import inca.backend.hints.DebugHints.SourceConstruct
import inca.backend.hints.{DataHints, MagicSetHints}
import inca.backend.hints.OptimizationHints.KeepPattern
import inca.backend.ir.Datalog
import inca.backend.transform.magic.demand.DemandTransformation.demandPatternExtensionalPrefix
import inca.compiler.CompiledDatalogModule
import inca.compiler.source.{ExcerptAbsoluteRegion, ExcerptRelativeRegion, SourceObject}
import inca.debugger.table.Table
import inca.debugger._
import inca.frontend.functional.compiler.CompiledFunctionalModule
import inca.frontend.functional.core.{BaseLit, Expression, FunctionDef, If, Let, Name, NoneExp, Pattern, SetExp, SomeExp, Tuple, Var}
import inca.runtime.data.{MockURI, WrappedURI}
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuples
import truechange.{JVMURI, URI}
import truediff.Diffable

import scala.annotation.tailrec
import scala.collection.mutable

final class FunctionalDebugger(val compiled: CompiledFunctionalModule) extends Debugger {
  {
    val pats = compiled.ir.pats.map { pat =>
      val p = pat.copy().withHints(pat)
      // constructors and selectors may not be inlined, so that we can read values from the database
      if (p.hasHint(DataHints.ConstructorKey) || p.hasHint(DataHints.SelectorKey))
        p.addHint(KeepPattern)
      p
    }
    val m = compiled.ir.copy(pats = pats)
    val modified = CompiledDatalogModule(m, compiled.dataModel, compiled.options)
    super.initialize(modified)
  }

  private var skipElseBranches: List[mutable.Set[SourceObject]] = List()
  private var skipToElse: List[Option[SourceObject]] = List()

  private var uris: Map[URI, Diffable] = Map()

  override type FrontendPoint = FunctionalControlPoint
  override type FrontendValue = Value

  def getFunction(pat: Datalog.Pattern): Option[FunctionDef] = pat.getHint(SourceConstruct.key) match {
    case Some(SourceConstruct(f: FunctionDef)) => Some(f)
    case _ => None
  }

  override def frontendPoint(cp: ControlPoint): Option[FunctionalControlPoint] = {
    val patPoint = cp.point
    val fun = getFunction(patPoint.pat).getOrElse(return None)
    patPoint.bodies match {
      case BeforeList =>
        // start of function
        Some(FunctionPoint(fun, fun.name.sourceObject, cp))
      case AtListElem(_, _, BodyPoint(_, atoms)) => atoms match {
        case BeforeList => None
        case AtListElem(_, _, AtomPoint(atom)) =>
          atom.getHint(SourceConstruct.key) match {
            case Some(SourceConstruct(constr: Expression)) =>
              expressionPoint(constr).map(FunctionPoint(fun, _, cp))
            case Some(SourceConstruct(constr: Pattern)) =>
              Some(FunctionPoint(fun, constr.sourceObject, cp))
            case Some(SourceConstruct((let: Let, v: String))) =>
              let.names.find(_.name == v).map(p => FunctionPoint(fun, p.sourceObject, cp))
            case Some(SourceConstruct((cond: If, thenBranch: Boolean))) =>
              Some(ConditionalPoint(cond, thenBranch, cp))
            case _ =>
              None
          }
        case AfterList => None
      }
      case AfterList =>
        // end of function
        Some(FunctionPoint(fun, fun.sourceObject, cp))
    }
  }

  private def expressionPoint(exp: Expression): Option[SourceObject] = exp match {
    case _: Var | _: Tuple | _: BaseLit | _: NoneExp | _: SomeExp | _: SetExp => None
    case _ => Some(exp.sourceObject)
  }

  override def frontendTable(fp: FunctionalControlPoint, bound: Table[Value]): Table[Value] = fp match {
    case fp: FunctionPoint =>
      var vars = fp.vars.map(_.name).toList.sorted.distinct
      if (fp.isFunctionExit)
        vars :+= fp.irPoint.point.pat.params.last.name
      var myVars = Table.empty[Value](vars)
      for (row <- bound.rows) {
        val vals = vars.map { v =>
          val ix = bound.columnIndex(v)
          if (ix < 0)
            null
          else
            row.lift(ix).orNull
        }
        myVars = myVars.addRow(vals)
      }
      myVars
    case _: ConditionalPoint =>
      Table.empty
  }

  def entry(mainFun: String, args: meta.Term*): Unit = {
    val (vals, debugVals) = args.map { t =>
      val syntax = s"{import ${defintionObjSym}.${compiled.name}._; ${t.syntax}}"
      scalaCompiler.compileAndLoadScala[Any](syntax) match {
        case diff: Diffable =>
          updateExtensionalData(diff.loadEdits)
          diff.foreachTree(t => uris += t.uri -> t)
          (diff.uri, URIValue(diff.uri))
        case v =>
          (v, ScalaValue(v))
      }
    }.unzip
    database.insert(demandPatternExtensionalPrefix + mainFun, Tuples.flatTupleOf(vals:_*))

    val pattern = patterns(mainFun)
    val adorn =  pattern.hints(MagicSetHints.Main.key).asInstanceOf[MagicSetHints.Main].adorn
    val inputParams = pattern.params.zip(adorn).filter(_._2).map(_._1.name)
    val inputTable = Table[Value](inputParams, Seq(debugVals))
    super.entry(mainFun, inputTable)
  }

  def getFunctionalCallStack: List[Name] = callStack.frames.flatMap { fr =>
    getFunction(fr.cp.point.pat).map(_.name)
  }

  def stepIntoFrontend(): Unit = {
    var fp: Option[FunctionalControlPoint] = None
    while (fp.isEmpty) {
      stepInto()
      if (callStack.isEmpty)
        return
      fp = frontendPoint(controlPointIR)
    }
    stepOverConditionalPoint(fp.get)
  }

  @tailrec
  def stepOverConditionalPoint(fp: FunctionalControlPoint): Unit = fp match {
    case _: FunctionPoint => // nothing
    case condp: ConditionalPoint =>
      _controlTraceFrontend.remove(_controlTraceFrontend.size - 1)
      val currentPat = condp.irPoint.point.pat.name
      val currentBody = condp.irPoint.point.bodyIndex
      stepInto()
      val cp = controlPointIR

      val next = if (!condp.thenBranch) {
        // we're at the else branch, continue
        cp
      } else if (controlPointIR.point.pat.name == currentPat && cp.point.bodyIndex == currentBody && !frame.bodyTable.isEmpty) {
        // we're in the same body and didn't fail => condition succeeded
        skipElseBranches.head += condp.cond.sourceObject
        cp
      } else {
        // condition failed and we were at the then branch => step to else branch
        skipToElse = Some(condp.cond.sourceObject) :: skipToElse.tail
        controlPointIR
      }
      frontendPoint(next) match {
        case Some(fp2) => stepOverConditionalPoint(fp2)
        case None => stepIntoFrontend()
      }
  }

  private def skipBody(body: Datalog.Body): Boolean = {
    val skipElse = skipElseBranches.head
    if (skipElse.isEmpty)
      return false
    body.atoms.exists { a =>
      a.getHint(SourceConstruct.key) match {
        case Some(SourceConstruct((cond: If, isThen: false))) => skipElse.contains(cond.sourceObject)
        case _ => false
      }
    }
  }

  override def doBodyEntry(frame: Frame, cp: ControlPoint): Unit = {
    val skip = skipBody(cp.point.body.get)
    if (skip) {
      val next = cp.stepOver.get
      callStack.update(Frame(next, frame.argsTable, Table.empty, frame.patternTable))
    } else skipToElse.head match {
      case None => super.doBodyEntry(frame, cp)
      case Some(elseCond) =>
        super.doBodyEntry(frame, cp)
        stepOverUntil { () =>
          val atom = controlPointIR.point.atom
          frontendPoint(controlPointIR).foreach(_ => _controlTraceFrontend.remove(_controlTraceFrontend.size - 1))
          atom.isEmpty || (SourceConstruct.get(atom.get) match {
            case Some((cond: If, false)) => cond.sourceObject == elseCond
            case _ => false
          })
        }
    }
  }

  override def stepIntoCall(frame: Frame, atom: Datalog.Atom): Unit = atom match {
    case call: Datalog.Call =>
      val pattern = patterns(call.name)
      if (pattern.hasHint(DataHints.ConstructorKey) || pattern.hasHint(DataHints.SelectorKey)) {
        // constructor or selector call
        val preTables = prepareCallTables(frame, pattern, call.args)
        val data = readDatabase(call.name, preTables._1)
        val nextTables = transitionReturnCallTables(frame, pattern.params.map(_.name), data)
        val next = frame.cp.stepOver.get
        callStack.update(Frame(next, nextTables))
      }
      else
        super.stepIntoCall(frame, atom)
    case _ => super.stepIntoCall(frame, atom)
  }

  override def doPatternEntry(cp: ControlPoint): Unit = {
    skipElseBranches = mutable.Set[SourceObject]() :: skipElseBranches
    skipToElse = None :: skipToElse
    super.doPatternEntry(cp)
  }

  override def doPatternExit(frame: Frame): Unit = {
    skipElseBranches = skipElseBranches.tail
    skipToElse = skipToElse.tail
    super.doPatternExit(frame)
  }

  override def controlPointFrontend: FunctionPoint = frontendPoint(controlPointIR) match {
    case Some(fp: FunctionPoint) => fp
    case o => throw new MatchError(s"Expected function point but got $o")
  }

  override def controlTraceFrontend: Seq[FunctionalControlPoint] = super.controlTraceFrontend

  def currentFunction: FunctionDef = controlPointFrontend.fun

  def currentCodeSurrounding: String =
    controlPointFrontend.point.loc.sourceExcerpt(ExcerptRelativeRegion(3, 3)).linesColored

  def currentCodeFunction: String = {
    val fp = controlPointFrontend
    fp.point.loc.sourceExcerpt(ExcerptAbsoluteRegion(fp.fun.startIndex, fp.fun.endIndex)).linesColored
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
  }

  def currentBindings: String = {
    val table = frontendTable(controlPointFrontend, varsIR)
    val rowStrings = table.rows.map { row =>
      val sb = new StringBuilder
      sb += '['
      table.columns.foreach { col =>
        val ix = table.columnIndex(col)
        val v = row(ix)
        if (v != null) {
          sb ++= col
          sb += '='
          sb ++= prettyPrint(v)
          sb ++= ", "
        }
      }
      if (sb.length() > 2) {
        sb.deleteCharAt(sb.length() - 1)
        sb.deleteCharAt(sb.length() - 1)
      }
      sb += ']'
      sb.toString()
    }
    rowStrings.size match {
      case 0 => "[]"
      case 1 => rowStrings.head
      case _ => rowStrings.mkString("{", ", ", "}")
    }
  }
}