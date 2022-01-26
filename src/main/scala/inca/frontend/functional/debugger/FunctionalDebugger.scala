package inca.frontend.functional.debugger

import inca.backend.hints.DebugHints.SourceConstruct
import inca.backend.hints.MagicSetHints
import inca.backend.ir.Datalog
import inca.backend.transform.magic.demand.DemandTransformation.demandPatternExtensionalPrefix
import inca.compiler.source.{ExcerptAbsoluteRegion, ExcerptRelativeRegion, SourceObject}
import inca.debugger.table.Table
import inca.debugger._
import inca.frontend.functional.compiler.CompiledFunctionalModule
import inca.frontend.functional.core.{FunctionDef, If, Name}
import inca.runtime.data.{MockURI, WrappedURI}
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuples
import truechange.{JVMURI, URI}
import truediff.Diffable

import scala.annotation.tailrec
import scala.collection.mutable

final class FunctionalDebugger(val compiled: CompiledFunctionalModule) extends Debugger {
  super.initialize(compiled)

  override val frontend: FunctionalDebuggerFrontend = new FunctionalDebuggerFrontend(this)

  private var skipElseBranches: List[mutable.Set[SourceObject]] = List()
  private var skipToElse: List[Option[SourceObject]] = List()

  private var uris: Map[URI, Diffable] = Map()

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
    frontend.getFunction(fr.cp.point.pat).map(_.name)
  }

  def stepIntoFrontend(): Unit = {
    var fp: Option[FunctionalControlPoint] = None
    while (fp.isEmpty) {
      stepInto()
      if (callStack.isEmpty)
        return
      fp = frontend.frontendPoint(controlPointIR)
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
      frontend.frontendPoint(next) match {
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
          frontend.frontendPoint(controlPointIR).foreach(_ => _controlTraceFrontend.remove(_controlTraceFrontend.size - 1))
          atom.isEmpty || (SourceConstruct.get(atom.get) match {
            case Some((cond: If, false)) => cond.sourceObject == elseCond
            case _ => false
          })
        }
    }
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

  override def controlPointFrontend: FunctionPoint = frontend.frontendPoint(controlPointIR) match {
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
    val table = frontend.frontendTable(controlPointFrontend, varsIR)
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