package inca.frontend.functional.debugger

import inca.backend.hints.DebugHints
import inca.backend.hints.DebugHints.SourceConstruct
import inca.backend.ir.DatalogScala
import inca.compiler.source.SourceObject
import inca.debugger.old.{AtListElem, AtomPoint, BodyPoint, BreakpointIR, ControlPoint, PatternPoint}
import inca.frontend.functional.core.Collect
import inca.frontend.functional.core.Expression
import inca.frontend.functional.core.FunctionDef
import inca.frontend.functional.core.Let
import inca.frontend.functional.core.Match
import inca.frontend.functional.core.Module
import inca.frontend.functional.core.Pattern

sealed trait BreakpointPos
case class FunctionEntry(f: String) extends BreakpointPos
case class InFunction(sourceObject: SourceObject) extends BreakpointPos
case class FunctionExit(f: String) extends BreakpointPos

case class FunctionalBreakpoint(pos: BreakpointPos)

object FunctionalBreakpoint {
  def convert(
      fbp: FunctionalBreakpoint
    )(implicit patterns: Map[String, DatalogScala.Pattern]
    ): Seq[BreakpointIR] = {
    val cps = fbp.pos match {
      case FunctionEntry(f) =>
        val pattern = patterns(f)
        Seq(ControlPoint.patternEntry(pattern))
      case FunctionExit(f) =>
        val pattern = patterns(f)
        Seq(ControlPoint.patternExit(pattern))
      case InFunction(so) =>
        // collects atoms to stop at
        val options = patterns.values.flatMap { pat =>
          pat.bodies.flatMap { body =>
            body.atoms.flatMap { atom =>
              atom.getHint(DebugHints.SourceConstruct.key) match {
                case Some(SourceConstruct(expression: Expression))
                    if expression.sourceObject == so =>
                  Some((pat, body, atom))
                case Some(SourceConstruct((m: Match, p: Pattern))) if p.sourceObject == so =>
                  Some((pat, body, atom))
                case Some(SourceConstruct((let: Let, v: String))) =>
                  val bindsV = let.names.exists { name =>
                    val sameVar = v == name.name
                    val sameSO = name.sourceObject == so
                    sameVar && sameSO
                  }
                  if (bindsV) Some((pat, body, atom))
                  else None
                case _ => None
              }
            }
          }
        }.toSeq

        // construct controlpoints
        options.map { case (pat, body, atom) =>
          val bodyIdx = pat.bodies.indexOf(body)
          val atomIdx = body.atoms.indexOf(atom)
          val atomPoint = AtomPoint(atom)
          val bodyPoint = BodyPoint(body, AtListElem(body.atoms.toIndexedSeq, atomIdx, atomPoint))
          ControlPoint(PatternPoint(pat, AtListElem(pat.bodies.toIndexedSeq, bodyIdx, bodyPoint)))
        }
    }
    cps.map(BreakpointIR.apply)
  }

  def forExpression(
      prog: Module,
      f: String,
      exp: Expression,
      occurrence: Int = 0
    ): FunctionalBreakpoint = {
    val sourceObject = getSourceObjectOfExpression(prog, f, exp, occurrence)
    FunctionalBreakpoint(InFunction(sourceObject))
  }

  def forPattern(prog: Module, f: String, p: Pattern, occurrence: Int = 0): FunctionalBreakpoint = {
    val sourceObject = getSourceObjectOfPattern(prog, f, p, occurrence)
    FunctionalBreakpoint(InFunction(sourceObject))
  }

  def forBinding(
      prog: Module,
      f: String,
      name: String,
      occurrence: Int = 0
    ): FunctionalBreakpoint = {
    val sourceObject = getSourceObjectOfBinding(prog, f, name, occurrence)
    FunctionalBreakpoint(InFunction(sourceObject))
  }

  private def getSourceObjectOfExpression(
      funProg: Module,
      f: String,
      expOfInterest: Expression,
      occurrence: Int
    ): SourceObject = {
    val collectExpressions = new Collect[SourceObject] {
      override def transFunDef(fun: FunctionDef): Seq[SourceObject] =
        if (fun.name.name == f) super.transFunDef(fun)
        else Seq()

      override def transExpression(exp: Expression): Seq[SourceObject] = {
        if (exp == expOfInterest)
          exp.sourceObject +: super.transExpression(exp)
        else
          super.transExpression(exp)
      }
    }
    val sourceObjectCandidates = collectExpressions.transModule(funProg)
    sourceObjectCandidates(occurrence)
  }

  private def getSourceObjectOfPattern(
      funProg: Module,
      f: String,
      patternOfInterest: Pattern,
      occurrence: Int
    ): SourceObject = {
    val collectExpressions = new Collect[SourceObject] {
      override def transFunDef(fun: FunctionDef): Seq[SourceObject] =
        if (fun.name.name == f) super.transFunDef(fun)
        else Seq()

      override def transPattern(p: Pattern): Seq[SourceObject] = {
        if (p == patternOfInterest) Seq(p.sourceObject)
        else Seq()
      }
    }
    val sourceObjectCandidates = collectExpressions.transModule(funProg)
    sourceObjectCandidates(occurrence)
  }

  private def getSourceObjectOfBinding(
      funProg: Module,
      f: String,
      bindingOfInterest: String,
      occurrence: Int
    ): SourceObject = {
    val collectExpressions = new Collect[SourceObject] {
      override def transFunDef(fun: FunctionDef): Seq[SourceObject] =
        if (fun.name.name == f) super.transFunDef(fun)
        else Seq()

      override def transExpression(e: Expression): Seq[SourceObject] = e match {
        case Let(names, _, bound, body) =>
          names.flatMap { n =>
            if (n.name == bindingOfInterest) Seq(n.sourceObject)
            else Seq()
          } ++ transExpression(bound) ++ transExpression(body)
        case _ => super.transExpression(e)
      }
    }
    val sourceObjectCandidates = collectExpressions.transModule(funProg)
    sourceObjectCandidates(occurrence)
  }
}
