package inca.foreign.scala.ir.primitive

import inca.ir.{Atom, BaseIR, ModuleEntry, Term, Type}
import inca.ir.visitors.BaseIRVisitor
import inca.ir.Hint.preserveHints

import scala.collection.immutable.Seq

trait Visitor extends BaseIRVisitor:
  override def visitModuleEntry(moduleEntry: ModuleEntry): Seq[ModuleEntry] = moduleEntry match
    case ScalaDefnModuleEntry(name, defn) => Seq(ScalaDefnModuleEntry(name, defn))
    case _ => super.visitModuleEntry(moduleEntry)

  override def visitAtom(atom: Atom): Seq[Atom] = atom match
    case ScalaAggregationAtom(ScalaAggregationOperator(ty, code), rel, out, args, col) =>
      val sty = visitType(ty) match
        case s: ScalaType => s
        case _ => throw IllegalStateException(s"Visiting scala type $ty yielded unexpected none scala type $ty")
      val op = ScalaAggregationOperator(sty, code)
      Seq(ScalaAggregationAtom(op, rel, visitTerm(out).head, args.flatMap(visitTerm), col))
    case _ => super.visitAtom(atom)

  override def visitTerm(term: Term): Seq[Term] = preserveHints(term) {
    term match
      case ScalaTerm(code, ty, args, isApp) => Seq(ScalaTerm(code, ty, args.flatMap(visitTerm), isApp))
      case ScalaConstantTerm(code, ty) => Seq(ScalaConstantTerm(code, ty))
      case _ => super.visitTerm(term)
  }

  override def visitType(ty: Type): Type = preserveHints(ty) {
    ty match
      case ScalaType(sty) => ScalaType(sty)
      case _ => super.visitType(ty)
  }