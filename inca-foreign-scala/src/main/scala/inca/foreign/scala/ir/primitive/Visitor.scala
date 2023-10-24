package inca.foreign.scala.ir.primitive

import inca.ir.{Atom, BaseIR, ModuleEntry, Term, Type}
import inca.ir.visitors.BaseIRVisitor
import inca.ir.Hint.preserveHints

import scala.collection.immutable.Seq

trait Visitor extends BaseIRVisitor:
  override def visitModuleEntry(moduleEntry: ModuleEntry): Seq[ModuleEntry] = moduleEntry match
    case ScalaDefnModuleEntry(defn) => Seq(ScalaDefnModuleEntry(defn))
    case _ => super.visitModuleEntry(moduleEntry)

  override def visitAtom(atom: Atom): Seq[Atom] = atom match
    case ScalaAggregationAtom(agg, rel, out, ty, args, col) =>
      Seq(ScalaAggregationAtom(agg, rel, visitTerm(out).head, ty, args.flatMap(visitTerm), col))
    case _ => super.visitAtom(atom)

  override def visitTerm(term: Term): Seq[Term] = preserveHints(term) {
    term match
      case ScalaTerm(closure, ty, args) => Seq(ScalaTerm(closure, ty, args.flatMap(visitTerm)))
      case _ => super.visitTerm(term)
  }

  override def visitType(ty: Type): Type = preserveHints(ty) {
    ty match
      case ScalaType(sty) => ScalaType(sty)
      case _ => super.visitType(ty)
  }