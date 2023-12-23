package inca.foreign.scala.ir.primitive

import inca.ir.{Atom, BaseIR, ModuleEntry, Term, Type}
import inca.ir.visitors.BaseIRVisitor
import inca.ir.Hint.preserveHints
import inca.ir.extension.aggregate
import inca.ir.extension.mono.MonoAggregationOperator

import scala.collection.immutable.Seq

trait Visitor extends BaseIRVisitor:
  override def visitModuleEntry(moduleEntry: ModuleEntry): Seq[ModuleEntry] = moduleEntry match
    case ScalaDefnModuleEntry(name, defn) => Seq(ScalaDefnModuleEntry(name, defn))
    case _ => super.visitModuleEntry(moduleEntry)

  def visitAggregationOperator(op: aggregate.AggregationOperator): aggregate.AggregationOperator = op match
    case ScalaAggregationOperator(name, ty, initCode, addCode) =>
      val vty = visitType(ty)
      ScalaAggregationOperator(name, vty, initCode, addCode)
    case ScalaMonoAggregationOperator(name, inputTy, stateTy, initCode, addCode) =>
      ScalaMonoAggregationOperator(name,
        visitType(inputTy),
        visitType(stateTy),
        initCode, addCode
      )
    case _ => op

  override def visitAtom(atom: Atom): Seq[Atom] = atom match
    case ScalaAggregationAtom(op, rel, out, args, col) =>
      Seq(ScalaAggregationAtom(visitAggregationOperator(op), rel, visitTerm(out).head, args.flatMap(visitTerm), col))
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