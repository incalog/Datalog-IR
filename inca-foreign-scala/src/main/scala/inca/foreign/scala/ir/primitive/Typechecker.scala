package inca.foreign.scala.ir.primitive

import inca.foreign.scala.ir.primitive.*
import inca.ir.extension.data.DataDefinition
import inca.ir.extension.foreign.ForeignModuleEntry
import inca.ir.typing.{BaseIRTypechecker, Mode}
import inca.ir.{Atom, ExtensionalRelation, ModuleEntry, Relation, TAny, Term, TermType, Type, string2name}

trait Typechecker extends BaseIRTypechecker:
  override def typecheck(moduleEntry: ModuleEntry): Unit = moduleEntry match
    case ScalaDefnModuleEntry(_, _) => // nothing
    case _ => super.typecheck(moduleEntry)

  override def checkAtom(atom: Atom, mode: Mode): Unit = atom match
    case ScalaAggregationAtom(_, rel, out, ty, args, aggregatedColumn) =>
      if (aggregatedColumn >= args.size)
        error(s"Aggregated column index $aggregatedColumn out of bounds ${args.size}")
      val params = lookupRelationParams(rel, args.size, atom)
      val argMode = mode match
        case Mode.Binding => Mode.Collapse // TODO: Verify this
        case Mode.Bound => Mode.Collapse
        case Mode.Collapse => Mode.Collapse
      args.zip(params).zipWithIndex.foreach {
        case ((t, p), i) if i == aggregatedColumn =>
          assertComparable(p.ty, ty, atom)
          checkTerm(t, p.ty, argMode)
        case ((t, p), i) =>
          checkTerm(t, p.ty, argMode)
      }
      checkTerm(out, ty, mode)
    case _ => super.checkAtom(atom, mode)

  protected override def inferTermExtend(term: Term, mode: Mode): TermType = term match
    case ScalaTerm(_, ty, args, _) =>
      args.foreach(inferTerm(_, Mode.Bound))
      ty.bound
    case ScalaConstantTerm(_, ty) =>
      ty.bound
    case _ => super.inferTermExtend(term, mode)
