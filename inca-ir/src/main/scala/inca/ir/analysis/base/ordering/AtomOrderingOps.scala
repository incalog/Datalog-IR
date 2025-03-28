package inca.ir.analysis.base.ordering

import inca.ir.{Atom, Eq, ExtensionalCall}

trait Priority(val rawValue: Int)

object Priority:
  case object Highest extends Priority(0)
  case object High extends Priority(1)
  case object Normal extends Priority(2)
  case object Low extends Priority(3)
  case object Lowest extends Priority(4)

  given Conversion[Priority, Int] with
    def apply(priority: Priority): Int = priority.rawValue

trait AtomOrderingOps:
  def priority(at: Atom): Int

trait BaseAtomOrderingOps extends AtomOrderingOps:
  def priority(at: Atom): Int = at match
    case _: Eq => Priority.Highest
    case _: ExtensionalCall => Priority.High
    case _ => Priority.Normal
