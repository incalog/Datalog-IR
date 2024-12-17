package inca.ir.extension.set.printer

import inca.ir.{Atom, Term, Type}
import inca.ir.extension.set.{TSet, SetLit, SetFrom, SetMember, SetUnion, SetIntersection, SetComprehension}
import inca.ir.printer.DatalogBaseIRPrinter

trait DatalogPrinter extends DatalogBaseIRPrinter:
  override def prettyPrint(atom: Atom): String = atom match
    case SetMember(mem, s) => s"(${prettyPrint(mem)} in ${prettyPrint(s)})"
    case _ => super.prettyPrint(atom)

  override def prettyPrint(term: Term): String = term match
    case SetLit(ts) => ts.map(prettyPrint).mkString("Set(", ", ", ")")
    case SetFrom(name) => s"Set.from(${prettyPrint(name)})"
    case SetUnion(ts) => ts.map(prettyPrint).mkString("(", " ∪ ", ")")
    case SetIntersection(t1, t2) =>  s"(${prettyPrint(t1)} ∩ ${prettyPrint(t2)}"
    case SetComprehension(elem, atoms) => s"Set(${prettyPrint(elem)} | ${atoms.map(prettyPrint).mkString(", ")})"
    case _ => super.prettyPrint(term)

  override def prettyPrint(ty: Type): String = ty match
    case TSet(ts) => s"TSet[${prettyPrint(ty)}]"
    case _ => super.prettyPrint(ty)


