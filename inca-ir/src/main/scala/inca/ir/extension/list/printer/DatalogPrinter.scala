package inca.ir.extension.list.printer

import inca.ir.{Atom, Term, Type}
import inca.ir.extension.list.{TList, ListLit, IsEmpty, Head, Size, Tail, Deconstruct, Append, Prepend}
import inca.ir.printer.DatalogBaseIRPrinter

trait DatalogPrinter extends DatalogBaseIRPrinter:
  override def prettyPrint(atom: Atom): String = atom match
    case Deconstruct(list, hd, tl) => s"?${prettyPrint(list)}(${prettyPrint(hd)}, ${prettyPrint(tl)})"
    case _ => super.prettyPrint(atom)

  override def prettyPrint(term: Term): String = term match
    case ListLit(ts) => ts.map(prettyPrint).mkString("List(", ", ", ")")
    case Size(list) => s"${prettyPrint(list)}.size"
    case Head(list) => s"${prettyPrint(list)}.head"
    case Tail(list) => s"${prettyPrint(list)}.tail"
    case IsEmpty(list) => s"${prettyPrint(list)}.isEmpty"
    case Append(list, element) => s"${prettyPrint(list)}.append(${prettyPrint(element)})"
    case Prepend(list, element) => s"${prettyPrint(list)}.prepend(${prettyPrint(element)})"
    case _ => super.prettyPrint(term)

  override def prettyPrint(ty: Type): String = ty match
    case TList(ty) => s"TList[${prettyPrint(ty)}]"
    case _ => super.prettyPrint(ty)


