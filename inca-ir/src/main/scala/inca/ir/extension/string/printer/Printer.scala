package inca.ir.extension.string.printer

import inca.ir.{Atom, Term, Type}
import inca.ir.extension.string.{OrdinalNumber, RegexMatch, StringConcat, StringLength, StringLit, Substring, TString, ToString}
import inca.ir.printer.BaseIRPrinter

trait Printer extends BaseIRPrinter:
  override def prettyPrint(atom: Atom): String = atom match
    case RegexMatch(t, pattern, neg) =>
      val negPrefix = if (neg) "~" else ""
      s"${negPrefix}reg_match(${prettyPrint(t)}, ${prettyPrint(pattern)})"
    case _ => super.prettyPrint(atom)

  override def prettyPrint(term: Term): String = term match
    case StringLit(value) => s"\"$value\""
    case StringConcat(lhs, rhs) => s"${prettyPrint(lhs)} + ${prettyPrint(lhs)}"
    case ToString(t) => s"${prettyPrint(t)}.toString"
    case Substring(t, index, length) => s"${prettyPrint(t)}[${prettyPrint(index)}..<${prettyPrint(length)}]"
    case StringLength(t) => s"${prettyPrint(t)}.length"
    case OrdinalNumber(t) => s"ord(${prettyPrint(t)})"
    case _ => super.prettyPrint(term)

  override def prettyPrint(ty: Type): String = ty match
    case TString => "TString"
    case _ => super.prettyPrint(ty)