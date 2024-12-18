package inca.ir.extension.map.printer

import inca.ir.{Atom, Term, Type}
import inca.ir.extension.map.{TMap, MapFun, MapFrom, MapPlus, MapComprehension, MapConcat, MapContains, MapLookUp, MapUnion, MapLit}
import inca.ir.printer.BaseIRPrinter

trait Printer extends BaseIRPrinter:
  override def prettyPrint(atom: Atom): String = atom match
    case MapContains(map, key) => s"${prettyPrint(key)} in ${prettyPrint(map)}"
    case _ => super.prettyPrint(atom)

  override def prettyPrint(term: Term): String = term match
    case MapLit(ts) => s"Map(${ts.map { (t1, t2) => s"${prettyPrint(t1)} -> ${prettyPrint(t2)}" }.mkString(", ")})"
    case MapFrom(name) => s"Map.from(${prettyPrint(name)})"
    case MapFun(params, valTerm) => s"MapFun(${params.map(prettyPrint).mkString(", ")} => ${prettyPrint(valTerm)})"
    case MapPlus(map, key, value) => s"${prettyPrint(map)} += ${prettyPrint(key)} -> ${prettyPrint(value)}"
    case MapUnion(t1, t2) => s"${prettyPrint(t1)} ∪ ${prettyPrint(t2)}"
    case MapConcat(t1, t2) => s"${prettyPrint(t1)} ++ ${prettyPrint(t2)}"
    case MapComprehension(key, value, atoms) => s"{ ${prettyPrint(key)} -> ${prettyPrint(value)} | ${atoms.map(prettyPrint).mkString(",")} }"
    case MapLookUp(map, key) => s"${prettyPrint(map)}(${prettyPrint(key)})"
    case _ => super.prettyPrint(term)

  override def prettyPrint(ty: Type): String = ty match
    case TMap(k, v) => s"Map[${prettyPrint(k)}, ${prettyPrint(v)}]"
    case _ => super.prettyPrint(ty)


