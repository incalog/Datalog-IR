package inca.ir.extension.map

import inca.ir.{Atom, Term, Type}
import inca.ir.visitors.BaseIRVisitor
import inca.ir.Hint.preserveHints

trait Visitor extends BaseIRVisitor:
  override def visitType(ty: Type): Type = ty match
    case TMap(tyK, tyV) => TMap(visitType(tyK), visitType(tyV))
    case _ => super.visitType(ty)

  override def visitTerm(term: Term): Seq[Term] = preserveHints(term) {
    term match
      case MapLit(ts) =>
        val tts: Seq[(Term, Term)] = ts.flatMap {
          case (k, v) => visitTerm(k).zip(visitTerm(v))
        }
        Seq(MapLit(tts))
      case MapFrom(ref) =>
        Seq(MapFrom(visitRef(ref)))
      case MapFun(params, valTerm) =>
        visitTerm(valTerm).map(MapFun(params.flatMap(visitParam), _))
      case MapPlus(map, key, value) =>
        visitTerm(map) zip visitTerm(key) zip visitTerm(value) map { case ((a, b), c) => MapPlus(a, b, c) }
      case MapUnion(t1, t2) =>
        visitTerm(t1).zip(visitTerm(t2)).map(MapUnion.apply)
      case MapConcat(t1, t2) =>
        visitTerm(t1).zip(visitTerm(t2)).map(MapConcat.apply)
      case MapComprehension(key, value, atoms) =>
        for ((k, v) <- visitTerm(key).zip(visitTerm(value))) yield
          MapComprehension(k, v, atoms.flatMap(visitAtom))
      case MapLookUp(map, key) => visitTerm(map).zip(visitTerm(key)).map(MapLookUp.apply)
      case _ => super.visitTerm(term)
  }

  override def visitAtom(atom: Atom): Seq[Atom] = preserveHints(atom) {
    atom match
      case MapContains(map, key) => visitTerm(map).zip(visitTerm(key)).map(MapContains.apply)
      case _ => super.visitAtom(atom)
  }
