package inca.ir.extension.map

import inca.ir.{Atom, TAny, TNothing, Term, TermType, Type}
import inca.ir.typing.{BaseIRTypechecker, Mode}

trait Typechecker extends BaseIRTypechecker{
  protected override def inferTermExtend(term: Term, mode: Mode): TermType = term match
    case MapLit(kvs) =>
      val tys = kvs map {
        case (k, v) => TMap(inferTerm(k, Mode.Bound).ty, inferTerm(v, Mode.Bound).ty)
      }
      if (tys.isEmpty)
        TMap(TNothing, TNothing).bound
      else if (tys.tail.forall(_ == tys.head))
        tys.head.bound
      else
        error(s"The types of elements in $term are not homogeneous: $tys")
        TAny.bound
    case MapUnion(t1, t2) =>
      val (TMap(k1, v1), m1): (TMap, Mode) = inferMapTerm(t1, Mode.Bound)
      val (TMap(k2, v2), m2): (TMap, Mode) = inferMapTerm(t2, Mode.Bound)
      assertComparable(k1, k2, term) // when to use checkTerm?
      assertComparable(v1, v2, term)
      TermType(TMap(k1, v1), m1 || m2)
    case MapComprehension(key, value, atoms) =>
      atoms.foreach(checkAtom(_, mode.inverted))
      val TermType(k, m1): TermType = inferTerm(key, mode)
      val TermType(v, m2): TermType = inferTerm(value, mode)
      TermType(TMap(k, v), m1 || m2)
    case MapLookUp(map, key) =>
      val (TMap(k, v), _): (TMap, Mode) = inferMapTerm(map, Mode.Bound)
      assertComparable(k, inferTerm(key, mode).ty, term)
      TermType(v, Mode.Binding)
    case _ => super.inferTermExtend(term, mode)

  override def checkAtom(atom: Atom, mode: Mode): Unit = atom match
    case MapContain(map, key) =>
      val (TMap(k, _), _): (TMap, Mode) = inferMapTerm(map, Mode.Bound)
      checkTerm(key, k, Mode.Bound)
    case MapAddEntry(map, key, value) =>
      val (TMap(k, v), m): (TMap, Mode) = inferMapTerm(map, Mode.Bound)
      checkTerm(key, k, Mode.Bound)
      checkTerm(value, v, Mode.Bound)
    case _ => super.checkAtom(atom, mode)

  private def inferMapTerm(t: Term, mode: Mode): (TMap, Mode) = inferTerm(t, mode) match
    case TermType(ty: TMap, m) => (ty, m)
    case TermType(ty, m) =>
      error(s"Expected map type but got $ty", t)
      (TMap(TNothing, TNothing), m)

  override def checkType(ty: Type): Unit = ty match
    case TMap(kty, vty) =>
      checkType(kty)
      checkType(vty)
    case _ => super.checkType(ty)

}