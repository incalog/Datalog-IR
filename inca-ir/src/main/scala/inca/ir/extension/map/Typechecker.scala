package inca.ir.extension.map

import inca.ir.extension.demand.TDemand
import inca.ir.extension.set.TSet
import inca.ir.extension.tuple.TTuple
import inca.ir.{Atom, Relation, TAny, TNothing, Term, TermType, Type}
import inca.ir.typing.{BaseIRTypechecker, Mode}

trait Typechecker extends BaseIRTypechecker:
  protected override def checkTermExtend(term: Term, expected: Type, mode: Mode): Mode = term match
    case MapLit(Nil) => expected match
      case TMap(_, _) => Mode.Bound
      case ty =>
        assertComparable(TMap(TNothing, TNothing), ty, term)
        Mode.Bound
    case _ => super.checkTermExtend(term, expected, mode)

  protected override def inferTermExtend(term: Term, mode: Mode): TermType = term match
    case MapLit(Nil) =>
      TMap(TNothing, TNothing).bound
    case MapLit((k, v) +: kvs) =>
      val tyK = inferTerm(k, Mode.Bound).ty
      val tyV = inferTerm(v, Mode.Bound).ty
      kvs.foreach { (nextKey, nextVal) =>
        checkTerm(nextKey, tyK, Mode.Bound)
        checkTerm(nextVal, tyV, Mode.Bound)
      }
      TMap(tyK, tyV).bound
    case MapFrom(name) =>
      lookupModuleEntry(name) match
        case Some(Relation(_, params, _)) =>
          val tys = params.map(_.ty)
          // should check nondemanded is also non-empty
          val (demanded, nondemanded) = tys.partition(_.isInstanceOf[TDemand])
          val output = TTuple.make(nondemanded)
          if (demanded.isEmpty)
            TSet(output).bound
          else {
            val input = TTuple.make(demanded.map(_.asInstanceOf[TDemand].ty))
            TMap(input, output).bound
          }
        case _ =>
          error(s"Cannot find relation $name", term)
          TMap(TNothing, TNothing).bound

    case fun@MapFun(params, valTerm) => scopedVariables(fun.names) {
      params.foreach { p =>
        registerVar(p.name, p, p.ty)
        bindVar(p.name)
      }
      val keyTy = TTuple.make(params.map(_._2))
      val TermType(valTy, valMode) = inferTerm(valTerm, mode)
      TermType(TMap(keyTy, valTy), valMode)
    }
    case MapPlus(map, key, value) =>
      val (TMap(keyTy, valTy), m): (TMap, Mode) = inferMapTerm(map, Mode.Bound)
      checkTerm(key, keyTy, Mode.Bound)
      checkTerm(value, valTy, Mode.Bound)
      TermType(TMap(keyTy, valTy), Mode.Bound)
    case MapUnion(t1, t2) =>
      val tyMap = inferMapTerm(t1, Mode.Bound)._1
      checkTerm(t2, tyMap, Mode.Bound)
      TermType(tyMap, Mode.Bound)
    case MapConcat(t1, t2) =>
      val tyMap = inferMapTerm(t1, Mode.Bound)._1
      checkTerm(t2, tyMap, Mode.Bound)
      TermType(tyMap, Mode.Bound)
    case MapComprehension(key, value, atoms) =>
      atoms.foreach(checkAtom(_, mode.inverted))
      val TermType(k, m1) = inferTerm(key, mode)
      val TermType(v, m2) = inferTerm(value, mode)
      TermType(TMap(k, v), m1 || m2)
    case MapLookUp(map, key) =>
      val TMap(tyK, tyV) = inferMapTerm(map, Mode.Bound)._1
      checkTerm(key, tyK, Mode.Bound)
      TermType(tyV, Mode.Bound)
    case _ => super.inferTermExtend(term, mode)

  protected override def checkAtom(atom: Atom, mode: Mode): Unit = atom match
    case MapContains(map, key) =>
      val TMap(tyK, _) = inferMapTerm(map, Mode.Bound)._1
      checkTerm(key, tyK, mode)
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