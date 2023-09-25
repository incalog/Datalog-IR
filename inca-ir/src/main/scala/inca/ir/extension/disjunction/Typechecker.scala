package inca.ir.extension.disjunction

import inca.ir.Atom
import inca.ir.extension.disjunction.Disjunction
import inca.ir.typing.{BaseIRTypechecker, Mode}

trait Typechecker extends BaseIRTypechecker:
  override def checkAtom(atom: Atom, mode: Mode): Unit = atom match
    case Disjunction(Seq()) => // nothing
    case Disjunction(alts) =>
      val as = alts.head
      val ass = alts.tail
      val varsBefore = this.vars
      as.foreach(checkAtom(_, mode))
      var varsAfter = vars
      ass.foreach { as =>
        vars = varsBefore
        as.foreach(checkAtom(_, mode))
        val varsAfterThis = vars
        // remove variables not bound by this alternative
        varsAfter = varsAfter.filter(kv => varsAfterThis.contains(kv._1))
        for ((x, VarInfo(_, ty2, vmode2)) <- varsAfter) varsAfter.get(x) match
          case None => // nothing
          case Some(VarInfo(trg1, ty1, vmode1)) =>
            varsAfter += x -> VarInfo(trg1, meet(ty1, ty2), vmode1 && vmode2)
      }
      vars = varsAfter
    case _ => super.checkAtom(atom, mode)