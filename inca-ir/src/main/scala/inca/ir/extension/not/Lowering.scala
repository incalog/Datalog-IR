package inca.ir.extension.not

import inca.ir.*
import inca.ir.Hint.preserveHints
import inca.ir.extension.*
import inca.ir.lowering.BaseLowering
import inca.ir.{Atom, BaseIR, Body, NegExtensionalCall, Term}

import scala.collection.mutable.ListBuffer

trait Lowering extends BaseLowering:

  override def loweredIRs: Set[BaseIR] = Set(IR)
  override def requiredIRs: Set[BaseIR] = Set()

  override def visitAtom(atom: Atom): Seq[Atom] = preserveHints(atom) {
    atom match
      case Not(atom) => visitAtom(negateAtom(atom))
      case _ => super.visitAtom(atom)
  }

  def negateAtom(atom: Atom): Atom = atom match
    case Call(name, args) => NegCall(name, args)
    case NegCall(name, args) => Call(name, args)
    case ExtensionalCall(name, args) => NegExtensionalCall(name, args)
    case NegExtensionalCall(name, args) => ExtensionalCall(name, args)
    case Eq(lhs, rhs) => Neq(lhs, rhs)
    case Neq(lhs, rhs) => Eq(lhs, rhs)
    case Not(at) => at



