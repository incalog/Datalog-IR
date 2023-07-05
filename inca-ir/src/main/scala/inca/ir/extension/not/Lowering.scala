package inca.ir.extension.not

import inca.ir.*
import inca.ir.extension.*
import inca.ir.lowering.BaseLowering
import inca.ir.{Atom, BaseIR, Body, NegExtensionalCall, Term}

import scala.collection.mutable.ListBuffer


trait Lowering[S <: IR, T <: BaseIR] extends BaseLowering[S, T]:

  override def loweredIRs: Set[BaseIR] = super.loweredIRs ++ Set(IR)

  override def visitAtom(atom: Atom): Seq[Atom] = atom match
    case Not(atom) => visitAtom(negateAtom(atom))
    case _ => super.visitAtom(atom)

  def negateAtom(atom: Atom): Atom = atom match
    case Call(name, args) => NegCall(name, args)
    case NegCall(name, args) => Call(name, args)
    case ExtensionalCall(name, args) => NegExtensionalCall(name, args)
    case NegExtensionalCall(name, args) => ExtensionalCall(name, args)
    case Eq(lhs, rhs) => Neq(lhs, rhs)
    case Neq(lhs, rhs) => Eq(lhs, rhs)
    case Not(at) => at



