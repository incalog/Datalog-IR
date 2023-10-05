package inca.ir.extension.not

import inca.ir.Hint.preserveHints
import inca.ir.visitors.BaseIRVisitor
import inca.ir.*

trait Visitor extends BaseIRVisitor:
  override def visitAtom(atom: Atom): Seq[Atom] = preserveHints(atom) {
    atom match
      case Not(at) =>
        visitAtom(at).map(Not.apply)
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


