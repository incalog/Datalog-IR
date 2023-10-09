package inca.ir.extension.not

import inca.ir.Hint.preserveHints
import inca.ir.visitors.BaseIRVisitor
import inca.ir.*
import inca.ir.extension.disjunction.{Disjunction, DisjunctionAlternative}

import scala.collection.mutable.ListBuffer

trait Visitor extends BaseIRVisitor:
  override def visitAtom(atom: Atom): Seq[Atom] = preserveHints(atom) {
    atom match
      case Not(at) =>
        val atoms = visitAtom(at)
        if (atoms.isEmpty)
          throw FailedBody
        else if (atoms.size == 1)
          Seq(Not(atoms.head))
        else {
          val alts = new ListBuffer[Seq[Atom]]
          val prefix = new ListBuffer[Atom]
          for (atom <- atoms) {
            alts += prefix.toList :+ Not(atom)
            prefix += atom
          }
          val disjunction = Disjunction(alts.toList.map(DisjunctionAlternative.apply))
          Seq(disjunction)
        }
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


