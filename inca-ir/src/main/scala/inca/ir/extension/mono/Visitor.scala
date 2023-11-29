package inca.ir.extension.mono

import inca.ir.{Atom, Term, Type}
import inca.ir.visitors.BaseIRVisitor
import inca.ir.Hint.preserveHints
import inca.ir.extension.aggregate.Aggregate

import scala.collection.immutable.Seq

trait Visitor extends BaseIRVisitor {
  override def visitTerm(term: Term): Seq[Term] = preserveHints(term)(term match
    case ReadMono(m) =>
      Seq(ReadMono(m))
    case NewMono(mono, keys, args) =>
      val visitedArgs = args.flatMap{arg => visitTerm(arg)}
      val visitedKeys = keys.map(visitType)
      Seq(NewMono(mono, visitedKeys, visitedArgs))
    case _ => super.visitTerm(term)
  )

  override def visitAtom(atom: Atom): Seq[Atom] = preserveHints(atom)(atom match
    case WriteMono(m, input, keys) =>
      val visitedM = visitTerm(m).head
      val visitedInput = visitTerm(input).head
      val visitedKey = keys.flatMap(visitTerm)
      Seq(WriteMono(visitedM, visitedInput, visitedKey))
    case _ => super.visitAtom(atom)
  )

  override def visitType(ty: Type): Type = preserveHints(ty)(ty match
    case TMono(input, output, keys) =>
      TMono(visitType(input), visitType(output), keys.map(visitType))
    case _ => super.visitType(ty)
  )
}
