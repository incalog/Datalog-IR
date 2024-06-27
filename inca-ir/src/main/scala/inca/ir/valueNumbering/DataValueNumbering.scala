package inca.ir.valueNumbering
import inca.ir.{Ref, Term}
import inca.ir.extension.data.{CaseDefinition, Construct}

trait DataValueNumbering extends BaseValueNumbering {

  override def isConst(term: Term): Boolean = term match {
    case Construct(caseRef, args) => args.forall(isConst)
    case _ => super.isConst(term)
  }

  override def normalize(term: Term): Term = super.normalize(term)

}
