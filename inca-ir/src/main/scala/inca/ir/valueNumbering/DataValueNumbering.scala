package inca.ir.valueNumbering
import inca.ir.{Arg, Atom, Ref, RefByName, Term, TermArg, Var}
import inca.ir.extension.data.{CaseDefinition, Construct, Deconstruct}

trait DataValueNumbering extends BaseValueNumbering {

  override def isConst(term: Term): Boolean = term match {
    case Construct(caseRef, args) => args.forall(isConst)
    case _ => super.isConst(term)
  }

  override def normalize(term: Term): Term = super.normalize(term)



  override def visitAtom(atom: Atom): Seq[Atom] = atom match {
    case Deconstruct(t, caseRef, args, false) => treatBindingsInDeconstruct(t, caseRef, args)
    case _ => super.visitAtom(atom)
  }

  def treatBindingsInDeconstruct(t: Term, caseRef: Ref[CaseDefinition], args: Seq[Arg]): Seq[Deconstruct] = {
    val newTerm = visitTerm(t).head
    val newArgs: Seq[Arg] = args.flatMap(visitArg)    // TODO conservative like calls but here value could be known by looking at construct
    Seq(Deconstruct(newTerm, caseRef.name, newArgs))
  }


}
