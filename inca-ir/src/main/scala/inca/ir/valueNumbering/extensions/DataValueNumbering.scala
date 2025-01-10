package inca.ir.valueNumbering.extensions

import inca.ir.valueNumbering.BaseValueNumbering
import inca.ir.{Arg, Atom, Ref, RefByName, Term, TermArg, Var}
import inca.ir.extension.data.{CaseDefinition, CaseDefinitionReference, Construct, Deconstruct}

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
  

  private def treatBindingsInDeconstruct(t: Term, caseRef: Ref[_ <: CaseDefinitionReference], args: Seq[Arg]): Seq[Deconstruct] = {
    val newTerm = visitTerm(t).head

    newTerm match {
      case Construct(caseRef, args_constr) =>
        val newArgs: Seq[Arg] = treatBindingsWithIndex(args, args_constr)
        Seq(Deconstruct(newTerm, caseRef.name, newArgs))
      case _ =>
        // if term in Deconstruct is not a Construct (because e.g. Construct is not a constant)
        // search equivalent terms for Construct and use it to value number arguments of Deconstruct
        vnTables.getConstruct(getIdOf(newTerm)) match {
          case Some(Construct(caseRef, args_constr)) =>
            val newArgs: Seq[Arg] = treatBindingsWithIndex(args, args_constr)
            Seq(Deconstruct(newTerm, caseRef.name, newArgs))
          case None =>
            val newArgs: Seq[Arg] = args.flatMap(visitArg)
            Seq(Deconstruct(newTerm, caseRef.name, newArgs))
        }
    }

  }


}
