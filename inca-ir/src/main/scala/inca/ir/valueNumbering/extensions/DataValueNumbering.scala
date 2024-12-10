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

  // TODO refactor & cover more cases ?
  def treatBindingsInDeconstruct(t: Term, caseRef: Ref[_ <: CaseDefinitionReference], args: Seq[Arg]): Seq[Deconstruct] = {

    def treatBinding(vari: Var, term: Term): Term = {
      val vn = getIdOf(term)
      if (vnTables.isCongrClassContained(vn)){
        updateCongrClassIfNecessary(vn, vari)
      }
      else{
        vnTables.addCongrClass(CongrClass(vn, vari, term))
        updateCongrClassIfNecessary(vn,term)
      }
      validBody &= vnTables.updateValueNumbersAndCongrClasses(vari, vn)
      if (!isParam(vari)) return vnTables.getReplacement(vari)
      else return vari
    }

    val construct = visitTerm(t).head
    construct match {
      case Construct(caseRef: Ref[_ <: CaseDefinitionReference], args_constr: Seq[Term]) =>
        val newArgs: Seq[Arg] = args.zipWithIndex.map {
          case (TermArg(vari@Var(_)), idx) if vari.mode.isBinding =>
            val newArg = treatBinding(vari, args_constr(idx))
            TermArg(newArg)
          case (arg,_) => visitArg(arg).head
        }
        Seq(Deconstruct(construct, caseRef.name, newArgs))

      case _ => {
        val newArgs: Seq[Arg] = args.flatMap(visitArg)
        Seq(Deconstruct(construct, caseRef.name, newArgs))
      }
    }

  }


}
