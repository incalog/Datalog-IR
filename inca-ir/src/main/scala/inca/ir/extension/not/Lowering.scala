package inca.ir.extension.not

import inca.ir.*
import inca.ir.Hint.preserveHints
import inca.ir.extension.*
import inca.ir.lowering.BaseLowering
import inca.ir.{Atom, BaseIR, Body, Term}
import inca.ir.typing.Mode

import scala.collection.mutable.ListBuffer

trait Lowering extends BaseLowering:

  override def loweredIRs: Set[BaseIR] = Set(IR)
  override def requiredIRs: Set[BaseIR] = Set()

  var collapseTerms: Boolean = false
  def collapseBindingTerms[A](f: => A): A =
    collapseTerms = true
    val t = f
    collapseTerms = false
    t

  override def visitAtom(atom: Atom): Seq[Atom] = preserveHints(atom) {
    atom match
      case Not(atom) => visitAtom(negateAtom(atom))
      case WeakNot(atom) => collapseBindingTerms(visitAtom(negateAtom(atom)))
      case _ => super.visitAtom(atom)
  }

  override def visitArg(arg: Arg): Seq[Arg] = arg match
    case TermArg(t) => t.typ match
      case Some(TermType(ty, Mode.Binding | Mode.Collapse)) if collapseTerms => Seq(WildcardArg)
      case _ => super.visitArg(arg)
    case _ => super.visitArg(arg)



//main_result$0: >TInt< == 
//  {x: >TInt< == 7; 
//    {{AtomAsBool(x: <TInt> > 0) == true, if_result$0: >TInt< == x: <TInt>} or 
//    {AtomAsBool(x: <TInt> > 0) == false, if_result$0: >TInt< == x: <TInt> * -1}; 
//  if_result$0: <TInt>}}
//
//  main_result$0: >TInt< == 
//    {x: >TInt< == 7; 
//      {{{{x: <TInt> > 0, Boolean$0: >TInt< == 1} or 
//         {not(x: <TInt> > 0), Boolean$0: >TInt< == 0}; Boolean$0: <TInt>} == 1, if_result$0: >TInt< == x: <TInt>} or 
//      {{{x: <TInt> > 0, Boolean$1: >TInt< == 1} or
//        {not(x: <TInt> > 0), Boolean$1: >TInt< == 0}; Boolean$1: <TInt>} == 0, if_result$0: >TInt< == x: <TInt> * -1}; if_result$0: <TInt>}}

//{{x: <TInt> > 0, Boolean$0: >TInt< == 1} or
// {not(x: <TInt> > 0), Boolean$0: >TInt< == 0}, 
// Boolean$0: <TInt> == 1, if_result$0: >TInt< == x: <TInt>} or
//      
//{{x: <TInt> > 0, Boolean$1: >TInt< == 1} or
// {not(x: <TInt> > 0), Boolean$1: >TInt< == 0}, 
// Boolean$1: <TInt> == 0, if_result$0: >TInt< == x: <TInt> * -1}
//  