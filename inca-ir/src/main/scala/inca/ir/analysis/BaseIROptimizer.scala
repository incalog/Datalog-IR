package inca.ir.analysis

import inca.ir.*
import inca.ir.extension.*
import inca.ir.visitors.IRVisitor

trait BaseIROptimizer(val analysis: IRAbstractInterpreter) extends IRVisitor:
  import analysis.{AtomKey, AtomResult, TermKey, TermResult}

  def atomResult(atom: Atom): VBool =
    atom.getAnalysisResult(AtomKey).getOrElse(throw new IllegalStateException(s"No atom result $atom")).value
  def isTrue(atom: Atom): Boolean = atomResult(atom) == VBool.True

  def termResult(term: Term): Option[Value] =
    term.getAnalysisResult(TermKey).map(_.value)

//  override def visitBody(body: Body): Seq[Body] =
//    val bodies = super.visitBody(body)
//    bodies.map { b =>
//      val readVars = b.vars.filter(_.mode.isBound)
//      val atoms = b.atoms.filter(a =>
//        !isTrue(a) || a.vars.exists(v => readVars.contains(a))
//      )
//      Body(atoms)
//    }

  override def visitAtom(atom: Atom): Seq[Atom] = atomResult(atom) match
    case VBool.False => throw FailedBody
    case VBool.True if atom.vars.forall(!_.mode.isBinding) => Seq()
    case _ => super.visitAtom(atom)

class IROptimizer(analysis: IRAbstractInterpreter) extends BaseIROptimizer(analysis) with arithmetic.Optimizer

