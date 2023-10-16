package inca.ir.analysis

import inca.ir.*
import inca.ir.extension.*
import inca.ir.visitors.IRVisitor

trait BaseIROptimizer(val analysis: IRAbstractInterpreter) extends IRVisitor:
  import analysis.{AtomKey, TermKey}

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

  var params: Set[Name] = _

  override def visitRelation(relation: Relation): Seq[Relation] =
    params = relation.params.map(_.name).toSet
    super.visitRelation(relation)

  var boundBodyVars: Set[Name] = _
  override def visitBody(body: Body): Seq[Body] =
    boundBodyVars = body.vars.filter(_.mode.isBound).map(_.name).toSet
    super.visitBody(body)

  private def atomBindsRelevantVar(atom: Atom): Boolean =
    val boundVars = atom.vars.filter(_.mode.isBinding)
    boundVars.exists(bind => boundBodyVars.contains(bind.name) || params.contains(bind.name))

  override def visitAtom(atom: Atom): Seq[Atom] = atomResult(atom) match
    case VBool.False => throw FailedBody
    case VBool.True if !atomBindsRelevantVar(atom) => Seq()
    case _ =>
      //
      super.visitAtom(atom)

class IROptimizer(analysis: IRAbstractInterpreter) extends BaseIROptimizer(analysis) with arithmetic.Optimizer

