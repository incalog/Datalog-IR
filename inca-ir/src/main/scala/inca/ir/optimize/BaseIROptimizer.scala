package inca.ir.optimize

import inca.ir.*
import inca.ir.analysis.base.values.{VBool, Value}
import inca.ir.analysis.IRAbstractInterpreter
import inca.ir.extension.*
import inca.ir.extension.arithmetic.analysis.optimize.Optimizer
import inca.ir.visitors.IRVisitor
import inca.ir.extension.arithmetic.analysis as arith

trait BaseIROptimizer(val analysis: IRAbstractInterpreter) extends IRVisitor:
  /*import analysis.{ TermKey, TermResult }

  def termResults(term: Term): Set[TermResult] =
    term.getAnalysisResult(TermKey)*/

  var params: Set[Ref[Var.Target]] = _

  override def visitRelation(relation: Relation): Seq[Relation] =
    params = relation.params.map(p => RefByName(p.name)).toSet
    super.visitRelation(relation)

  var boundBodyVars: Set[Ref[Var.Target]] = _

  override def visitBody(body: Body): Seq[Body] =
    boundBodyVars = body.vars.filter(_.mode.isBound).map(_.ref).toSet
    super.visitBody(body)

  private def atomBindsRelevantVar(atom: Atom): Boolean =
    val boundVars = atom.vars.filter(_.mode.isBinding)
    boundVars.exists(bind => boundBodyVars.contains(bind.ref) || params.contains(bind.ref))

/*override def visitAtom(atom: Atom): Seq[Atom] = atomResult(atom) match
  case VBool.False => throw FailedBody
  case VBool.True if !atomBindsRelevantVar(atom) => Seq()
  case _ =>
    //
    super.visitAtom(atom)*/

class IROptimizer(analysis: IRAbstractInterpreter) extends BaseIROptimizer(analysis)
  with arith.optimize.Optimizer

