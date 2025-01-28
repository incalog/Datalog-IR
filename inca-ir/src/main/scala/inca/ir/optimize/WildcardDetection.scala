package inca.ir.optimize

import inca.ir
import inca.ir.Hint.preserveHints
import inca.ir.analysis.{AnalysisKey, AnalysisResult}
import inca.ir.extension.aggregate.AggregateColumnArg
import inca.ir.printer.IRDebugPrinter
import inca.ir.util.SourceLocation
import inca.ir.{Arg, Atom, Body, Call, Eq, ExtensionalCall, Ref, RefByName, Relation, Term, TermArg, Var, WildcardArg}
import inca.ir.visitors.IRVisitor

import scala.collection.{immutable, mutable}
import scala.collection.immutable.Queue
import scala.compiletime.uninitialized

// Detect if binding arguments are unused, and as such can be replaced by Wildcards.
// While mostly cosmetic, this optimization also helps to execute Souffle programs. There is a bug in Souffle, that
// incorrectly detects binding variables as ungrounded. However, this is not the case if a wildcard is used.
class WildcardDetection extends IRVisitor with Optimizer:
  override val name: String = "Wildcard rewriting"

  var params: Set[Ref[Var.Target]] = Set()
  var relevantBodyVars: Set[Ref[Var.Target]] = Set()

  def argUsesRelevantVar(arg: Arg): Boolean = arg match
    case TermArg(t) if t.typ.get.mode.isBinding =>
      t.vars.exists(bind => relevantBodyVars.contains(bind.ref) || params.contains(bind.ref))
    case _ => true

  override def visitRelation(relation: Relation): Seq[Relation] =
    params = relation.params.map(p => RefByName(p.name)).toSet
    super.visitRelation(relation)

  override def visitBody(body: Body): Seq[Body] = preserveHints(body) {
    relevantBodyVars = body.vars.filter(_.mode.isBound).map(_.ref).toSet
    super.visitBody(body)
  }

  override def visitArg(arg: Arg): Seq[Arg] =
    if (argUsesRelevantVar(arg))
      super.visitArg(arg)
    else
      logOptimizationStat("Replaced variables", 1, _ + 1)
      Seq(WildcardArg())

