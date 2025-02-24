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

// A singleton variable is a variable that has exactly one binding side and is not used
// otherwise in a body. We can replace these variables with wildcards.
// This assists backends, since the can introduce existential checks for some calls now,
// and it helps us to detect duplicated relations with a simple syntactic matching.
class ReplaceSingletonVariables extends IRVisitor with Optimizer:
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

