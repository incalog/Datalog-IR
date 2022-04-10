package inca.frontend.souffle

import inca.frontend.souffle.Syntax._

object FreeVars {
  def freeVars(argument: Argument): Set[String] = argument match {
    case ArgumentConstant(_) => Set()
    case ArgumentVariable(name) => Set(name)
    case Syntax.ArgumentNil => Set()
    case ArgumentList(args) => args.flatMap(freeVars).toSet
    case ArgumentBranchConstructor(_, args) => args.flatMap(freeVars).toSet
    case ArgumentSingle(arg) => freeVars(arg)
    case ArgumentAlias(arg, _) => freeVars(arg)
    case ArgumentIntrinsicFunc(_, args) => args.flatMap(freeVars).toSet
    case ArgumentUserDefinedFunc(_, args) => args.flatMap(freeVars).toSet
    case ArgumentAggregator(aggregator) => freeVars(aggregator)
    case ArgumentUnOp(op, arg) => freeVars(arg)
    case ArgumentBinOp(op, l, r) => freeVars(l) ++ freeVars(r)
  }

  def freeVars(aggregator: Aggregator): Set[String] = aggregator match {
    case AggregatorMin(argument, cond) => ???
    case AggregatorMax(argument, cond) => ???
    case AggregatorMean(argument, cond) => ???
    case AggregatorSum(argument, cond) => ???
    case AggregatorCount(cond) => ???
    case AggregatorRange(arg1, arg2, arg3) => ???
  }

  def freeVars(atom: Atom): Set[String] =
    atom.args.flatMap(freeVars).toSet

  def freeVars(constraint: Constraint): Set[String] = constraint match {
    case ConstraintCmp(ty, l, r) => ???
    case ConstraintMatch(pattern, argument) => ???
    case ConstraintContains(substring, argument) => ???
    case Syntax.ConstraintTrue => ???
    case Syntax.ConstraintFalse => ???
  }

  def freeVars(term: Term): Set[String] = term match {
    case TermConjunction(terms, _) => terms.flatMap(freeVars).toSet
    case TermDisjunction(terms, _) => terms.flatMap(freeVars).toSet
    case TermAtom(atom, _) => freeVars(atom)
    case TermConstraint(constraint, _) => freeVars(constraint)
  }
}
