package inca.frontend.souffle

import inca.frontend.souffle.Syntax._

object FreeVars {
  def freeVars(argument: Argument): Set[String] = argument match {
    case ArgumentConstant(_) => Set()
    case ArgumentVariable(name) => Set(name)
    case Syntax.ArgumentNil => Set()
    case ArgumentList(args) => args.flatMap(freeVars).toSet
    case ArgumentDollarFunctor(_, args) => args.flatMap(freeVars).toSet
    case ArgumentSingle(arg) => freeVars(arg)
    case ArgumentAlias(arg, _) => freeVars(arg)
    case ArgumentFunctorCall(_, args) => args.flatMap(freeVars).toSet
    case ArgumentAggregator(aggregator) => freeVars(aggregator)
    case ArgumentUnOp(op, arg) => freeVars(arg)
    case ArgumentBinOp(op, l, r) => freeVars(l) + freeVars(r)
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

  def freeVars(term: ConjunctionTerm): Set[String] = term match {
    case ConjunctionTermAtom(isNegated, atom) => ???
    case ConjunctionTermConstraint(isNegated, constraint) => ???
    case ConjunctionTermDisjunction(isNegated, disjunction) => ???
  }

  def freeVars(conj: Conjunction): Set[String] =
    conj.terms.flatMap(freeVars).toSet

  def freeVars(disjunction: Disjunction): Set[String] =
    disjunction.conjunctions.flatMap(freeVars).toSet
}
