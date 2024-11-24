package inca.ir.valueNumbering.extensions

import inca.ir.valueNumbering.BaseValueNumbering
import inca.ir.{Arg, Atom, Ref, RefByName, Relation, Var}
import inca.ir.extension.aggregate.{Aggregate, AggregateColumnArg, AggregationOperator}

trait AggregateValueNumbering extends BaseValueNumbering {

  // no new terms -> no implementation of isConst or normalization necessary
  // but includes new Arg in which terms are bound -> treat bindings
  
  override def visitArg(arg: Arg): Seq[Arg] = arg match {
    case AggregateColumnArg(vari@Var(RefByName(variName))) if vari.mode.isBinding =>
      Seq(AggregateColumnArg(conservativeBinding(vari)))
    case _ => super.visitArg(arg)
  }

}
