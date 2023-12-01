package inca.ir.visitors

import inca.ir.extension.*

trait IRVisitor extends BaseIRVisitor
  with aggregate.Visitor
  with aggregateset.Visitor
  with arithmetic.Visitor
  with data.Visitor
  with block.Visitor
  with bool.Visitor
  with datamatch.Visitor
  with demand.Visitor
  with disjunction.Visitor
  with impure.Visitor
  with not.Visitor
  //with primitiveScala.Visitor
  with set.Visitor
  with map.Visitor
  with string.Visitor
  with tuple.Visitor
  with mono.Visitor
  with typeparam.Visitor

