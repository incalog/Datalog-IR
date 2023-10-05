package inca.ir.visitors

import inca.ir.extension.*

trait IRVisitor extends BaseIRVisitor
  with arithmetic.Visitor
  with block.Visitor
  with bool.Visitor
  with data.Visitor
  with datamatch.Visitor
  with demand.Visitor
  with disjunction.Visitor
  with not.Visitor
  with primitiveScala.Visitor
  with set.Visitor
  with string.Visitor
  with tuple.Visitor

