package inca.ir.visitors

import inca.ir.extension.*

trait IRVisitor extends BaseIRVisitor
  with disjunction.Visitor
  with tuple.Visitor
  with block.Visitor
  with bool.Visitor
  with not.Visitor
  with arithmetic.Visitor
  with data.Visitor
  with set.Visitor
  with primitiveScala.Visitor

