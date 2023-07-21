package inca.ir.visitors

import inca.ir.extension.*

trait IRVisitor extends BaseIRVisitor
  with tuple.Visitor
  with disjunction.Visitor
  with block.Visitor
  with bool.Visitor
  with not.Visitor
  with arithmetic.Visitor

