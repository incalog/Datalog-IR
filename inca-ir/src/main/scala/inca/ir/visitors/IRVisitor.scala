package inca.ir.visitors

import inca.ir.extension.disjunction.Visitor

trait IRVisitor extends BaseIRVisitor
  with Visitor
  with TupleIRVisitor
