package inca.ir.visitors

trait IRVisitor extends BaseIRVisitor
  with DisjunctionIRVisitor
  with TupleIRVisitor
