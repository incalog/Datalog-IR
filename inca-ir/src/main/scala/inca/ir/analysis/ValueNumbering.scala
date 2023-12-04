package inca.ir.analysis
import inca.ir
import inca.ir.visitors.IRVisitor

// TODO inherit from IROptimizer or IRVisitor
//class ValueNumbering(analysis: IRAbstractInterpreter) extends IROptimizer(analysis) {
class ValueNumbering extends IRVisitor {

  def valueNumbering(module: ir.Module): ir.Module = {
    // TODO
    module
  }

}
