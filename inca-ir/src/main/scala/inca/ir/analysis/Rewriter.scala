package inca.ir.analysis

import inca.ir.visitors.IRVisitor
import inca.ir
import inca.ir.BaseIR
import inca.ir.lowering.BaseLowering
import inca.ir.optimize.BaseIROptimizer
import inca.ir.typing.{BaseIRTypechecker, IRTypechecker}

trait Rewriter(optimizer: (IRAbstractInterpreter) => BaseIROptimizer) extends BaseLowering:
  override def name: String = "Rewriter"
  override def loweredIRs: Set[BaseIR] = Set(ir.BaseIR)
  override def requiredIRs: Set[inca.ir.BaseIR] = Set(ir.BaseIR)

  protected def typechecker: BaseIRTypechecker = new IRTypechecker

  override def visitProgram(modules: Seq[ir.Module]): Seq[ir.Module] =
    val aeval = new IRAbstractInterpreter
    modules.foreach(aeval.evalModule)
    val opt = optimizer(aeval)

    println(modules)

    val po = opt.visitProgram(modules)
    val checker = typechecker
    checker.checkProgram(po)
    po
