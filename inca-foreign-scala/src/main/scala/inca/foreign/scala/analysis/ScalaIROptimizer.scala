package inca.foreign.scala.analysis

import inca.foreign.scala.visitors.ScalaVisitor
import inca.ir.analysis.IROptimizer

class ScalaIROptimizer(analyzer: ScalaAbstractInterpreter) extends IROptimizer(analyzer) with ScalaVisitor
