package inca.viatra.optimize

import inca.ir.visitors.IRVisitor
import inca.viatra.ir.primitiveScala.Visitor

trait Optimizer extends Visitor with IRVisitor
