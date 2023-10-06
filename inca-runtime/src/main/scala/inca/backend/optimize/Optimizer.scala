package inca.backend.optimize

import inca.ir.visitors.IRVisitor
import inca.ir.extension.primitiveScala.Visitor

trait Optimizer extends Visitor with IRVisitor
