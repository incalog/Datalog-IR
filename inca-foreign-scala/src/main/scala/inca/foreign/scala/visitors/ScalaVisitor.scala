package inca.foreign.scala.visitors

import inca.ir.visitors.IRVisitor
import inca.foreign.scala.ir.*

trait ScalaVisitor extends IRVisitor with primitive.Visitor