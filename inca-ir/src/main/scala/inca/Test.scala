package inca.ir

import inca.ir.extensions.*
import inca.ir.typing.{CompilationMessage, Typechecker}

import scala.collection.immutable.Seq

case class Failed(messages: Seq[CompilationMessage]) extends Exception(messages.mkString("\n"))

