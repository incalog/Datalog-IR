package inca.ir.typing

import inca.ir.Type
import inca.ir.extension.*

trait Typechecker extends BaseIRTypechecker
  with tuple.Typechecker
  with disjunction.Typechecker
  with block.Typechecker
  with arithmetic.Typechecker
  with data.Typechecker
  with not.Typechecker
  with set.Typechecker
  with primitiveScala.Typechecker

enum Mode:
  case Closing
  case Closed

  inline def isClosing: Boolean = this == Closing
  inline def requiresClosed: Boolean = !isClosing
  def inverted: Mode = this match
    case Closing => Closed
    case Closed => Closing

enum VarMode:
  case Bound
  case Unbound

object Typechecker:
  lazy val typer = new Typechecker {}

