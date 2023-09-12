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

  inline def requiresClosed: Boolean = this == Closed
  def inverted: Mode = this match
    case Closing => Closed
    case Closed => Closing
  
  def ||(that: Mode): Mode =
    if (this == Closing || that == Closing)
      Closing
    else
      Closed

enum VarMode:
  case Bound
  case Unbound

