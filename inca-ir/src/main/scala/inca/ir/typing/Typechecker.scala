package inca.ir.typing

import inca.ir.Type
import inca.ir.extension.*

trait Typechecker extends BaseIRTypechecker
  with tuple.Typechecker
  with disjunction.Typechecker
  with block.Typechecker
  with arithmetic.Typechecker
  with data.Typechecker
  with datamatch.Typechecker
  with demand.Typechecker
  with not.Typechecker
  with set.Typechecker
  with primitiveScala.Typechecker

enum Mode:
  case Binding
  case Bound

  inline def requiresBound: Boolean = this == Bound
  def inverted: Mode = this match
    case Binding => Bound
    case Bound => Binding
  
  def ||(that: Mode): Mode =
    if (this == Binding || that == Binding)
      Binding
    else
      Bound

enum VarMode:
  case Bound
  case Unbound

