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
  with foreign.Typechecker
  with not.Typechecker
  with set.Typechecker
  with string.Typechecker
  with aggregate.Typechecker 

enum Mode:
  case Binding
  case Bound
  case Collapse

//  inline def requiresBound: Boolean = this == Bound || this == Collapse

  def isBinding: Boolean = this == Binding
  def isBound: Boolean = this == Bound
  def isCollapse: Boolean = this == Collapse

  def inverted: Mode = this match
    case Binding => Bound
    case Bound | Collapse => Binding

  def weakInverted: Mode = this match
    case Binding => Collapse
    case Bound | Collapse => Binding

  def ||(that: Mode): Mode = this match
    case Binding => Binding
    case Bound => that match
      case Binding => Binding
      case Bound => Bound
      case Collapse => Bound
    case Collapse => that

enum VarMode:
  case Bound
  case Unbound

  def &&(that: VarMode): VarMode = this match
    case Bound => that
    case Unbound => Unbound
