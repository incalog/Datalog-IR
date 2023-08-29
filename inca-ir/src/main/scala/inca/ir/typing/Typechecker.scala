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
  
object Typechecker:
  lazy val typer = new Typechecker {}
  def subtype(ty1: Type, ty2: Type): Boolean = typer.subtype(ty1, ty2)

