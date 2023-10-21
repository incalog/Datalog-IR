package inca.ir.extension.monotypes

import inca.ir.*
import inca.ir.extension.arithmetic.TInt

trait IR extends BaseIR:
  override val name: String = "Mono-Types"

  override def language: Language = super.language + IR

  override def requires: Language = Language()

object IR extends IR { }

case class TMono(input: Type, output: Type, keys: Seq[Type]) extends Type:
  override def toString: String = s"Mono[$input, $output]@$keys"

case class MkMono(cls: MonoTypeOperator, args: Seq[Term], keys: Seq[Type]) extends Term:
  override def vars: Seq[Var] =
    args.flatMap(_.vars)

  override def toString: String = s"new $cls(${args.mkString(",")})@${keys}"

case class AddMono(m: Term, input: Term, keys: Seq[Term]) extends Atom:
  override def vars: Seq[Var] = m.vars ++ input.vars ++ keys.flatMap(_.vars)

  override def toString: String = s"$m += $input@$keys"
  
case class ResultMono(m: Term) extends Term:
  override def vars: Seq[Var] = m.vars
  
  override def toString: String = s"$m.result()"

trait MonoTypeOperator:
  val name: String
  
  val input: Type
  
  val output: Type
  
  val params: Seq[Type]
  
trait CountMono extends MonoTypeOperator:
  override val name : String = "CountMono"
  override val input: Type = TInt
  override val output: Type = TInt
  override val params: Seq[Type] = Seq(TInt)
  
  