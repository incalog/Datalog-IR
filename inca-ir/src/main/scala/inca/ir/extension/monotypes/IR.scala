package inca.ir.extension.monotypes

import inca.ir.*
import inca.ir.extension.arithmetic.TInt
import inca.ir.extension.arithmetic.TDouble

trait IR extends BaseIR:
  override val name: String = "Mono-Types"

  override def language: Language = super.language + IR

  override def requires: Language = Language()

object IR extends IR { }

case class TMono(input: Type, output: Type, keys: Seq[Type]) extends Type:
  override def toString: String = s"Mono[$input, $output]@$keys"

case class MkMono(cls: MonoTypeOperator, args: Seq[Term], monoTyp: TMono) extends Term:
  override def vars: Seq[Var] = args.flatMap(_.vars)

  override def toString: String = s"new $cls()@${monoTyp.keys}"

case class AddMono(m: Term, input: Term, keys: Seq[Term]) extends Atom:
  override def vars: Seq[Var] = m.vars ++ input.vars ++ keys.flatMap(_.vars)

  override def toString: String = s"$m += $input@$keys"
  
case class ResultMono(m: Term) extends Term:
  override def vars: Seq[Var] = m.vars
  
  override def toString: String = s"$m.result()"

trait MonoTypeOperator:
  def typecheck(in: Seq[Type]): Either[String, Type]

enum ArithmeticMono extends MonoTypeOperator:
  case CountMono
  override def typecheck(in: Seq[Type]): Either[String, Type] = this match
    case CountMono =>
      if (in == Seq(TInt))
        Right(TInt)
      else
        Left(s"Cannot compute $this for values of type $in")
