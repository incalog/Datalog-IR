package inca.ir.extension.monotypes

import inca.ir.*
import inca.ir.extension.arithmetic.TInt
import inca.ir.extension.arithmetic.TDouble

trait IR extends BaseIR:
  override val name: String = "Mono-Types"

  override def language: Language = super.language + IR

  override def requires: Language = Language()

object IR extends IR { }

case class TMono(input: Type, output: Type, key: Type) extends Type:
  override def toString: String = s"Mono[$input, $output]@$key"

case class MkMono(mono: MonoDef, args: Seq[Term], keys: Type) extends Term:
  override def vars: Seq[Var] = args.flatMap(_.vars)
  override def toString: String = s"new $mono(${args.mkString(", ")})@$keys"

case class AddMono(m: Term, input: Term, key: Term) extends Atom:
  override def vars: Seq[Var] = m.vars ++ input.vars ++ key.vars
  override def toString: String = s"$m += $input@$key"

case class ResultMono(m: Term) extends Term:
  override def vars: Seq[Var] = m.vars
  override def toString: String = s"$m.result()"

trait MonoDef:
  def typecheck(constructorArgs: Seq[Term]): Either[String, (Type, Type)]

enum ArithmeticMono extends MonoDef:
  case CountMono
  case MaxMono

  override def typecheck(constructorArgs: Seq[Term]): Either[String, (Type, Type)] = this match
    case CountMono =>
      if (constructorArgs.nonEmpty)
        Left(s"$CountMono does not take arguments")
      else
        Right((TInt, TInt)) // should be parametric in input type
    case MaxMono =>
      if (constructorArgs.nonEmpty)
        Left(s"$CountMono does not take arguments")
      else
        Right((TInt, TInt))
