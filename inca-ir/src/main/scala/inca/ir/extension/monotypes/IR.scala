package inca.ir.extension.monotypes

import inca.ir.*

trait IR extends BaseIR:
  override val name: String = "Mono-Types"

  override def language: Language = super.language + IR

  override def requires: Language = Language()

object IR extends IR { }

case class TMono(input: Type, output: Type, cols: Seq[Type]) extends Type:
  override def toString: String = s"Mono[$input, $output]@$cols"

case class MkMono(m : Var, cls: Name, args: Seq[Term], typ: TMono) extends Atom:
  override def vars: Seq[Var] = Seq(m) ++ args.flatMap(_.vars)

  override def toString: String = s"$m = new $cls()@${typ.cols}"

case class AddMono(m: Var, input: Term, keys: Seq[Term]) extends Atom:
  override def vars: Seq[Var] = Seq(m) ++ input.vars ++ keys.flatMap(_.vars)

  override def toString: String = s"$m += $input@$keys"
  
case class ResultMono(m: Var, output: Term) extends Atom:
  override def vars: Seq[Var] = Seq(m) ++ output.vars
  
  override def toString: String = s"$output = $m.result()"