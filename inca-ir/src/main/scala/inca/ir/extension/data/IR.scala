package inca.ir.extension.data

import inca.ir.*

import scala.language.implicitConversions

case class TData(name: Name) extends Type:
  override def toString: String = s"$name"

case class CaseDefinition(name: Name, args: Seq[Type]) extends Hints:
  override def toString: String = s"""$name(${args.mkString(",")})"""

case class DataDefinition(name: Name, cases: Seq[CaseDefinition]) extends ModuleEntry:
  override def toString: String = s"""data $name = ${cases.mkString(" | ")}"""

case class Construct(name: Name, args: Seq[Term]) extends Term:
  override def toString: String = s"!$name(${args.mkString(", ")})" + analysisString
  override def vars: Seq[Var] = args.flatMap(_.vars)

case class Deconstruct(t: Term, caseName: Name, args: Seq[Arg], neg: Boolean) extends Atom:
  override def toString: String =
    val ifArgs = if (args.isEmpty) "" else ", "
    val negPrefix = if (neg) "~" else ""
    s"$negPrefix?$caseName($t$ifArgs${args.mkString(", ")})" + analysisString
  override def vars: Seq[Var] = t.vars ++ args.flatMap(_.vars)

object IR extends IR { }
trait IR extends BaseIR:
  override val name: String = "Data"
  override def language: Language = super.language + IR
  override def requires: Language = Language()
