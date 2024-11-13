package inca.ir.extension.foreign

import inca.ir.extension.aggregate.AggregationOperatorUserDefined
import inca.ir.extension.mono.UserDefinedMonoDefinition
import inca.ir.{Atom, BaseIR, Language, ModuleEntry, Name, Term, Type, Var}
import inca.ir.visitors.BaseIRVisitor

object IR extends IR {}

trait IR extends BaseIR:
  override val name: String = "Foreign"

  override def language: Language = super.language + IR

  override def requires: Language = Language()

trait ForeignLanguage:
  type Code

trait ForeignType extends Type:
  val lang: ForeignLanguage
  val code: lang.Code

trait ForeignAtom extends Atom:
  val lang: ForeignLanguage
  val code: lang.Code

trait ForeignModuleEntry extends ModuleEntry:
  val lang: ForeignLanguage
  val code: lang.Code

trait ForeignTerm(args: Seq[Term]) extends Term:
  val lang: ForeignLanguage
  val code: lang.Code

  def inTypes: Seq[Type]

  def outTypes: Seq[Type]

  def visitArgs(f: Term => Seq[Term]): Seq[Term]

  override def vars: Seq[Var] = args.flatMap(_.vars)

case class ConvertIRForeign(term: Term, irType: Type, foreignType: Type) extends Term:
  override def toString: String = s"$term as $foreignType"

  override def vars: Seq[Var] = term.vars

case class ConvertForeignIR(term: Term, foreignType: Type, irType: Type) extends Term:
  override def toString: String = s"$term as $irType"

  override def vars: Seq[Var] = term.vars

trait ForeignAggregationOperator extends AggregationOperatorUserDefined:
  val name: Name
  val lang: ForeignLanguage
  val initCode: lang.Code
  val addCode: lang.Code

trait ForeignMonoDefinition extends UserDefinedMonoDefinition:
  val lang: ForeignLanguage
  val initCode: lang.Code
  val addCode: lang.Code
  val resultCode: lang.Code
