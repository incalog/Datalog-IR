package inca.ir.extension.foreign

import inca.ir.extension.aggregate.AggregationOperatorUserDefined
import inca.ir.extension.mono.UserDefinedMonoDefinition
import inca.ir.{Atom, BaseIR, Language, ModuleEntry, Term, Type, Var}

object IR extends IR { }
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
  
  override def vars: Seq[Var] = args.flatMap(_.vars)

trait ForeignAggregationOperator extends AggregationOperatorUserDefined:
  val lang: ForeignLanguage
  val code: lang.Code
  
trait ForeignMonoDefinition extends UserDefinedMonoDefinition:
  val lang: ForeignLanguage
  val code: lang.Code
