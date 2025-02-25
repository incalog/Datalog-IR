package inca.foreign.ddlog.ir.primitive

import inca.ir.*
import inca.ir.extension.foreign.*
import inca.foreign.ddlog.ir.*

object DDLogInca extends ForeignLanguage:
  override val name: Name = Name("DDLog")
  type Code = String

case class DDLogAggregationOperator(name: Name, ty: Type, initCode: String, addCode: String) extends ForeignAggregationOperator:
  override val lang: DDLogInca.type = DDLogInca
  override def resultType: Type = ty
  def typecheck(in: Seq[Type]): Option[String] = None

case class DDLogDefnModuleEntry(name: Name, code: String) extends ForeignModuleEntry:
  override def withName(name: String): ModuleEntry = this.copy(name = Name(name))
  override val lang: DDLogInca.type = DDLogInca
  override def toString: String = code

trait IR extends BaseIR:
  override val name: String = "PrimitiveDDLog"
  override def language: Language = super.language + new IR {}
  // We can not lower this IR any further
  override def requires: Language = Language(new IR {})
object IR extends IR { }