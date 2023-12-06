package inca.ir.extension.data

import inca.ir.extension.data.*
import inca.ir.extension.typeparam.ParametricModuleEntry
import inca.ir.typing.{BaseIRTypeContext, BaseIRTypechecker}
import inca.ir.util.SourceLocation
import inca.ir.{Atom, ModuleEntry, Name, Relation, TAny, Term, Type}

trait TypeContext extends BaseIRTypeContext:
  var caseDefs: Map[Name, (Seq[Name], DataDefinition, CaseDefinition)] = Map()

  override def scopedTypeContext[T](f: => T): T = {
    val caseSaved = caseDefs
    val t = super.scopedTypeContext(f)
    caseDefs = caseSaved
    t
  }

  def lookupDataDefinition(name: Name, s: SourceLocation): Option[(Seq[Name], DataDefinition)] =
    entries.get(name) match
      case Some(dd: DataDefinition) =>
        Some((Seq(), dd))
      case Some(ParametricModuleEntry(tyParams, dd: DataDefinition)) =>
        Some((tyParams, dd))
      case _ =>
        error(s"Could not find data type $name", s)
        None

  def lookupConstruct(name: Name, locations: SourceLocation*): Option[(Seq[Name], CaseDefinition)] =
    entries.get(name) match
      case Some(cd: CaseDefinition) =>
        Some((Seq(), cd))
      case Some(ParametricModuleEntry(tyParams, cd: CaseDefinition)) =>
        Some((tyParams, cd))
      case _ =>
        error(s"Could not find constructor $name", locations:_*)
        None
