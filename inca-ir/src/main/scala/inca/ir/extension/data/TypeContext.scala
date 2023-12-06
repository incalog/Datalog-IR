package inca.ir.extension.data

import inca.ir.extension.data.*
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

  def bindData(typeParams: Seq[Name], data: DataDefinition): Unit = {
    // Make sure the CaseDefinition is unique
    val newCases = data.cases.map(c => c.name -> (typeParams, data, c))
    newCases.foreach { case (k, (_, dDef, cDef)) =>
      if (caseDefs.contains(k))
        error(s"CaseDefinition $k in ${data.name} shadows previously defined case.", dDef)
    }
    caseDefs ++= newCases
  }

  def lookupConstruct(name: Name, locations: SourceLocation*): Option[(Seq[Name], DataDefinition, CaseDefinition)] = {
    val constr = caseDefs.get(name)
    if (constr.isEmpty)
      error(s"Could not find constructor $name", locations:_*)
    constr
  }
