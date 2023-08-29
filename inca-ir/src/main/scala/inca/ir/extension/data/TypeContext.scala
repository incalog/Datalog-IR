package inca.ir.extension.data

import inca.ir.extension.data.*
import inca.ir.typing.{BaseIRTypeContext, BaseIRTypechecker}
import inca.ir.util.SourceLocation
import inca.ir.{Atom, ModuleEntry, Name, Relation, TAny, Term, Type}

trait TypeContext extends BaseIRTypeContext:
  var caseDefs: Map[Name, (DataDefinition, CaseDefinition)] = Map()

  override def scopedTypeContext[T](f: => T): T = {
    val modulesSaved = modules
    val entriesSaved = entries
    val varsSaved = vars
    val caseSaved = caseDefs
    val t = f
    vars = varsSaved
    entries = entriesSaved
    modules = modulesSaved
    caseDefs = caseSaved
    t
  }

  def bindData(data: DataDefinition): Unit = {
    // Make sure the CaseDefinition is unique
    val newCases = data.cases.map(c => c.name -> (data, c))
    newCases.foreach { case (k, (dDef, cDef)) =>
      if (caseDefs.contains(k))
        error(s"CaseDefinition $k in ${data.name} shadows previously defined case.", dDef)
    }
    caseDefs ++= newCases
  }

  def lookupConstruct(name: Name, locations: SourceLocation*): Option[(DataDefinition, CaseDefinition)] = {
    val constr = caseDefs.get(name)
    if (constr.isEmpty)
      error(s"Could not find constructor $name", locations:_*)
    constr
  }
