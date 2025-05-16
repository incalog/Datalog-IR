package inca.foreign.scala.ir.primitive

import inca.foreign.scala.ir.primitive.*
import inca.ir.extension.data.DataDefinition
import inca.ir.extension.foreign.ForeignModuleEntry
import inca.ir.typing.{BaseIRTypechecker, Mode}
import inca.ir.util.SourceLocation
import inca.ir.{Atom, ExtensionalRelation, ModuleEntry, RefByName, Relation, TAny, Term, TermType, Type, string2name}

trait Typechecker extends BaseIRTypechecker:
  override def checkModuleEntry(moduleEntry: ModuleEntry): Unit = moduleEntry match
    case ScalaDefnModuleEntry(_, _) => // nothing
    case _ => super.checkModuleEntry(moduleEntry)

  protected override def inferTermExtend(term: Term, mode: Mode): TermType = term match
    case ScalaTerm(_, ty, args, _) =>
      args.foreach(inferTerm(_, Mode.Bound))
      ty.bound
    case ScalaConstantTerm(_, ty) =>
      ty.bound
    case _ => super.inferTermExtend(term, mode)

  override def checkTermExtend(term: Term, expected: Type, mode: Mode): Mode =
    // Implicit conversion elimination during type checking for Scala types and IR types
    // This is required, because e.g. Substring uses the arithmetic IR. That is Substring
    // contains partially lowered integer.
    val action = startContextTransaction()
    withErrors(super.checkTermExtend(term, expected, mode)) match
      case (m, Nil) =>
        action.commit()
        m
      case (tt, errsInfer) =>
        action.abort()
        super.checkTermExtend(term, ScalaInca.compileType(expected), mode)

  override def checkType(ty: Type): Unit = ty match
    case ScalaType(_) => // good
    case _ => super.checkType(ty)
