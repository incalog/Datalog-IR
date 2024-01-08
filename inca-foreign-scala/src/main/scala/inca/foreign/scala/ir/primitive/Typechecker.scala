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

  // this is wrong, scala and datalog types are not compatible
//  override def assertComparable(ty: Type, outside: Type, t: SourceLocation): Unit =
//    // ScalaTypes and their corresponding type are the same
//    val areEqual = (ty, outside) match
//      case (ScalaType(_), ScalaType(_)) => ty == outside
////      case (ScalaType(_), _) => ty == ScalaInca.compileType(outside)
////      case (_, ScalaType(_)) => ScalaInca.compileType(ty) == outside
//      case (_, _) => ty == outside
//    if (!areEqual)
//      error(s"$t of type $ty is not comparable to $outside", t)

  protected override def inferTermExtend(term: Term, mode: Mode): TermType = term match
    case ScalaTerm(_, ty, args, _) =>
      args.foreach(inferTerm(_, Mode.Bound))
      ty.bound
    case ScalaConstantTerm(_, ty) =>
      ty.bound
    case _ => super.inferTermExtend(term, mode)

  override def checkType(ty: Type): Unit = ty match
    case ScalaType(_) => // good
    case _ => super.checkType(ty)
