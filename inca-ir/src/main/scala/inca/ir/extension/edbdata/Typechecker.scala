package inca.ir.extension.edbdata

import inca.ir.typing.{BaseIRTypechecker, Mode}
import inca.ir.util.SourceLocation
import inca.ir.*

trait Typechecker extends BaseIRTypechecker:

  def lookupEdbNode(name: Name, s: SourceLocation): Option[(Seq[Name], EdbNodeDefinition)] =
    entries.get(name) match
      case Some(dd: EdbNodeDefinition) =>
        Some((Seq(), dd))
      case _ =>
        error(s"Could not find data type $name", s)
        None

  def lookupEdbField(name: Name, locations: SourceLocation*): Option[(Seq[Name], EdbFieldDefinition)] =
    entries.get(name) match
      case Some(cd: EdbFieldDefinition) =>
        Some((Seq(), cd))
      case _ =>
        error(s"Could not find constructor $name", locations: _*)
        None

  override def checkModuleEntry(moduleEntry: ModuleEntry): Unit = moduleEntry match
    case EdbNodeDefinition(name, sup) => sup.foreach(lookupEdbNode(_, moduleEntry))
    case EdbFieldDefinition(node, name, ty) =>
      lookupEdbNode(node, moduleEntry)
      checkEdbType(ty, moduleEntry)
    case _ => super.checkModuleEntry(moduleEntry)

  override def checkType(ty: Type): Unit = ty match
    case ety: EdbType => checkEdbType(ety, ty)
    case _ => super.checkType(ty)

  def checkEdbType(ety: EdbType, s: SourceLocation): Unit = ety match
    case TEdbValue(ty) => // ok
    case TEdbList(ty) => checkEdbType(ty, s)
    case TEdbNode(name) => lookupEdbNode(name, s)
    case _ => throw new IllegalArgumentException(s"Cannot check unknown $ety")

  protected override def inferTermExtend(term: Term, mode: Mode): TermType = term match
    case LookupEdbType(ety) =>
      checkEdbType(ety, term)
      ety.bound
    case LookupEdbField(src, link) =>
      link match
        case Link.Parent | Link.Children =>
          inferTerm(src, Mode.Binding)
          TEdbValue(TAny).bound
        case Link.Prev | Link.Next =>
          inferTerm(src, Mode.Binding).ty match
            case TEdbList(ety) => ety.bound
            case ty =>
              error(s"Cannot lookup field $link on $ty", term)
              ty.bound
        case Link.Field(field) => inferTerm(src, Mode.Binding).ty match
          case TEdbNode(node) => lookupEdbField(edbFieldName(node, field)) match
            case Some((_, EdbFieldDefinition(_, _, target))) =>
              target.bound
            case _ =>
              // error produced by lookupEdbConstruct
              TAny.bound
        case _ =>
          val tt = inferTerm(src, Mode.Binding)
          error(s"Cannot lookup field $link on ${tt.ty}", term)
          tt
    case _ => super.inferTermExtend(term, mode)

