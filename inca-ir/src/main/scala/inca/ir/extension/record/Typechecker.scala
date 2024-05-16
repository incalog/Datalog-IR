package inca.ir.extension.record

import inca.ir.{Arg, Atom, ModuleEntry, Name, Ref, RefByName, TAny, Term, TermArg, TermType, Type, Var, WildcardArg}
import inca.ir.extension.record.{Deconstruct, FieldDefinition, RecordDefinition, RecordLit}
import inca.ir.typing.{BaseIRTypechecker, Mode}
import inca.ir.util.SourceLocation
import inca.ir.extension.typeparam.{ParametricModuleEntry, TypeApplication, TypeSubst, TypeVar}
import inca.ir.typing.Mode
import inca.ir.extension.typeparam
import inca.ir.typing.Mode.Bound

// TODO: Support type substitutions
trait Typechecker extends BaseIRTypechecker with typeparam.Typechecker:

  lazy val fieldDefs: Seq[FieldDefinition] = entries.values.collect {
    case fd: FieldDefinition => fd
  }.toSeq

  private def lookupRecordDefinition(ref: Ref[RecordDefinition], s: SourceLocation): Option[(Seq[Name], RecordDefinition)] =
    entries.get(ref.name) match
      case Some(rd: RecordDefinition) =>
        ref.resolved(rd)
        Some((Seq(), rd))
      case Some(ParametricModuleEntry(tyParams, rd: RecordDefinition)) =>
        ref.resolved(rd)
        Some((tyParams, rd))
      case _ =>
        error(s"Could not find record type ${ref.name}", s)
        None

  private def lookupFieldDefinition(ref: Ref[FieldDefinition], s: SourceLocation, recordName: Name): Option[(Seq[Name], FieldDefinition)] =
    entries.get(Name(s"${recordName.name}.${ref.name}")) match
      case Some(fd: FieldDefinition) =>
        ref.resolved(fd)
        Some((Seq(), fd))
      case Some(ParametricModuleEntry(tyParams, fd: FieldDefinition)) =>
        ref.resolved(fd)
        Some((tyParams, fd))
      case _ =>
        error(s"Could not find field definition ${ref.name}", s)
        None

  override def checkModuleEntry(moduleEntry: ModuleEntry): Unit = moduleEntry match
    case dd: RecordDefinition => // nothing to check
    case FieldDefinition(name, ty, record) =>
      checkType(ty) 
      checkType(record) 
    case _ => super.checkModuleEntry(moduleEntry)

  private def getAllFieldDefinitions(recName: Name): Seq[FieldDefinition] =
    fieldDefs.filter(_.record.ref.name.name == recName.name)

  protected override def inferTermExtend(term: Term, mode: Mode): TermType = term match
    case RecordLit(ref, fields) => lookupRecordDefinition(ref, term) match
      case None =>
        error(s"Unknown record definition $ref", term)
        TAny.bound
      case Some((typeParams, cd@RecordDefinition(recName))) =>
        val fieldDefsOfRecord = getAllFieldDefinitions(recName)
        if (fieldDefsOfRecord.size != fields.size)
          error(s"Wrong number of field entries $fields for fields $fieldDefsOfRecord of Record Definition of ${ref.name}")
        fields.foreach{ case (fieldRef, t) =>
          val expectedType = lookupFieldDefinition(fieldRef, term, ref.name) match {
            case Some((_, fd@FieldDefinition(name, ty, record))) =>
              addTypeDependency(fd)
              checkType(ty)
              checkType(record)
              ty
            case None => error(s"Not found FieldDefinition $fieldRef")
              TAny
          }
          checkTerm(t, expectedType, Mode.Bound)
        }
        TermType(TRecord(ref.name), Mode.Bound)

    case FieldLookup(record, field) =>
      record match {
        case other => super.inferTermExtend(record, mode).ty match {
          case TRecord(ref) => lookupFieldDefinition(field, term, ref.name) match {
            case None =>
              error(s"No field definition ${ref.name}.$field")
              TermType(TAny, mode)
            case Some((_, fd@FieldDefinition(fieldName, ty, recordType))) =>
              checkTerm(record, recordType, mode)
              TermType(ty, Mode.Bound)
            }
          case _ =>
            error(s"expected RecordLit but got $other")
            TermType(TAny, mode)

        }
      }
    case _ => super.inferTermExtend(term, mode)

  def checkDeconstructRecord(matcheeType: Type, recordRef: Ref[RecordDefinition], s: SourceLocation): Unit = matcheeType match {
    case TRecord(matcheeRef) =>
      if (matcheeRef.name != recordRef.name) 
        error(s"Constructor ${recordRef.name} does not belong to matchee's data type ${matcheeRef.name}", matcheeRef)
    case ty =>
      error(s"Expected data type but got $matcheeType", s)
  }
  
  override def checkAtom(atom: Atom, mode: Mode): Unit = atom match
    case Deconstruct(record, ref, fields, neg) => lookupRecordDefinition(ref, atom) match {
      case None =>
        error(s"Unknown record definition $ref", atom)
      case Some((typeParams, cd@RecordDefinition(recName))) =>
        addTypeDependency(cd)
        inferTerm(record, Mode.Bound).ty match {
          case ty@TRecord(ref) =>
            val recFields = getAllFieldDefinitions(ref.name)
            if (recFields.size != fields.size)
              error(s"Expected ${fields.size} arguments but got: ${recFields.size}", atom)

            val namesTypes = recFields.map(fDef => (fDef.fieldName, fDef.ty)).toMap

            checkDeconstructRecord(ty, ref, record)

            val argsTypes: Seq[(Arg, Type)] = fields.map { case (nameArg, arg) =>
              val ty = namesTypes.get(nameArg) match
                case Some(tty) => tty
                case _ =>
                  error(s"Field not found $nameArg", atom)
                  TAny
              arg -> ty
            }
            argsTypes.map {
              case (TermArg(v), ty) =>
                checkTerm(v, ty, mode)
              case (wildcard@WildcardArg(), ty) => wildcard.typed(ty.collapsed, force = true)
            }
          case other => error(s"Expected RecordLit but got $other")
        }
    }
    case _ => super.checkAtom(atom, mode)

  override def checkType(ty: Type): Unit = ty match
    case TRecord(ref) =>
      val (tyParams, entry) = lookupRecordDefinition(ref, ty) match
        case None => (Seq(), null)
        case Some((tyParams, dd)) =>
          addTypeDependency(dd)
          (tyParams, dd)
      ref match {
        case RefByName(name) =>
          if (tyParams.nonEmpty)
            error(s"Expected type application of $name with ${tyParams.size} type arguments", ty)
        case TypeApplication(name, args) =>
          if (tyParams.size != args.size)
            error(s"Type application has ${args.size} arguments, but $name requires ${tyParams.size} arguments: $entry", ty)
          args.foreach(checkType)
      }
    case _ => super.checkType(ty)