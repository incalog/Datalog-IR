package inca.ir.extension.record

import inca.ir
import inca.ir.Hint.preserveHints
import inca.ir.extension.block.Block
import inca.ir.{Atom, BaseIR, ModuleEntry, Name, Ref, RefByName, Term, TermArg, TermType, Type, Var, string2name}
import inca.ir.lowering.BaseLowering
import inca.ir.extension.data
import inca.ir.extension.data.{CaseDefinition, Construct, DataDefinition, TData}
import inca.ir.extension.block

trait Lowering extends BaseLowering:
  override val name: String = "Record"
  override val loweredIRs: Set[BaseIR] = Set(IR)
  override val requiredIRs: Set[BaseIR] = Set(data.IR, block.IR)

  private var fieldDefsByRecordName: Map[Name, Seq[FieldDefinition]] = Map()
  private var indexForFieldInRecord: Map[Name, Map[Name, Int]] = Map()

  override def visitType(ty: Type): Type = ty match
    case TRecord(RefByName(name)) => TData(RefByName(s"$name$$Record"))
    case _ => super.visitType(ty)

  override def visitModule(module: ir.Module): ir.Module = preserveHints(module) {

    val recordDefsByName = module.contents.collect {
      case rDef: RecordDefinition => rDef.name -> rDef
    }.toMap

    fieldDefsByRecordName = module.contents.collect {
      case fDef: FieldDefinition => fDef.record.ref.name -> fDef
    }.groupBy(_._1).view.mapValues(_.map(_._2)).toMap

    indexForFieldInRecord = recordDefsByName.map { (name, _) =>
      name -> fieldDefsByRecordName(name).map(_.fieldName).zipWithIndex.toMap
    }

    val recordAdtDef = recordDefsByName.map { (name, recordDef) =>
      DataDefinition(s"$name$$Record")
    }.toSeq
    val recordCaseDefs = recordDefsByName.map { (name, rDef) =>
      val fields = fieldDefsByRecordName(name).map(f => f.fieldName -> f.ty)
      CaseDefinition(name, fields.map((_, ty) => visitType(ty)), TData(s"$name$$Record"))
    }

    val ir.Module(name, lang, contents) = super.visitModule(module)
    ir.Module(name, lang, recordAdtDef ++ recordCaseDefs ++ contents)
  }

  override def visitModuleEntry(moduleEntry: ModuleEntry): Seq[ModuleEntry] = moduleEntry match
    case RecordDefinition(name) => Seq()
    case FieldDefinition(_, _, _) => Seq()
    case _ => super.visitModuleEntry(moduleEntry)

  override def visitTerm(term: Term): Seq[Term] = term match {
    case RecordLit(ref, fields) =>
      val argTerms = fields.sortBy { (fieldRef, term) =>
        indexForFieldInRecord(ref.name)(fieldRef.name)
      }.map(_._2)
      Seq(Construct(ref.name, argTerms.flatMap(visitTerm)))

    case FieldLookup(record, fieldRef) =>
      val recordName = record.typ match
        case Some(TermType(TRecord(ref), mode)) => ref.name
        case ty => throw new IllegalStateException(s"FieldLookup expected TRecord, but got $ty")

      visitTerm(record).map { recordTerm =>
        val fieldDefs = fieldDefsByRecordName(recordName)
        val newVars = fieldDefs.map(_ => Var(gensym.freshName("field")))
        val fieldIdx = indexForFieldInRecord(recordName)(fieldRef.name)

        Block(
          visitTerm(record).map { r => data.Deconstruct(r, recordName, newVars.map(_.arg)) },
          newVars(fieldIdx)
        )
      }
    case _ =>  super.visitTerm(term)
  }

  override def visitAtom(atom: Atom): Seq[Atom] = atom match {
    case Deconstruct(record, recordRef, fields, neg) =>
      val newTerms = visitTerm(record)
      val newFields = fields.sortBy((fieldName, _) => indexForFieldInRecord(recordRef.name)(fieldName)).map(_._2)
      newTerms.map { newTerm =>
        data.Deconstruct(newTerm, recordRef.name, newFields.flatMap(visitArg), neg)
      }
    case _ => super.visitAtom(atom)
  }