package inca.ir.extension.typeparam

import inca.ir.extension.tuple.{Project, TTuple, TupleLit}
import inca.ir.typing.{BaseIRTypechecker, Mode}
import inca.ir.util.SourceLocation
import inca.ir.{ExtensionalRelation, ModuleEntry, Name, Ref, Relation, TAny, Term, TermType, Type}

trait Typechecker extends BaseIRTypechecker:
  var typeVars: Set[Name] = Set()

  override def scopedTypeContext[T](f: => T): T = {
    val typeVarsSnap = typeVars
    try super.scopedTypeContext(f)
    finally typeVars = typeVarsSnap
  }

  override def checkType(ty: Type): Unit = ty match
    case TypeVar(name) =>
      if (!typeVars.contains(name))
        error(s"Unknown type variable $name", ty)
    case _ => super.checkType(ty)

  override def checkModuleEntry(entry: ModuleEntry): Unit = entry match
    case ParametricModuleEntry(tyParams, en) => scopedTypeContext {
      typeVars ++= tyParams
      checkModuleEntry(en)
    }
    case _ => super.checkModuleEntry(entry)

  override def inferRelationRef(ref: Ref[Relation], s: SourceLocation): Seq[Type] = ref match
    case TypeApplication(name, args) => lookupModuleEntry(name) match
      case None =>
        error(s"Unknown entry $name", s)
        Seq()
      case Some(ParametricModuleEntry(tyParams, entry)) =>
        if (tyParams.size != args.size)
          error(s"Expected ${tyParams.size} type arguments but got ${args.size}", ref)
        val colTypes = entry match
          case Relation(_, params, _) => params.map(_.ty)
          case ExtensionalRelation(_, params) => params.map(_.ty)
        val typeMap = tyParams.zip(args).toMap
        val typeSubst = new TypeSubst(typeMap)
        colTypes.map(typeSubst.visitType)
      case Some(entry) =>
        error(s"Illegal type application of $args to $entry", s)
        Seq()
    case _ => super.inferRelationRef(ref, s)
