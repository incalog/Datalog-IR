package inca.ir.extension.typeparam

import inca.ir.extension.tuple.{Project, TTuple, TupleLit}
import inca.ir.typing.{BaseIRTypechecker, Mode}
import inca.ir.util.SourceLocation
import inca.ir.Module
import inca.ir.{ExtensionalRelation, ModuleEntry, Name, Ref, RefByName, Relation, TAny, Term, TermType, Type}

import scala.reflect.ClassTag

trait Typechecker extends BaseIRTypechecker:
  var typeVars: Seq[Name] = Seq()

  def scopedTypeVars[T](f: => T): T =
    val typeVarsSnap = typeVars
    try f
    finally typeVars = typeVarsSnap

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
    case ParametricModuleEntry(tyParams, en) => scopedTypeVars {
      typeVars ++= tyParams
      this.checkModuleEntry(en)
    }
    case _ => super.checkModuleEntry(entry)

  override def inferRelationRef[R <: ModuleEntry](ref: Ref[R], s: SourceLocation, module: Module)(implicit tag: ClassTag[R]): Seq[Type] = ref match
    case RefByName(name) => lookupModuleEntry(name)(module) match
      case Some(ParametricModuleEntry(tyParams, _)) =>
        error(s"Expected type application of $name with ${tyParams.size} type arguments", s)
        super.inferRelationRef(ref, s, module)
      case _ => super.inferRelationRef(ref, s, module)
    case TypeApplication(name, args) => lookupModuleEntry(name)(module) match
      case None =>
        error(s"Unknown entry $name", s)
        Seq()
      case Some(ParametricModuleEntry(tyParams, entry)) =>
        ref.resolved(entry.asInstanceOf[R])
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
    case _ => super.inferRelationRef(ref, s, module)

  def matchRef[Target](ref: Ref[Target], typeParams: Seq[Name], s: SourceLocation): Map[Name, Type] = ref match
      case RefByName(name) =>
        if (typeParams.nonEmpty)
          error(s"Missing type arguments $typeParams for constructor $name", s)
        Map()
      case TypeApplication(name, tyArgs) =>
        if (typeParams.size != tyArgs.size)
          error(s"Wrong number of type arguments, got ${tyArgs.size} but expected ${typeParams.size} for constructor $name", s)
        typeParams.zip(tyArgs).toMap
