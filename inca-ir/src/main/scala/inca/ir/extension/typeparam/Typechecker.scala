package inca.ir.extension.typeparam

import inca.ir.typing.{BaseIRTypechecker}
import inca.ir.util.SourceLocation
import inca.ir.{ExtensionalRelation, ModuleEntry, Name, Ref, RefByName, Relation, Type}

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

  override def inferRelationRef[R <: ModuleEntry](ref: Ref[R], isExtensional: Boolean, s: SourceLocation*): Seq[Type] = ref match
    case RefByName(name) => lookupModuleEntry(name) match
      case Some(ParametricModuleEntry(tyParams, _)) =>
        error(s"Expected type application of $name with ${tyParams.size} type arguments", s:_*)
        super.inferRelationRef(ref, isExtensional, s:_*)
      case _ => super.inferRelationRef(ref, isExtensional, s:_*)
    case TypeApplication(name, args) => lookupModuleEntry(name) match
      case None =>
        error(s"Unknown entry $name", s:_*)
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
        error(s"Illegal type application of $args to $entry", s:_*)
        Seq()
    case _ => super.inferRelationRef(ref, isExtensional, s:_*)

  def matchRef[Target](ref: Ref[Target], typeParams: Seq[Name], s: SourceLocation): Map[Name, Type] = ref match
      case RefByName(name) =>
        if (typeParams.nonEmpty)
          error(s"Missing type arguments $typeParams for constructor $name", s)
        Map()
      case TypeApplication(name, tyArgs) =>
        if (typeParams.size != tyArgs.size)
          error(s"Wrong number of type arguments, got ${tyArgs.size} but expected ${typeParams.size} for constructor $name", s)
        typeParams.zip(tyArgs).toMap
