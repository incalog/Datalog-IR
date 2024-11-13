package inca.ir.extension.typeparam

import inca.ir
import inca.ir.*
import inca.ir.Hint.preserveHints
import inca.ir.visitors.BaseIRVisitor

import scala.collection.immutable.Seq

trait Visitor extends BaseIRVisitor:
  override def visitModuleEntry(moduleEntry: ModuleEntry): Seq[ModuleEntry] = moduleEntry match
    case ParametricModuleEntry(typeParams, entry) =>
      visitModuleEntry(entry).map(ParametricModuleEntry(typeParams, _))
    case _ => super.visitModuleEntry(moduleEntry)

  override def visitType(ty: Type): Type = preserveHints(ty) {
    ty match
      case TypeVar(x) => TypeVar(x)
      case _ => super.visitType(ty)
  }

  override def visitRef[Target](ref: Ref[Target]): Ref[Target] = ref match
    case TypeApplication(name, args) => TypeApplication(name, args.map(visitType))
    case _ => super.visitRef(ref)
