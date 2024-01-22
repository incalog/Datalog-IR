package inca.ir.extension.typeparam

import inca.ir.{Name, Ref, Type}
import inca.ir.visitors.IRVisitor

class TypeSubst(val subst: Map[Name, Type]) extends IRVisitor:
  override def visitType(ty: Type): Type = ty match
    case TypeVar(name) => subst.getOrElse(name, ty)
    case _ => super.visitType(ty)
