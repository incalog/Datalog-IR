package inca.ir.extension.demand

import inca.ir.*
import inca.ir.Hint.preserveHints
import inca.ir.visitors.BaseIRVisitor

import scala.collection.immutable.Seq

trait Visitor extends BaseIRVisitor:

  override def visitType(ty: Type): Type = ty match
    case TDemand(ty) => preserveHints(ty)(TDemand(visitType(ty)))
    case _ => super.visitType(ty)
