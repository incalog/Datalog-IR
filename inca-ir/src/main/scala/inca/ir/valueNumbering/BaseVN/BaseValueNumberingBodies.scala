package inca.ir.valueNumbering.BaseVN

import inca.ir
import inca.ir.*

import inca.ir.valueNumbering.VNTables.*



trait BaseValueNumberingBodies extends BaseValueNumberingAtoms {

  private var VNs_Bodies = ValueIds[Body]()

  override def visitRelation(relation: Relation): Seq[Relation] = {
    val newRelation = super.visitRelation(relation)
    VNs_Bodies = ValueIds[Body]()
    newRelation
  }

  override def visitBody(body: Body): Seq[Body] = {
    val newBody = super.visitBody(body)
    return valueNumberBodies(newBody)
  }


  protected def normalizeBody(body: Body): Seq[Body] = Seq(body) // TODO

  private def valueNumberBodies(bodySeq: Seq[Body]): Seq[Body] = {
    if (bodySeq.isEmpty) return bodySeq
    val body = normalizeBody(bodySeq.head) match {
      case h :: _ => h
      case _ => return Seq()
    }

    if (VNs_Bodies.contains(body)) {
      return Seq()
    }
    else {
      val vn = VNs_Bodies.getValNumOf(body)
      return Seq(body)
    }
  }

}
