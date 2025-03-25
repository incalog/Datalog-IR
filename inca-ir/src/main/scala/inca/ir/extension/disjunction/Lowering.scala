package inca.ir.extension.disjunction

import inca.ir.Hint.preserveHints
import inca.ir.extension.disjunction.{Disjunction, IR}
import inca.ir.lowering.BaseLowering
import inca.ir.{Atom, BaseIR, Body}

import scala.collection.mutable.ListBuffer

trait Lowering extends BaseLowering:
  override val name: String = "Disjunction"
  override val loweredIRs: Set[BaseIR] = Set(IR)
  override val requiredIRs: Set[BaseIR] = Set()

  override def visitBody(body: Body): Seq[Body] = preserveHints(body) {
    val before: ListBuffer[Atom] = ListBuffer.empty
    var after: Seq[Atom] = body.atoms
    println(after.map(_.getClass.getSimpleName))
    while (after.nonEmpty && !after.head.isInstanceOf[Disjunction]) {
      before += after.head
      after = after.tail
    }
    if (after.isEmpty)
      super.visitBody(body)
    else {
      val disj = after.head.asInstanceOf[Disjunction]
      after = after.tail
      // If we don't have a disjunction alternative, we still want preserve everything before and after
      val alternatives =
        if (disj.alternatives.isEmpty)
          Seq(DisjunctionAlternative(Seq()))
        else
          disj.alternatives
      val bodies = alternatives.map(alt => Body(before.toList ++ alt.body.atoms ++ after))
      bodies.flatMap(visitBody)
    }
  }
