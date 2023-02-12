package inca.frontend.constraint.desugar

trait Desugarable {
  val desugarsTo: Seq[Desugarable] = Seq()

  def trans(): DesugarTrans
}
