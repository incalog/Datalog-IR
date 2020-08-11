package inca.frontend.desugar

trait Desugarable {
  val desugarsTo: Seq[Desugarable] = Seq()
  def trans(): DesugarTrans
}
