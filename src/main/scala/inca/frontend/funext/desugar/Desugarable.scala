package inca.frontend.funext.desugar

trait Desugarable {
  val desugarsTo: Seq[Desugarable] = Seq()
  def trans(): DesugarTrans
}
