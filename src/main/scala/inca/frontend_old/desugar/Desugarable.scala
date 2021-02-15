package inca.frontend_old.desugar

trait Desugarable {
  val desugarsTo: Seq[Desugarable] = Seq()

  def trans(): DesugarTrans
}
