package inca.lang.funext.desugar

trait Desugarable {
  val desugarsTo: Set[Desugarable] = Set()
  def trans(): DesugarTrans
}
