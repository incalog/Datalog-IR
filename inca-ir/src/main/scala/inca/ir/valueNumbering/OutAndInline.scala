package inca.ir.valueNumbering

/** determines if terms should be outlined when using Value Numbering with propagating defining terms */
trait Outline {
  // TODO actual implementation (currently no good heuristics when term gets "better")
  def shouldOutline: Boolean = false
}

/** determines if terms should be inlined when using Value Numbering with propagating defining terms */
trait Inline {
  // TODO actual implementation (currently no good heuristics when term gets "better")
  def shouldInline: Boolean = false
}