package inca.compiler

object CompilerFlags {
  val DEBUGMODE: Boolean = true

  object DebugConfig {
    // true to include the AST
    val AST: Boolean = false
    // true to include all optimizations
    val OPTIMIZATIONS: Boolean = false
    // true to include all transformations
    val TRANSFORMATIONS: Boolean = true
  }
}