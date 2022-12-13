package inca.compiler

object CompilerFlags {
  val DEBUGMODE: Boolean = true

  object DebugConfig {
    // true to include the AST
    val AST_STEPS: Boolean = false
    // true to include all optimizations
    val OPTIMIZATION_STEPS: Boolean = false
    // true to include all transformations
    val TRANSFORMATION_STEPS: Boolean = false
  }
}