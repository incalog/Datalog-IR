package inca.embedded2

object Operations {
  def solve[FL <: Language[Nothing], L <: Language[FL]](
      prog: L#Program,
      entry: L#EntryPoint,
      input: L#Input
    )(implicit compiler: Compiler[FL, L, FL, PSystem[FL]],
      interpreter: Interpreter[FL]
    ): L#Value = {
    val psystem = compiler.compile(prog)
    val loweredEntry = compiler.lowerEntry(entry)
    val loweredInput = compiler.lower(input)
    val psystemValue = solvePSystem(psystem, loweredEntry, loweredInput)(interpreter)
    compiler.lift(psystemValue)
  }

  // foreign language FL has no other foreign language embedded
  private def solvePSystem[FL <: Language[Nothing]](
      prog: PSystem[FL]#Program,
      entry: PSystem[FL]#EntryPoint,
      input: PSystem[FL]#Input
    )(implicit interpreter: Interpreter[FL]
    ): PSystem[FL]#Value = {
    // if L is scala then we do nothing special
    // if L is not scala we use the interpreter (written in scala) to implement evaluator expressions
    null.asInstanceOf[PSystem[FL]#Value]
  }

//  def incSolve[L <: Language[_]](
//      lang: L
//    )(
//      prog: lang.Program,
//      entry: lang.EntryPoint,
//      input: lang.Input
//    ): lang.Change => lang.Value = {}

}
