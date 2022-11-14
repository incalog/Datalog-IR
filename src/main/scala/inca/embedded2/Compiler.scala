package inca.embedded2

trait Compiler[SFL <: Language[_], SL <: Language[SFL], TFL <: Language[_], TL <: Language[TFL]] {
  def compile(sl: SL#Program): TL#Program
  def lower(input: SL#Input): TL#Input
  def lowerEntry(input: SL#EntryPoint): TL#EntryPoint
  def lift(value: TL#Value): SL#Value
}

object CompilerCombinationImplicit {
  // OL[IL[FL]] => OL[IL'[FL]]
  // OL[OL[FL] => OL[FL]
  // TODO How can we enforce outer type is the same?
//  implicit def flatten[
//      FL <: Language[Nothing],
//      OL <: Language[FL],
//      IL1 <: Language[IL1],
//      IL2 <: Language[FL],
//      OL2 <: Language[IL2]
//    ](implicit innerCompiler: Compiler[FL, IL1, FL, IL2]
//    ): Compiler[IL1, OL1, IL2, OL2] = new Compiler[IL1, OL1, IL2, OL2] {
//    override def compile(sl: OL1#Program): OL2#Program = n
//    override def lower(input: OL1#Input): OL2#Input = ???
//    override def lowerEntry(input: OL1#EntryPoint): OL2#EntryPoint = ???
//    override def lift(value: OL2#Value): OL1#Value = ???
//  }
  implicit def inner[
      FL <: Language[Nothing],
      IL1 <: Language[FL],
      OL1 <: Language[IL1],
      IL2 <: Language[FL],
      OL2 <: Language[IL2]
    ](implicit innerCompiler: Compiler[FL, IL1, FL, IL2]
    ): Compiler[IL1, OL1, IL2, OL2] = new Compiler[IL1, OL1, IL2, OL2] {
    override def compile(sl: OL1#Program): OL2#Program = n
    override def lower(input: OL1#Input): OL2#Input = ???
    override def lowerEntry(input: OL1#EntryPoint): OL2#EntryPoint = ???
    override def lift(value: OL2#Value): OL1#Value = ???
  }

  implicit def compose[
      FL <: Language[Nothing],
      SL <: Language[FL],
      IL <: Language[FL],
      TFL <: Language[Nothing],
      TL <: Language[FL]
    ](implicit firstCompiler: Compiler[FL, SL, FL, IL],
      secondCompiler: Compiler[FL, IL, FL, TL]
    ): Compiler[FL, SL, FL, TL] = new Compiler[FL, SL, FL, TL] {
    override def compile(prog: SL#Program): TL#Program = {
      secondCompiler.compile(firstCompiler.compile(prog))
    }
    override def lower(input: SL#Input): TL#Input = {
      secondCompiler.lower(firstCompiler.lower(input))
    }
    override def lowerEntry(input: SL#EntryPoint): TL#EntryPoint = {
      secondCompiler.lowerEntry(firstCompiler.lowerEntry(input))
    }
    override def lift(value: TL#Value): SL#Value = {
      firstCompiler.lift(secondCompiler.lift(value))
    }
  }
}

object FunCompilerImplicit {
  implicit object FunCompiler extends Compiler[Scala, Fun[Scala], Scala, ASTIR[Scala]] {
    override def compile(sl: Fun[Scala]#Program): ASTIR[Scala]#Module = ???
    override def lower(input: Fun[Scala]#Input): ASTIR[Scala]#Input = ???
    override def lowerEntry(input: Fun[Scala]#EntryPoint): ASTIR[Scala]#EntryPoint = ???
    override def lift(value: DB): Fun[Scala]#Value = ???
  }
}

object IRCompilerImplicit {
  implicit object IRCompiler extends Compiler[Scala, IR[Scala], Scala, PSystem[Scala]] {
    override def compile(sl: IR[Scala]#Module): PSystem[Scala]#Program = ???
    override def lower(input: IR[Scala]#Input): PSystem[Scala]#Input = ???
    override def lowerEntry(input: IR[Scala]#EntryPoint): PSystem[Scala]#EntryPoint = ???
    override def lift(value: PSystem[Scala]#Value): DB = ???
  }
  implicit object ASTIRCompiler extends Compiler[Scala, ASTIR[Scala], Scala, PSystem[Scala]] {
    override def compile(sl: ASTIR[Scala]#Module): PSystem[Scala]#Program = ???
    override def lower(input: ASTIR[Scala]#Input): PSystem[Scala]#Input = ???
    override def lowerEntry(input: ASTIR[Scala]#EntryPoint): PSystem[Scala]#EntryPoint = ???
    override def lift(value: PSystem[Scala]#Value): DB = ???
  }
}
