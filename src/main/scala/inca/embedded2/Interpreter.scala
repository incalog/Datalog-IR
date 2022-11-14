package inca.embedded2

trait Interpreter[L <: Language[_]] {
  def interp(prog: L#Program, entry: L#EntryPoint, input: L#Input): L#Value
}

object InterpreterImplicit {
  implicit object ScalaInterpreter extends Interpreter[Scala] {
    override def interp(prog: Scala#Program, entry: Scala#EntryPoint, input: Scala#Input): Any = ???
  }
}
