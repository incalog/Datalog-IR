package inca.embedded2

trait Interpreter[L <: Language[_]] {
  def interp(prog: L#Program, entry: L#EntryPoint, input: L#Input): L#Value
}
