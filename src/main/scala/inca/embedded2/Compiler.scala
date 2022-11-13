package inca.embedded2

trait Compiler[SL <: Language[_], TL <: Language[_]] {
  def compile(sl: SL#Program): TL#Program
  def lower(input: SL#Input): TL#Input
  def lowerEntry(input: SL#EntryPoint): TL#EntryPoint
  def lift(value: TL#Value): SL#Value
}
