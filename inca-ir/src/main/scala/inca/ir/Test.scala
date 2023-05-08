package inca.ir
@main
def test() = {
  val module = Module(Name("Test"), TupleIR.language, Seq(IR.Relation(
    Name("rel"), Seq(IR.Param(Name("a"), TupleIR.TTuple(Seq(IR.TInt, BooleanIR.TBoolean)))),
    Seq()
  )))
  println(module)
  val lowering = new TupleToPure(module)
  println(lowering.lower)
}
