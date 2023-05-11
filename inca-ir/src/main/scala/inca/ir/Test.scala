package inca.ir
@main
def test() = {
  val boolIR = new BooleanIR {}
  println("Source IR: " + boolIR)
  val module = Module(Name("Test"), boolIR.language, Seq(boolIR.Relation(
    Name("rel"),
    Seq(boolIR.Param(Name("a"), boolIR.TBoolean)),
    Seq(
      boolIR.Body(Seq(
        boolIR.BoolAtom(
          boolIR.BoolAnd(
            boolIR.BoolNot(
              boolIR.Var(Name("a"))
            ),
            boolIR.BoolOr(
              boolIR.Var(Name("a")), boolIR.BoolFalse
            )
          )
        )
      )
    )
    )
  )))
  println(module)
  val baseIR = new IR {}
  val lowering = new BooleanLowering(module)(baseIR)
  println(lowering.lower)
}
