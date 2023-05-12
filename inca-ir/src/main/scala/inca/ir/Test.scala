package inca.ir
@main
def test() = {
  val mods1 = lowerBoolIR()
  val mods2 = lowerDisjunctionIR()
  val mods3 = lowerCombinedIR()

  (mods1 ++ mods2 ++ mods3).foreach { m =>
    println()
    println(m)
    println()
  }
}

def lowerDisjunctionIR(): Seq[Module] = {
  val disjunctionIR = new DisjunctionIR {}

  val module = Module(Name("Test"), Language(disjunctionIR), Seq(disjunctionIR.Relation(
    Name("rel"),
    Seq(disjunctionIR.Param(Name("a"), disjunctionIR.TInt)),
    Seq(
      disjunctionIR.Body(Seq(
        disjunctionIR.Call(Name("a"), Seq(disjunctionIR.Num(4))),
        disjunctionIR.Call(Name("b"), Seq(disjunctionIR.Var(Name("c")))),
        disjunctionIR.Disjunction(
          Seq(
            disjunctionIR.Call(Name("d"), Seq(disjunctionIR.Num(5))),
            disjunctionIR.Call(Name("e"), Seq(disjunctionIR.Var(Name("f"))))
          ),
          Seq(
            disjunctionIR.Call(Name("g"), Seq(disjunctionIR.Num(6))),
            disjunctionIR.Call(Name("h"), Seq(disjunctionIR.Var(Name("i"))))
          ),
        ),
        disjunctionIR.Disjunction(
          Seq(
            disjunctionIR.Call(Name("j"), Seq(disjunctionIR.Num(7))),
            disjunctionIR.Call(Name("k"), Seq(disjunctionIR.Var(Name("f"))))
          ),
          Seq(
            disjunctionIR.Call(Name("l"), Seq(disjunctionIR.Num(8))),
            disjunctionIR.Call(Name("m"), Seq(disjunctionIR.Var(Name("n"))))
          ),
        )
      ))
    )
  )))

  val baseIR = new IR {}
  val lowering = new DisjunctionLowering(module)(disjunctionIR, baseIR).lower
  Seq(module, lowering)
}


def lowerBoolIR(): Seq[Module] = {
  val boolIR = new BooleanIR {}

  val module = Module(Name("Test"), Language(boolIR), Seq(boolIR.Relation(
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

  val baseIR = new IR {}
  val lowering = new BooleanLowering(module)(boolIR, baseIR).lower
  Seq(module, lowering)
}

trait BooleanDisjunctionIR extends BooleanIR, DisjunctionIR:
  override val name: String = "BooleanDisjunction"
  override def language: Language = super.language + new BooleanDisjunctionIR {}
  override def requires: Language = Language()

def lowerCombinedIR(): Seq[Module] = {
  val disjunctionIR = new DisjunctionIR {}
  val boolIR = new BooleanIR {}
  val combinedIR = new BooleanDisjunctionIR {}

  val language = Language(disjunctionIR, boolIR)

  val module = Module(Name("Test"), language, Seq(combinedIR.Relation(
    Name("rel"),
    Seq(combinedIR.Param(Name("a"), combinedIR.TInt)),
    Seq(
      combinedIR.Body(Seq(
        combinedIR.Call(Name("a"), Seq(combinedIR.Num(4))),
        combinedIR.Call(Name("b"), Seq(combinedIR.Var(Name("c")))),
        combinedIR.Disjunction(
          Seq(
            combinedIR.Call(Name("d"), Seq(combinedIR.Num(5))),
            combinedIR.Call(Name("e"), Seq(combinedIR.Var(Name("f"))))
          ),
          Seq(
            combinedIR.Call(Name("g"), Seq(combinedIR.Num(6))),
            combinedIR.Call(Name("h"), Seq(combinedIR.Var(Name("i"))))
          ),
        ),
        combinedIR.Disjunction(
          Seq(
            combinedIR.Call(Name("j"), Seq(combinedIR.Num(7))),
            combinedIR.Call(Name("k"), Seq(combinedIR.Var(Name("f"))))
          ),
          Seq(
            combinedIR.BoolAtom(
              combinedIR.BoolAnd(
                combinedIR.BoolNot(
                  combinedIR.Var(Name("a"))
                ),
                combinedIR.BoolOr(
                  combinedIR.Var(Name("a")), combinedIR.BoolFalse
                )
              )
            )
          ),
        )
      ))
    )
  )))

  val baseIR = new IR {}
  val loweringBool = new BooleanLowering(module)(combinedIR, disjunctionIR).lower
  //val loweringDisjunction = new DisjunctionLowering(module)(disjunctionIR, baseIR).lower

  Seq(module, loweringBool)//, loweringDisjunction)
}