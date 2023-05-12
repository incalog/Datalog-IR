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

  val mod = Module(Name("Test"), Language(disjunctionIR), Seq(disjunctionIR.Relation(
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
  val lowering = new DisjunctionLowering[DisjunctionIR, IR]:
    override val src: DisjunctionIR = disjunctionIR
    override val trg: IR = baseIR
    override def module: Module = mod

  Seq(mod, lowering.lower)
}


def lowerBoolIR(): Seq[Module] = {
  val boolIR = new BooleanIR {}

  val mod = Module(Name("Test"), Language(boolIR), Seq(boolIR.Relation(
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
  val lowering = new BooleanLowering[BooleanIR, IR]:
    override val src: BooleanIR = boolIR
    override val trg: IR = baseIR
    override def module: Module = mod

  Seq(mod, lowering.lower)
}


trait BooleanDisjunctionIR extends BooleanIR, DisjunctionIR:
  override val name: String = "BooleanDisjunction"
  override def language: Language = super.language + new BooleanIR {} + new DisjunctionIR {}
  override def requires: Language = Language()


def lowerCombinedIR(): Seq[Module] = {
  val baseIR = new IR {}
  val boolIR = new BooleanIR {}
  val disjunctionIR = new DisjunctionIR {}
  val combinedIR = new BooleanDisjunctionIR {}

  val language = Language(disjunctionIR, boolIR)

  val mod = Module(Name("Test"), language, Seq(combinedIR.Relation(
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

  val loweringBool1 = new BooleanToDisjunctionLowering[BooleanDisjunctionIR, DisjunctionIR]:
    override val src: BooleanDisjunctionIR = combinedIR
    override val trg: DisjunctionIR = disjunctionIR
    override def module: Module = mod

  val loweredMod1 = loweringBool1.lower

  val loweringDisjunction1 = new DisjunctionLowering[DisjunctionIR, IR]:
    override val src: DisjunctionIR = disjunctionIR
    override val trg: IR = baseIR
    override def module: Module = loweredMod1


  val loweringDisjunction2 = new DisjunctionToBooleanLowering[BooleanDisjunctionIR, BooleanIR]:
    override val src: BooleanDisjunctionIR = combinedIR
    override val trg: BooleanIR = boolIR
    override def module: Module = mod

  val loweredMod2 = loweringDisjunction2.lower

  val loweringBool2 = new BooleanLowering[BooleanIR, IR]:
    override val src: BooleanIR = boolIR
    override val trg: IR = baseIR
    override def module: Module = loweredMod2

  Seq(mod, loweredMod1, loweringDisjunction1.lower, loweredMod2, loweringBool2.lower)
}