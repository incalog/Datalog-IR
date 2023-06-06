package inca.ir

import inca.ir.extensions.*
import inca.ir.lowering.{DisjunctionLowering, TupleLowering}
import inca.ir.typing.{CompilationMessage, Typechecker}

import scala.collection.immutable.Seq

val typechecker = new Typechecker {}


case class Failed(messages: Seq[CompilationMessage]) extends Exception(messages.mkString("\n"))

def stopIfNeeded(): Unit = {
  val errors = typechecker.getErrors
  if (errors.nonEmpty)
    throw Failed(errors)
}

@main
def test() = {
  val mods1 = lowerTupleIR()
  val mods2 = lowerDisjunctionIR()
  //val mods3 = lowerCombinedIR()

  Seq(mods1, mods2).flatten.foreach { m =>
    println()
    println(m)
    println()
  }
}

def lowerDisjunctionIR(): Seq[Module] = {
  val disjunctionIR = new DisjunctionIR {}

  val mod = Module(Name("Test"), Language(disjunctionIR), Seq(Relation(
    Name("R"),
    Seq(Param(Name("a"), TAny)),
    Seq(
      Body(Seq(
        Call(Name("A"), Seq(Var(Name("a")))),
        Call(Name("B"), Seq(Var(Name("a")))),
        Disjunction(
          Seq(
            Call(Name("C"), Seq(Var(Name("a")))),
            Call(Name("D"), Seq(Var(Name("a"))))
          ),
          Seq(
            Call(Name("Y"), Seq(Var(Name("a"))))
          ),
        ),
        Disjunction(
          Seq(
            Call(Name("H"), Seq(Var(Name("a")))),
            Call(Name("I"), Seq(Var(Name("a"))))
          ),
          Seq(
            Disjunction(
              Seq(
                Call(Name("J"), Seq(Var(Name("a")))),
                Call(Name("K"), Seq(Var(Name("a"))))
              ),
              Seq(
                Call(Name("L"), Seq(Var(Name("a"))))
              ),
            ),
          ),
        )
      ))
    )
  )))

  val baseIR = new BaseIR {}
  val lowering = new DisjunctionLowering[DisjunctionIR, BaseIR](disjunctionIR, baseIR) {}
  Seq(mod, lowering.lower(mod))
}

def lowerTupleIR(): Seq[Module] = {
  val tupleIR = new TupleIR {}

  val mod = Module(Name("Test"), Language(tupleIR), Seq(

    Relation(
      Name("R"),
      Seq(Param(Name("a"), TTuple(Seq(TAny, TTuple(Seq(TAny, TAny))))), Param(Name("b"), TAny)),
      Seq(
        Body(Seq(
            Call(Name("Test"), Seq(Tuple(Seq(Var("b"), Project(Var("a"), 1)))))
        ))
      )
    ),

    Relation(
      Name("Test"),
      Seq(Param(Name("a"), TTuple(Seq(TAny, TTuple(Seq(TAny, TAny)))))),
      Seq(
        Body(Seq(
        ))
      )
    )
  ))


  typechecker.typecheck(mod)
  stopIfNeeded()

  val baseIR = new BaseIR {}
  val lowering = new TupleLowering[TupleIR, BaseIR](tupleIR, baseIR) {}
  Seq(mod, lowering.lower(mod))
  Seq(mod)
}


/*def lowerBoolIR(): Seq[extensions.Module] = {
  val boolIR = new BooleanIR {}

  val mod = extensions.Module(Name("Test"), Language(boolIR), Seq(boolIR.Relation(
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
    override def module: extensions.Module = mod

  Seq(mod, lowering.lower)
}


trait BooleanDisjunctionIR extends BooleanIR, DisjunctionIR:
  override val name: String = "BooleanDisjunction"
  //override def language: Language = super.language + new BooleanIR {} + new DisjunctionIR {}
  override def requires: Language = Language()

trait BooleanToDisjunctionLowering[S <: DisjunctionIR & BooleanIR, T <: DisjunctionIR] extends BooleanLowering[S, T] with PreserveDisjunction[S, T]

trait DisjunctionToBooleanLowering[S <: BooleanIR & DisjunctionIR, T <: BooleanIR] extends DisjunctionLowering[S, T] with PreserveBoolean[S, T]

def lowerCombinedIR(): Seq[extensions.Module] = {
  val baseIR = new IR {}
  val boolIR = new BooleanIR {}
  val disjunctionIR = new DisjunctionIR {}
  val combinedIR = new BooleanDisjunctionIR {}

  val language = Language(disjunctionIR, boolIR)

  val mod = extensions.Module(Name("Test"), language, Seq(combinedIR.Relation(
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
    override def module: extensions.Module = mod

  val loweredMod1 = loweringBool1.lower

  val loweringDisjunction1 = new DisjunctionLowering[DisjunctionIR, IR]:
    override val src: DisjunctionIR = disjunctionIR
    override val trg: IR = baseIR
    override def module: extensions.Module = loweredMod1


  val loweringDisjunction2 = new DisjunctionToBooleanLowering[BooleanDisjunctionIR, BooleanIR]:
    override val src: BooleanDisjunctionIR = combinedIR
    override val trg: BooleanIR = boolIR
    override def module: extensions.Module = mod

  val loweredMod2 = loweringDisjunction2.lower

  val loweringBool2 = new BooleanLowering[BooleanIR, IR]:
    override val src: BooleanIR = boolIR
    override val trg: IR = baseIR
    override def module: extensions.Module = loweredMod2

  Seq(mod, loweredMod1, loweringDisjunction1.lower, loweredMod2, loweringBool2.lower)
}*/