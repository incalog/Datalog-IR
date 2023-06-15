package inca.ir.lowering

import inca.ir.*
import inca.ir.extension.disjunction.{Disjunction, IR}
import inca.ir.extensions.*
import inca.ir.lowering.TupleLowering.separator
import inca.ir.typing.{CompilationMessage, Typechecker}
import org.scalatest.funsuite.AnyFunSuiteLike


class CombinedLoweringTest extends AnyFunSuiteLike:
  case class Failed(messages: Seq[CompilationMessage]) extends Exception(messages.mkString("\n"))

  trait TupleDisjunctionIR extends TupleIR, IR:
    override val name: String = "TupleDisjunction"

  val baseIR: BaseIR = new BaseIR {}
  val tupleIR: TupleIR = new TupleIR {}
  val disjunctionIR: IR = new IR {}
  val tupleDisjunctionIR: TupleDisjunctionIR = new TupleDisjunctionIR {}

  val typechecker: Typechecker = new Typechecker {}

  def stopIfNeeded(): Unit = {
    val errors = typechecker.getErrors
    if (errors.nonEmpty)
      throw Failed(errors)
  }

  test("Disjunction Tuple to Base") {
    /*
    // We could use mixin inheritance to lower two IRs at once
    trait TupleDisjunctionLowering[S <: DisjunctionIR & TupleIR, T <: BaseIR] extends Lowering[S, T]
      with TupleLowering[S, T]
      with DisjunctionLowering[S, T]

    val tupleDisjunctionIR: TupleDisjunctionIR = new TupleDisjunctionIR {}
    val lowering: TupleDisjunctionLowering[TupleDisjunctionIR, BaseIR] = new TupleDisjunctionLowering[TupleDisjunctionIR, BaseIR] {
      override def src: TupleDisjunctionIR = tupleDisjunctionIR
      override def trg: BaseIR = baseIR
    }*/
    val tupleLowering: TupleLowering[TupleDisjunctionIR, IR] = new TupleLowering[TupleDisjunctionIR, IR] {
      override def src: TupleDisjunctionIR = tupleDisjunctionIR
      override def trg: IR = disjunctionIR
    }
    val disjunctionLowering: DisjunctionLowering[IR, BaseIR] = new DisjunctionLowering[IR, BaseIR] {
      override def src: IR = disjunctionIR
      override def trg: BaseIR = baseIR
    }

    val mod = Module("Test", tupleDisjunctionIR.language, Seq(
      Relation(
        "R",
        Seq(
          Param("a", TTuple(Seq(TAny, TAny))),
          Param("b", TAny)
        ),
        Seq(
          Body(Seq(
            Disjunction(
              Seq(
                Call("S", Seq(Var("a")))
              ),
              Seq(
                Call("T", Seq(Var("a")))
              )
            )
          ))
        )
      ),

      Relation(
        "S",
        Seq(
          Param("a", TTuple(Seq(TAny, TAny)))
        ),
        Seq(
          Body(Seq())
        )
      ),

      Relation(
        "T",
        Seq(
          Param("a", TTuple(Seq(TAny, TAny)))
        ),
        Seq(
          Body(Seq())
        )
      )
    ))
    val expectedMod = Module("Test", Language(baseIR), Seq(
      Relation(
        "R",
        Seq(
          Param("a" + separator + "0", TAny),
          Param("a" + separator + "1", TAny),
          Param("b", TAny)
        ),
        Seq(
          Body(Seq(
            Call("S", Seq(
              Var("a" + separator + "0"),
              Var("a" + separator + "1")
            ))
          )),
          Body(Seq(
            Call("T", Seq(
              Var("a" + separator + "0"),
              Var("a" + separator + "1")
            ))
          ))
        )
      ),

      Relation(
        "S",
        Seq(
          Param("a" + separator + "0", TAny),
          Param("a" + separator + "1", TAny),
        ),
        Seq(
          Body(Seq())
        )
      ),

      Relation(
        "T",
        Seq(
          Param("a" + separator + "0", TAny),
          Param("a" + separator + "1", TAny),
        ),
        Seq(
          Body(Seq())
        )
      )
    ))

    typechecker.typecheck(mod)

    assertResult(expectedMod)(
      disjunctionLowering.lower(
        tupleLowering.lower(mod)
      )
    )
  }

  test("Disjunction Tuple to Disjunction") {
    // Tuple lowering to DisjunctionIR
    val lowering: TupleLowering[TupleDisjunctionIR, IR] = new TupleLowering[TupleDisjunctionIR, IR] {
      override def src: TupleDisjunctionIR = tupleDisjunctionIR
      override def trg: IR = disjunctionIR
    }

    val mod = Module("Test", tupleDisjunctionIR.language, Seq(
      Relation(
        "R",
        Seq(
          Param("a", TTuple(Seq(TAny, TAny))),
          Param("b", TAny)
        ),
        Seq(
          Body(Seq(
            Disjunction(
              Seq(
                Call("S", Seq(Var("a")))
              ),
              Seq(
                Call("T", Seq(Var("a")))
              )
            )
          ))
        )
      ),

      Relation(
        "S",
        Seq(
          Param("a", TTuple(Seq(TAny, TAny)))
        ),
        Seq(
          Body(Seq())
        )
      ),

      Relation(
        "T",
        Seq(
          Param("a", TTuple(Seq(TAny, TAny)))
        ),
        Seq(
          Body(Seq())
        )
      )
    ))
    val expectedMod = Module("Test", disjunctionIR.language, Seq(
      Relation(
        "R",
        Seq(
          Param("a" + separator + "0", TAny),
          Param("a" + separator + "1", TAny),
          Param("b", TAny)
        ),
        Seq(
          Body(Seq(
            Disjunction(
              Seq(
                Call("S", Seq(
                  Var("a" + separator + "0"),
                  Var("a" + separator + "1")
                ))
              ),
              Seq(
                Call("T", Seq(
                  Var("a" + separator + "0"),
                  Var("a" + separator + "1")
                ))
              )
            )
          )),
        )
      ),

      Relation(
        "S",
        Seq(
          Param("a" + separator + "0", TAny),
          Param("a" + separator + "1", TAny),
        ),
        Seq(
          Body(Seq())
        )
      ),

      Relation(
        "T",
        Seq(
          Param("a" + separator + "0", TAny),
          Param("a" + separator + "1", TAny),
        ),
        Seq(
          Body(Seq())
        )
      )
    ))

    typechecker.typecheck(mod)

    assertResult(expectedMod)(lowering.lower(mod))
  }


