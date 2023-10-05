package inca.ir.lowering

import inca.ir.*
import inca.ir.extension.*
import inca.ir.extension.disjunction.Disjunction
import inca.ir.extension.tuple.TTuple
import inca.ir.extension.tuple.Lowering.separator
import inca.ir.typing.Typechecker
import inca.util.CompilationMessage
import org.scalatest.funsuite.AnyFunSuiteLike


class CombinedLoweringTest extends AnyFunSuiteLike:
  case class Failed(messages: Seq[CompilationMessage]) extends Exception(messages.mkString("\n"))

  trait TupleDisjunctionIR extends tuple.IR, disjunction.IR:
    override val name: String = "TupleDisjunction"

  val baseIR: BaseIR = new BaseIR {}
  val tupleIR = tuple.IR
  val disjunctionIR = disjunction.IR
  val tupleDisjunctionIR: TupleDisjunctionIR = new TupleDisjunctionIR {}

  val typechecker: Typechecker = new Typechecker {}

  test("Disjunction Tuple to Base") {
    val tupleLowering = new tuple.Lowering {}
    val disjunctionLowering = new disjunction.Lowering {}

    val mod = Module("Test", tupleDisjunctionIR.language, Seq(
      Relation(
        "R",
        Seq(
          Param("a", TTuple(Seq(TAny, TAny)))
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
          Body(Seq(Call("S", Seq(Var("a")))))
        )
      ),

      Relation(
        "T",
        Seq(
          Param("a", TTuple(Seq(TAny, TAny)))
        ),
        Seq(
          Body(Seq(Call("T", Seq(Var("a")))))
        )
      )
    ))
    val expectedMod = Module("Test", Language(baseIR), Seq(
      Relation(
        "R",
        Seq(
          Param("a" + separator + "0", TAny),
          Param("a" + separator + "1", TAny)
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
          Body(Seq(Call("S", Seq(Var("a_0"), Var("a_1")))))
        )
      ),

      Relation(
        "T",
        Seq(
          Param("a" + separator + "0", TAny),
          Param("a" + separator + "1", TAny),
        ),
        Seq(
          Body(Seq(Call("T", Seq(Var("a_0"), Var("a_1")))))
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
    val lowering = new tuple.Lowering {}

    val mod = Module("Test", tupleDisjunctionIR.language, Seq(
      Relation(
        "R",
        Seq(
          Param("a", TTuple(Seq(TAny, TAny)))
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
          Body(Seq(Call("S", Seq(Var("a")))))
        )
      ),

      Relation(
        "T",
        Seq(
          Param("a", TTuple(Seq(TAny, TAny)))
        ),
        Seq(
          Body(Seq(Call("T", Seq(Var("a")))))
        )
      )
    ))
    val expectedMod = Module("Test", disjunctionIR.language, Seq(
      Relation(
        "R",
        Seq(
          Param("a" + separator + "0", TAny),
          Param("a" + separator + "1", TAny)
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
          Body(Seq(Call("S", Seq(Var("a_0"), Var("a_1")))))
        )
      ),

      Relation(
        "T",
        Seq(
          Param("a" + separator + "0", TAny),
          Param("a" + separator + "1", TAny),
        ),
        Seq(
          Body(Seq(Call("T", Seq(Var("a_0"), Var("a_1")))))
        )
      )
    ))

    typechecker.typecheck(mod)

    assertResult(expectedMod)(lowering.lower(mod))
  }


