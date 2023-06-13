package inca.ir.lowering

import inca.ir.{Var, *}
import inca.ir.extensions.{Disjunction, DisjunctionIR}
import org.scalatest.funsuite.AnyFunSuiteLike

import scala.collection.immutable.Seq

class DisjunctionLoweringTest extends AnyFunSuiteLike:
  val baseIR: BaseIR = new BaseIR {}
  val disjunctionIR: DisjunctionIR = new DisjunctionIR {}
  val lowering: DisjunctionLowering[DisjunctionIR, BaseIR] = new DisjunctionLowering[DisjunctionIR, BaseIR](disjunctionIR, baseIR) {}

  test("Simple lower to BaseIR") {
    val mod = Module(
      "Test",
      Language(disjunctionIR),
      Seq(
        Relation("R",
          Seq(
            Param("a", TAny)
          ),
          Seq(
            Body(Seq(
              Call("A", Seq(Var("a"))),
              Call("B", Seq(Var("a"))),
              Disjunction(
                Seq(
                  Call("C", Seq(Var("a"))),
                  Call("D", Seq(Var("a")))
                ),
                Seq(
                  Call("Y", Seq(Var("a")))
                )
              )
            ))
          )
        )
      )
    )
    val expectedMod = Module(
      "Test",
      Language(baseIR),
      Seq(
        Relation("R",
          Seq(
            Param("a", TAny)
          ),
          Seq(
            Body(Seq(
              Call("A", Seq(Var("a"))),
              Call("B", Seq(Var("a"))),
              Call("C", Seq(Var("a"))),
              Call("D", Seq(Var("a")))
            )),
            Body(Seq(
              Call("A", Seq(Var("a"))),
              Call("B", Seq(Var("a"))),
              Call("Y", Seq(Var("a")))
            ))
          )
        )
      )
    )

    assertResult(expectedMod)(lowering.lower(mod))
  }

  test("Nested lower to BaseIR") {
    val mod = Module(
      "Test",
      Language(disjunctionIR),
      Seq(
        Relation("R",
          Seq(
            Param("a", TAny)
          ),
          Seq(
            Body(Seq(
              Call("A", Seq(Var("a"))),
              Call("B", Seq(Var("a"))),
              Disjunction(
                Seq(
                  Call("C", Seq(Var("a"))),
                  Call("D", Seq(Var("a")))
                ),
                Seq(
                  Call("Y", Seq(Var("a")))
                )
              ),
              Disjunction(
                Seq(
                  Call("H", Seq(Var("a"))),
                  Call("I", Seq(Var("a")))
                ),
                Seq(
                  Disjunction(
                    Seq(
                      Call("J", Seq(Var("a"))),
                      Call("K", Seq(Var("a")))
                    ),
                    Seq(
                      Call("L", Seq(Var("a")))
                    )
                  ),
                ),
              )
            ))
          )
        )
      )
    )
    val expectedMod = Module(
      "Test",
      Language(baseIR),
      Seq(
        Relation("R",
          Seq(
            Param("a", TAny)
          ),
          Seq(
            Body(Seq(
              Call("A", Seq(Var("a"))),
              Call("B", Seq(Var("a"))),
              Call("C", Seq(Var("a"))),
              Call("D", Seq(Var("a"))),
              Call("H", Seq(Var("a"))),
              Call("I", Seq(Var("a"))),
            )),
            Body(Seq(
              Call("A", Seq(Var("a"))),
              Call("B", Seq(Var("a"))),
              Call("Y", Seq(Var("a"))),
              Call("H", Seq(Var("a"))),
              Call("I", Seq(Var("a"))),
            )),
            Body(Seq(
              Call("A", Seq(Var("a"))),
              Call("B", Seq(Var("a"))),
              Call("C", Seq(Var("a"))),
              Call("D", Seq(Var("a"))),
              Call("J", Seq(Var("a"))),
              Call("K", Seq(Var("a"))),
            )),
            Body(Seq(
              Call("A", Seq(Var("a"))),
              Call("B", Seq(Var("a"))),
              Call("Y", Seq(Var("a"))),
              Call("J", Seq(Var("a"))),
              Call("K", Seq(Var("a"))),
            )),
            Body(Seq(
              Call("A", Seq(Var("a"))),
              Call("B", Seq(Var("a"))),
              Call("C", Seq(Var("a"))),
              Call("D", Seq(Var("a"))),
              Call("L", Seq(Var("a"))),
            )),
            Body(Seq(
              Call("A", Seq(Var("a"))),
              Call("B", Seq(Var("a"))),
              Call("Y", Seq(Var("a"))),
              Call("L", Seq(Var("a"))),
            )),
          )
        )
      )
    )

    assertResult(expectedMod)(lowering.lower(mod))
  }
