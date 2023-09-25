package inca.ir.extension.tuple

import inca.ir.*
import inca.ir.extension.tuple
import inca.ir.extension.tuple.{IR, Project, TTuple}
import Lowering.separator
import inca.ir.typing.{CompilationMessage, Typechecker}
import org.scalatest.funsuite.AnyFunSuiteLike


class TupleLoweringTest extends AnyFunSuiteLike:
  case class Failed(messages: Seq[CompilationMessage]) extends Exception(messages.mkString("\n"))

  val baseIR: BaseIR = new BaseIR {}
  val tupleIR: IR = IR
  val lowering = new Lowering {}

  test("Param lower to Base") {
    val typechecker: Typechecker = new Typechecker {}
    val mod = Module("Test", tupleIR.language, Seq(
      Relation("T", Seq(Param("a", TTuple(Seq(TAny, TAny)))), Seq()),
      Relation(
        "R",
        Seq(
          Param("a", TTuple(Seq(TAny, TAny)))
        ),
        Seq(
          Body(Seq(
            Call("S", Seq(
              Var("a")
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
          Body(Seq(Call("T", Seq(Var("a")))))
        )
      )
    ))
    val expectedMod = Module("Test", Language(baseIR), Seq(
      Relation("T", Seq(Param("a_0", TAny), Param("a_1", TAny)), Seq()),
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
          Body(Seq(
            Call("T", Seq(Var("a_0"), Var("a_1")))
          ))
        )
      )
    ))

    typechecker.typecheck(mod)

    assertResult(expectedMod)(lowering.lower(mod))
  }

  test("Term lower to Base") {
    val typechecker: Typechecker = new Typechecker {}
    val mod = Module("Test", tupleIR.language, Seq(
      Relation("T", Seq(Param("a", TTuple(Seq(TAny, TAny)))), Seq()),
      Relation(
        "R",
        Seq(
          Param("b", TAny)
        ),
        Seq(
          Body(Seq(
            Call("S", Seq(
              tuple.TupleLit(Seq(
                Var("b"),
                Var("b")
              ))
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
          Body(Seq(
            Call("T", Seq(Var("a")))
          ))
        )
      )
    ))
    val expectedMod = Module("Test", Language(baseIR), Seq(
      Relation("T", Seq(Param("a_0", TAny), Param("a_1", TAny)), Seq()),
      Relation(
        "R",
        Seq(
          Param("b", TAny)
        ),
        Seq(
          Body(Seq(
            Call("S", Seq(
              Var("b"),
              Var("b")
            )
            )
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
          Body(Seq(
            Call("T", Seq(Var("a_0"), Var("a_1")))
          ))
        )
      )
    ))

    typechecker.typecheck(mod)

    assertResult(expectedMod)(lowering.lower(mod))
  }

  test("Equality lower to Base") {
    val typechecker: Typechecker = new Typechecker {}
    val mod = Module("Test", tupleIR.language, Seq(
      Relation("T", Seq(Param("a", TTuple(Seq(TAny, TAny)))), Seq()),
      Relation(
        "R",
        Seq(
          Param("a", TTuple(Seq(TAny, TAny))),
        ),
        Seq(
          Body(Seq(
            Call("T", Seq(Var("a"))),
            Eq(Var("x"), Var("a"))
          ))
        )
      )
    ))
    val expectedMod = Module("Test", Language(baseIR), Seq(
      Relation("T", Seq(Param("a_0", TAny), Param("a_1", TAny)), Seq()),
      Relation(
        "R",
        Seq(
          Param("a" + separator + "0", TAny),
          Param("a" + separator + "1", TAny),
        ),
        Seq(
          Body(Seq(
            Call("T", Seq(Var("a_0"), Var("a_1"))),
            Eq(Var("x" + separator + "0"), Var("a" + separator + "0")),
            Eq(Var("x" + separator + "1"), Var("a" + separator + "1")),
          ))
        )
      )
    ))

    typechecker.typecheck(mod)
    typechecker.failOnError()
    println(mod)
    assertResult(expectedMod)(lowering.lower(mod))
  }

  test("Param project nested lower to Base") {
    val typechecker: Typechecker = new Typechecker {}
    val mod = Module("Test", tupleIR.language, Seq(
      Relation("T", Seq(Param("t", TAny)), Seq()),
      Relation(
        "R",
        Seq(
          Param("a", TTuple(Seq(TAny, TTuple(Seq(TAny, TAny)))))
        ),
        Seq(
          Body(Seq(
            Call("T", Seq(Var("t"))),
            Eq(Var("a"), tuple.TupleLit(Seq(Var("t"), tuple.TupleLit(Seq(Var("t"), Var("t")))))),
            Call("Test", Seq(
              tuple.TupleLit(Seq(
                Project(Project(Var("a"), 1), 1),
                Project(Var("a"), 1)
              ))
            )
            )
          ))
        )
      ),

      Relation(
        "Test",
        Seq(
          Param("a", TTuple(Seq(TAny, TTuple(Seq(TAny, TAny)))))
        ),
        Seq(
          Body(Seq(
            Call("T", Seq(Var("t"))),
            Eq(Var("a"), tuple.TupleLit(Seq(Var("t"), tuple.TupleLit(Seq(Var("t"), Var("t")))))),
          ))
        )
      )
    ))
    val expectedMod = Module("Test", Language(baseIR), Seq(
      Relation("T", Seq(Param("t", TAny)), Seq()),
      Relation(
        "R",
        Seq(
          Param("a" + separator + "0", TAny),
          Param("a" + separator + "1" + separator + "0", TAny),
          Param("a" + separator + "1" + separator + "1", TAny)
        ),
        Seq(
          Body(Seq(
            Call("T", Seq(Var("t"))),
            Eq(Var("a_0"), Var("t")),
            Eq(Var("a_1_0"), Var("t")),
            Eq(Var("a_1_1"), Var("t")),
            Call("Test", Seq(
              Var("a" + separator + "1" + separator + "1"),
              Var("a" + separator + "1" + separator + "0"),
              Var("a" + separator + "1" + separator + "1")
            ))
          ))
        )
      ),

      Relation(
        "Test",
        Seq(
          Param("a" + separator + "0", TAny),
          Param("a" + separator + "1" + separator + "0", TAny),
          Param("a" + separator + "1" + separator + "1", TAny),
        ),
        Seq(
          Body(Seq(
            Call("T", Seq(Var("t"))),
            Eq(Var("a_0"), Var("t")),
            Eq(Var("a_1_0"), Var("t")),
            Eq(Var("a_1_1"), Var("t")),
          ))
        )
      )
    ))

    typechecker.typecheck(mod)
    typechecker.failOnError()

    assertResult(expectedMod)(lowering.lower(mod))
  }
