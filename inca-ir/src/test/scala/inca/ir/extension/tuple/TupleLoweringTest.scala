package inca.ir.extension.tuple

import inca.ir.*
import inca.ir.extension.tuple
import inca.ir.extension.tuple.{IR, Project, TTuple}
import inca.ir.extensions.*
import Lowering.separator
import inca.ir.typing.{CompilationMessage, Typechecker}
import org.scalatest.funsuite.AnyFunSuiteLike


class LoweringTest extends AnyFunSuiteLike:
  case class Failed(messages: Seq[CompilationMessage]) extends Exception(messages.mkString("\n"))

  val baseIR: BaseIR = new BaseIR {}
  val tupleIR: IR = IR
  val lowering = Lowering(tupleIR, baseIR)

  val typechecker: Typechecker = new Typechecker {}

  def stopIfNeeded(): Unit = {
    val errors = typechecker.getErrors
    if (errors.nonEmpty)
      throw Failed(errors)
  }

  test("Param lower to Base") {
    val mod = Module("Test", tupleIR.language, Seq(
      Relation(
        "R",
        Seq(
          Param("a", TTuple(Seq(TAny, TAny))),
          Param("b", TAny)
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
          Body(Seq())
        )
      )
    ))

    typechecker.typecheck(mod)

    assertResult(expectedMod)(lowering.lower(mod))
  }

  test("Term lower to Base") {
    val mod = Module("Test", tupleIR.language, Seq(
      Relation(
        "R",
        Seq(
          Param("a", TTuple(Seq(TAny, TAny))),
          Param("b", TAny)
        ),
        Seq(
          Body(Seq(
            Call("S", Seq(
              tuple.Tuple(Seq(
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
          Body(Seq())
        )
      )
    ))

    typechecker.typecheck(mod)

    assertResult(expectedMod)(lowering.lower(mod))
  }

  test("Equality lower to Base") {
    val mod = Module("Test", tupleIR.language, Seq(
      Relation(
        "R",
        Seq(
          Param("a", TTuple(Seq(TAny, TAny))),
        ),
        Seq(
          Body(Seq(
            Eq(Var("x"), Var("a"))
          ))
        )
      )
    ))
    val expectedMod = Module("Test", Language(baseIR), Seq(
      Relation(
        "R",
        Seq(
          Param("a" + separator + "0", TAny),
          Param("a" + separator + "1", TAny),
        ),
        Seq(
          Body(Seq(
            Eq(Var("x" + separator + "0"), Var("a" + separator + "0")),
            Eq(Var("x" + separator + "1"), Var("a" + separator + "1")),
          ))
        )
      )
    ))

    typechecker.typecheck(mod)

    assertResult(expectedMod)(lowering.lower(mod))
  }

  test("Param project nested lower to Base") {
    val mod = Module("Test", tupleIR.language, Seq(
      Relation(
        "R",
        Seq(
          Param("a", TTuple(Seq(TAny, TTuple(Seq(TAny, TAny))))),
          Param("b", TAny)
        ),
        Seq(
          Body(Seq(
            Call("Test", Seq(
              tuple.Tuple(Seq(
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
          Body(Seq())
        )
      )
    ))
    val expectedMod = Module("Test", Language(baseIR), Seq(
      Relation(
        "R",
        Seq(
          Param("a" + separator + "0", TAny),
          Param("a" + separator + "1" + separator + "0", TAny),
          Param("a" + separator + "1" + separator + "1", TAny),
          Param("b", TAny)
        ),
        Seq(
          Body(Seq(
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
          Body(Seq())
        )
      )
    ))

    typechecker.typecheck(mod)

    assertResult(expectedMod)(lowering.lower(mod))
  }
