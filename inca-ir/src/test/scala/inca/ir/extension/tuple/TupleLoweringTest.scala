package inca.ir.extension.tuple

import inca.ir.*
import inca.ir.extension.tuple
import inca.ir.extension.tuple.{IR, Project, TTuple}
import inca.ir.typing.Typechecker
import inca.util.CompilationMessage
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
      Relation("T", Seq(Param("a$0", TAny), Param("a$1", TAny)), Seq()),
      Relation(
        "R",
        Seq(
          Param("a$0", TAny),
          Param("a$1", TAny)
        ),
        Seq(
          Body(Seq(
            Call("S", Seq(
              Var("a$0"),
              Var("a$1")
            ))
          ))
        )
      ),

      Relation(
        "S",
        Seq(
          Param("a$0", TAny),
          Param("a$1", TAny),
        ),
        Seq(
          Body(Seq(
            Call("T", Seq(Var("a$0"), Var("a$1")))
          ))
        )
      )
    ))

    typechecker.checkModule(mod)

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
      Relation("T", Seq(Param("a$0", TAny), Param("a$1", TAny)), Seq()),
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
          Param("a$0", TAny),
          Param("a$1", TAny),
        ),
        Seq(
          Body(Seq(
            Call("T", Seq(Var("a$0"), Var("a$1")))
          ))
        )
      )
    ))

    typechecker.checkModule(mod)

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
      Relation("T", Seq(Param("a$0", TAny), Param("a$1", TAny)), Seq()),
      Relation(
        "R",
        Seq(
          Param("a$0", TAny),
          Param("a$1", TAny),
        ),
        Seq(
          Body(Seq(
            Call("T", Seq(Var("a$0"), Var("a$1"))),
            Eq(Var("x$0"), Var("a$0")),
            Eq(Var("x$1"), Var("a$1")),
          ))
        )
      )
    ))

    typechecker.checkModule(mod)
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
          Param("a$0", TAny),
          Param("a$2", TAny),
          Param("a$3", TAny)
        ),
        Seq(
          Body(Seq(
            Call("T", Seq(Var("t"))),
            Eq(Var("a$0"), Var("t")),
            Eq(Var("a$2"), Var("t")),
            Eq(Var("a$3"), Var("t")),
            Call("Test", Seq(
              Var("a$3"),
              Var("a$2"),
              Var("a$3")
            ))
          ))
        )
      ),

      Relation(
        "Test",
        Seq(
          Param("a$0", TAny),
          Param("a$2", TAny),
          Param("a$3", TAny),
        ),
        Seq(
          Body(Seq(
            Call("T", Seq(Var("t"))),
            Eq(Var("a$0"), Var("t")),
            Eq(Var("a$2"), Var("t")),
            Eq(Var("a$3"), Var("t")),
          ))
        )
      )
    ))

    typechecker.checkModule(mod)
    typechecker.failOnError()

    assertResult(expectedMod)(lowering.lower(mod))
  }
