package inca.ir.extension.tuple

import inca.ir.*
import inca.ir.extension.arithmetic.{IntNum, TInt, IR as arithIR}
import inca.ir.extension.string.{StringLit, TString, IR as stringIR}
import inca.ir.extension.tuple
import inca.ir.extension.tuple.{IR, Project, TTuple}
import inca.ir.typing.{IRTypechecker, Typechecker}
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

    typechecker.checkProgram(Seq(mod))

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

    typechecker.checkProgram(Seq(mod))

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

    typechecker.checkProgram(Seq(mod))
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

    typechecker.checkProgram(Seq(mod))
    typechecker.failOnError()

    assertResult(expectedMod)(lowering.lower(mod))
  }

  test("Nested tuples"){
    val mod = Module("Test", tupleIR.language + arithIR + stringIR, Seq(
      Relation("main",
        Seq(
          Param("a", TTuple(Seq(TTuple(Seq(TInt, TString)), TInt))),
          Param("b", TTuple(Seq(TInt, TString))),
          Param("c", TInt),
          Param("d", TString),
          Param("e", TInt)
        ),
        Seq(Body(Seq(
          Eq(Var("a"), TupleLit(Seq(TupleLit(Seq(IntNum(1), StringLit("1"))), IntNum(1)))),
          Eq(Var("b"), Project(Var("a"), 0)),
          Eq(Var("c"), Project(Var("b"), 0)),
          Eq(Var("d"), Project(Var("b"), 1)),
          Eq(Var("e"), Project(Var("a"), 1)),
        )))
      )
    ))

    val typechecker1 = IRTypechecker()
    typechecker1.checkProgram(Seq(mod))
    typechecker1.failOnError()
    val typechecker2 = IRTypechecker()
    typechecker2.checkProgram(Seq(lowering.lower(mod)))
    typechecker2.failOnError()
  }
