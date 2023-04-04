package inca.backend.transform.magic

import inca.backend.hints.MagicSetHints
import inca.backend.ir.Datalog
import inca.backend.transform.magic.demand.{DemandTransformation, DeriveDemandPatterns}
import inca.frontend.datalog.executor.DatalogExecutor
import org.scalatest.funsuite.AnyFunSuite

class DemandTransformationTest extends AnyFunSuite {

  val prog1: String =
    s"""module Test
       |
       |relation Edge(`Int`, `Int`)
       |Edge(1,2).
       |Edge(2,3).
       |Edge(3,4).
       |Edge(4,5).
       |Edge(3,1).
       |
       |relation Entry(`Int`)
       |Entry(X) :- Edge(1, X).
       |Entry(X) :- Edge(X, 2).
       |""".stripMargin

  def demandTrans(prog: String, main: String, adorn: Seq[Boolean]): Unit = {
    val loaded = DatalogExecutor.loadDatalog(prog)
    val patterns = loaded.compiled.ir.pats
    val entryPat = patterns.find(_.name == main).get
    val otherPats = patterns.filter(_.name != main)
    val mainEntryPat = entryPat.addHint(MagicSetHints.Main(adorn))
    val module = Datalog.Module("Test", Seq(), mainEntryPat +: otherPats, Seq())
    val derivDemandPats = DeriveDemandPatterns.transformer(loaded.compiled.dataModel).transformModule(module)
    val demandTransformed = DemandTransformation.transformer(loaded.compiled.dataModel).transformModule(derivDemandPats)
    println(demandTransformed)
  }
  test("two call-sites demand trans") {
    demandTrans(prog1, "Entry", Seq(false))
  }

  val prog2: String =
    s"""module Test
       |
       |relation A(`Int`, `Int`)
       |A(X, Y) :- B(X, Y).
       |relation B(`Int`, `Int`)
       |B(1, 2).
       |B(2, 2).
       |B(2, 1).
       |
       |relation Entry(`Int`)
       |Entry(X) :- A(1, X).
       |Entry(X) :- A(X, 2).
       |""".stripMargin
  test("two call-sites demand trans 2") {
    demandTrans(prog2, "Entry", Seq(false))
  }

  val prog3: String =
    s"""module Test
       |
       |relation A(`Int`, `Int`)
       |A(X, Y) :- B(X, Z), A(Y, Z).
       |A(X, Y) :- B(Y, X).
       |relation B(`Int`, `Int`)
       |B(1, 2).
       |B(2, 2).
       |B(2, 1).
       |
       |relation Entry(`Int`)
       |Entry(X) :- A(1, X).
       |""".stripMargin
       // Entry_f -> A_bf
       // A_bf -> B_bf, A_fb (R1)
       // A_bf -> B_fb       (R2)
       // A_fb -> B_ff, A_bb (R1)
       // A_fb -> B_bf       (R2)
       // A_bb -> B_bf, A_bb (R1)
       // A_bb -> B_bb       (R2)
  test("call-sites demand trans 2") {
    demandTrans(prog3, "Entry", Seq(false))
  }
}
