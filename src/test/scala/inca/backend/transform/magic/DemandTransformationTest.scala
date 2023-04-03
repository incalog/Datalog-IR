package inca.backend.transform.magic

import inca.backend.hints.MagicSetHints
import inca.backend.ir.Datalog
import inca.backend.transform.magic.demand.{DemandTransformation, DeriveDemandPatterns}
import inca.frontend.datalog.executor.DatalogExecutor
import org.scalatest.funsuite.AnyFunSuite

class DemandTransformationTest extends AnyFunSuite {

  val prog: String =
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
       |Entry(X) :- Edge(X, 1).
       |""".stripMargin


  test("demand trans") {
    val loaded = DatalogExecutor.loadDatalog(prog)
    println(loaded.compiled.transformed)
    val patterns = loaded.compiled.ir.pats
    val entryPat = patterns.find(_.name == "Entry").get
    val otherPats = patterns.filter(_.name != "Entry")
    val mainEntryPat = entryPat.addHint(MagicSetHints.Main(Seq(false)))
    val module = Datalog.Module("Test", Seq(), mainEntryPat +: otherPats, Seq())

    val derivDemandPats = DeriveDemandPatterns.transformer(loaded.compiled.dataModel).transformModule(module)
    val demandTransformed = DemandTransformation.transformer(loaded.compiled.dataModel).transformModule(derivDemandPats)
    println(demandTransformed)
  }
}
