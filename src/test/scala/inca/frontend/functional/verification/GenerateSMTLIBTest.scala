package inca.frontend.functional.verification

import inca.frontend.functional.verification.examples.Lattices.{compiledIntOpsSignLattice, compiledModifiedIntervalLattice}
import inca.util.Gensym
import org.scalatest.funsuite.AnyFunSuite

class GenerateSMTLIBTest extends AnyFunSuite {

  test("why doesnt nonZeroDouble work?") {
    implicit val gensym:Gensym = new Gensym(Seq())
    val verifier = new Verifier()
    val module = compiledIntOpsSignLattice.typed
    verifier.fillDicts(module)
    val aggregations = verifier.collectAnnotated(module)
    val verificationScripts = aggregations.toSeq.map(ag => verifier.generateScript(ag._1, ag._2))
    // val a = 1
    // val b = 10
    // println("a = ", a)
    // println("b = ", b)
    // println("a/b = ", a/b)
    // println("a/10 = ", a/10)
    // println("1/10 = ", 1/10)
    print(verificationScripts.map(_.commands.mkString("")).mkString("\n"))
  }
}

