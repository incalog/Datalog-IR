package inca.frontend.functional.verification

import inca.frontend.functional.verification.examples.Lattices.compiledModifiedIntervalLattice
import inca.util.Gensym
import org.scalatest.funsuite.AnyFunSuite

class GenerateSMTLIBTest extends AnyFunSuite {

  test("why doesnt nonZeroDouble work?") {
    implicit val gensym:Gensym = new Gensym(Seq())
    val verifier = new Verifier()
    val module = compiledModifiedIntervalLattice.typed
    verifier.fillDicts(module)
    val aggregations = verifier.collectAnnotated(module)
    val verificationScripts = aggregations.toSeq.map(ag => verifier.generateScript(ag._1, ag._2))
    print(verificationScripts.map(_.commands.mkString("")).mkString("\n"))
  }
}

