package inca.frontend.functional.verification

import org.scalatest.funsuite.AnyFunSuite
import inca.frontend.functional.verification.Verifier
import inca.frontend.functional.verification.examples.Lattices.sign_lattice_module

class ExampleLatticesTest extends AnyFunSuite {
  test("test sign_lattice_module"){
    val module = sign_lattice_module
    val verifier = new Verifier()
    verifier.fillDicts(module)
    print("#################### DataDict: \n")
    print(verifier.dataDict)
    print("\n################## FunctionDict: \n")
    print(verifier.functionDict)
    val aggregations = verifier.collectAggregations(module)
    print("\n###################### Aggregations: \n")
    print(aggregations)
    val joinFunc = verifier.functionDict(aggregations.toSeq.map(ag => ag._1).head)

  }
}
