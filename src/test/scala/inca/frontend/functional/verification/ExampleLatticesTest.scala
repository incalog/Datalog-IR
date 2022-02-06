package inca.frontend.functional.verification

import org.scalatest.funsuite.AnyFunSuite
import inca.frontend.functional.verification.examples.Lattices.{const_lattice_module, sign_lattice_module}
import inca.util.Gensym

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
    val joinFuncName = aggregations.head._1
    val calledFunctions = verifier.collectCalledFunctions(verifier.functionDict(joinFuncName))
    print("\n############## Called Functions: \n")
    print(calledFunctions)
    val dataDefs = (calledFunctions :+ joinFuncName).flatMap(fName => verifier.collectUsedDataDefs(verifier.functionDict(fName)))
    print("\n############## Used DataDefs: \n")
    print(dataDefs)
    print("\n############## Translated DataDefs: \n")
    val gensym = new Gensym(Seq())
    print(dataDefs.map(d => verifier.transDataDef(d)(gensym)))
    print("\n############## Translated FunctionDefs: \n")
    val functions = calledFunctions :+ joinFuncName
    print(functions.map(f => verifier.transFunctionDef(f)))
  }
  test("test const_lattice_module"){
    val module = const_lattice_module
    val verifier = new Verifier()
    verifier.fillDicts(module)
    print("#################### DataDict: \n")
    print(verifier.dataDict)
    print("\n################## FunctionDict: \n")
    print(verifier.functionDict)
    val aggregations = verifier.collectAggregations(module)
    print("\n###################### Aggregations: \n")
    print(aggregations)
    val joinFuncName = aggregations.head._1
    val calledFunctions = verifier.collectCalledFunctions(verifier.functionDict(joinFuncName))
    print("\n############## Called Functions: \n")
    print(calledFunctions)
    val dataDefs = (calledFunctions :+ joinFuncName).flatMap(fName => verifier.collectUsedDataDefs(verifier.functionDict(fName)))
    print("\n############## Used DataDefs: \n")
    print(dataDefs)
  }
}
