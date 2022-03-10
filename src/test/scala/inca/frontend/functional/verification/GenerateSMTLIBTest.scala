package inca.frontend.functional.verification

import inca.examples.functional.ControlDataFlow
import inca.frontend.functional.Collect
import inca.frontend.functional.core.{Call, DataDef, Expression, FunctionDef, Name, TData, Type, Var}
import inca.frontend.functional.parser.Parser
import org.scalatest.funsuite.AnyFunSuite
import smtlib.trees.Commands.PropLiteral
import smtlib.trees.Terms.SSymbol
import inca.frontend.functional.verification.Verifier
import inca.frontend.functional.verification.examples.Aggregations.compiledAdditionModule
import inca.frontend.functional.verification.examples.Lattices.{compiledConstLattice, compiledSignLattice, compiledSignValLattice, signValLattice}
import inca.util.Gensym
import smtlib.Interpreter
import smtlib.interpreters.Z3Interpreter

class GenerateSMTLIBTest extends AnyFunSuite {
  test("run z3") {
    implicit val z3Interp = Z3Interpreter.buildDefault
  }
  test("generate data type") {
    print(PropLiteral(SSymbol("x"), true))
    assertResult(1)(1)
  }
  test("what does the functional AS look like") {
    print(Parser.parse(ControlDataFlow.IntervalModule))
  }
  test("print addition module") {
    print(compiledAdditionModule.typed)
  }
  test("print sign lattice") {
    print(compiledSignValLattice.typed)
  }
  test("print constant Propagation lattice") {
    print(compiledConstLattice.typed)
  }
  test("collect called functions") {
    val module = Parser.parse(ControlDataFlow.IntervalModule)
    val verifier = new Verifier()
    verifier.fillDicts(module)
    print(module.content.flatMap {
      case DataDef(_, _, _, _) => Seq()
      case f:FunctionDef => verifier.collectCalledFunctions(f)
    }.distinct )
    print(verifier.collectCalledFunctions(module.content(2).asInstanceOf[FunctionDef]))
    print(verifier.collectCalledFunctions(module.content(4).asInstanceOf[FunctionDef]))
  }
  test("collect used datatypes") {
    val module = Parser.parse(ControlDataFlow.IntervalModule)
    val verifier = new Verifier()
    verifier.fillDicts(module)
    print(verifier.collectUsedDataDefs(module.content(2).asInstanceOf[FunctionDef]))
    print(verifier.collectUsedDataDefs(module.content(4).asInstanceOf[FunctionDef]))
  }
  test("collect aggregations") {
    val module = Parser.parse(ControlDataFlow.IntervalModule)
    val verifier = new Verifier()
    print(verifier.collectAggregations(module))
  }


  test("test signVal_lattice_module Schritt für Schritt") {
    val module = compiledSignValLattice.typed
    val verifier = new Verifier()
    verifier.fillDicts(module)
    print("#################### DataDict: \n")
    print(verifier.dataDict)
    print("\n################## FunctionDict: \n")
    print(verifier.functionDict)
    val aggregations = verifier.collectAggregations(module)
    print("\n###################### Aggregations: \n")
    print(aggregations)
    val joinFuncName = "join"
    val calledFunctions = verifier.collectCalledFunctions(verifier.functionDict(joinFuncName))
    print("\n############## Called Functions: \n")
    print(calledFunctions)
    val dataDefs = (calledFunctions :+ joinFuncName).flatMap(fName => verifier.collectUsedDataDefs(verifier.functionDict(fName))).distinct
    print("\n############## Used DataDefs: \n")
    print(dataDefs)
    print("\n############## Translated DataDefs: \n")
    val gensym = new Gensym(Seq())
    print(dataDefs.map(d => verifier.transDataDef(d)(gensym)))
    print("\n############## Translated FunctionDefs: \n")
    val functions = calledFunctions :+ joinFuncName
    print(functions.map(f => verifier.transFunctionDef(f)))
    print("\n\n############## Output generate with assoc and comm: \n")
    val verificationScript = verifier.generateScript(joinFuncName, aggregations(joinFuncName))
    print(verificationScript.commands.mkString(""))
    print("\n\n############## Output verificationScript feedback: \n")
    implicit val z3Interp = Z3Interpreter.buildDefault
    print(Interpreter.execute(verificationScript))
  }

  test("test sign_lattice_module Schritt für Schritt") {
    val module = compiledSignLattice.typed
    val verifier = new Verifier()
    verifier.fillDicts(module)
    print("#################### DataDict: \n")
    print(verifier.dataDict)
    print("\n################## FunctionDict: \n")
    print(verifier.functionDict)
    val aggregations = verifier.collectAggregations(module)
    print("\n###################### Aggregations: \n")
    print(aggregations)
    val joinFuncName = "join"
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
    print("\n\n############## Output generate with assoc and comm: \n")
    val verificationScript = verifier.generateScript(joinFuncName, aggregations(joinFuncName))
    print(verificationScript.commands.mkString(""))
    print("\n\n############## Output verificationScript feedback: \n")
    implicit val z3Interp = Z3Interpreter.buildDefault
    print(Interpreter.execute(verificationScript))
  }

  test("test const_lattice_module") {
    val module = compiledConstLattice.typed
    val verifier = new Verifier()
    verifier.fillDicts(module)
    print("#################### DataDict: \n")
    print(verifier.dataDict)
    print("\n################## FunctionDict: \n")
    print(verifier.functionDict)
    val aggregations = verifier.collectAggregations(module)
    print("\n###################### Aggregations: \n")
    print(aggregations)
    val joinFuncName = "join"
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
    print("\n\n############## Output generate with assoc and comm: \n")
    val verificationScript = verifier.generateScript(joinFuncName, aggregations(joinFuncName))
    print(verificationScript.commands.mkString(""))
    print("\n\n############## Output verificationScript feedback: \n")
    implicit val z3Interp = Z3Interpreter.buildDefault
    print(Interpreter.execute(verificationScript))
  }
}

