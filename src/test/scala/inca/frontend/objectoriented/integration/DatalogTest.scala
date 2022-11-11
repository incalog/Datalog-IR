package inca.frontend.objectoriented.integration

import inca.compiler.Compiler
import inca.frontend.objectoriented.compiler.ObjectOptions
import inca.frontend.objectoriented.runner.{ObjectOrientedRunnerFactory, TypeCastException}
import inca.frontend.objectoriented.integration.TestDefinition._
import inca.util.FileUtil.readFile
import org.scalatest.{Assertion, Ignore}
import org.scalatest.funsuite.AnyFunSuite

class DatalogTest extends AnyFunSuite {
  def options: ObjectOptions = ObjectOptions()

  def performTests(tests: TestDefinition[_]*): Seq[Assertion] = {
    tests.map { test =>
      val code = readFile(test.filePath)
      val module = Compiler.compileObject(code, options)
      val runnerFactory = new ObjectOrientedRunnerFactory(module)
      val runner = runnerFactory.runner(test.mainClass, test.mainMethod)
      val rel = runner.run(test.input:_*)
      println("Expected: ", test.expectedRelation)
      println("Actual: ", rel)
      assert(rel == test.expectedRelation)
    }
  }

  test("Base Examples") {
    performTests(baseTests: _*)
  }

  test("Factorial Example") {
    performTests(factorialTest)
  }

  test("Fibonacci Example") {
    performTests(fibonacciTest)
  }

  test("Field Examples") {
    performTests(fieldTests: _*)
  }

  test("Constructor ") {
    performTests(constructorTest)
  }

  test("Null Example") {
    performTests(nullTest)
  }

  test("Equals Example") {
    performTests(equalsTest)
  }

  test("InstanceOf Example") {
    performTests(instanceOfTest)
  }

  test("TypeCast Example") {
    performTests(typeCastTest)
  }

  test("TypeCast Failure Example") {
    val caught = intercept[TypeCastException] {
      performTests(typeCastFailureTest)
    }
    assert(caught.typ == "B")
    assert(caught.obj.typ == "A")
  }

  test("DynamicDispatch Example") {
    performTests(dynamicDispatchTest)
  }

  test("Object as Parameter Example") {
    performTests(objectAsParamTest)
  }

  test("Method Inheritance Example") {
    performTests(methodInheritanceTest)
  }

  test("Binary Tree Example") {
    performTests(binaryTreeSumTest)
  }

  test("Plus Example") {
    performTests(plusTest)
  }

  test("Mutability Example") {
    performTests(mutabilityTest)
  }

  test("Var Assignment Example") {
    performTests(varAssignmentTest)
  }

  test("If Constant Example") {
    performTests(ifConstantTests: _*)
  }

  test("If Nested Example") {
    performTests(ifNestedTests: _*)
  }

  test("If Duplicate Example") {
    performTests(ifDuplicateTest)
  }

  test("Return Example") {
    performTests(returnTests: _*)
  }

  test("Return Implicit Example") {
    performTests(returnImplicitTests: _*)
  }

  test("Return Unit Example") {
    performTests(returnUnitTests: _*)
  }

  test("Super Example") {
    performTests(superTest)
  }

  test("Tuple Example") {
    performTests(tupleTest)
  }

  test("Set Simple Example") {
    performTests(simpleSetTests: _*)
  }

  test("Set Tuple Example") {
    performTests(tupleSetTest)
  }

  test("Set Union Intersection Example") {
    performTests(unionIntersectionSetTest: _*)
  }

  test("Set Advanced Example") {
    performTests(advancedSetTest: _*)
  }

  test("Set Comprehension Example") {
    performTests(comprehensionSetTest: _*)
  }

  test("Set Recursive Example") {
    performTests(recursiveSetTest)
  }

  /*test("Set Empty Example") {
    performTests(emptySetTest)
  }

  test("Casestudy Example") {
    performTests(caseStudyTest: _*)
  }*/
}