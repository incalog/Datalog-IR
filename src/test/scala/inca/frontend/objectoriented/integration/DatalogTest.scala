package inca.frontend.objectoriented.integration

import inca.frontend.objectoriented.executor.ObjectExecutor.TypeCastException
import inca.frontend.objectoriented.executor.{Executor, ObjectExecutor}
import inca.frontend.objectoriented.integration.core.GenericTest
import inca.frontend.objectoriented.integration.TestDefinition._

class DatalogTest extends GenericTest {
  val executor: Executor = ObjectExecutor

  test("Base Examples") {
    performTests(baseTests)
  }

  test("Factorial Example") {
    performTest(factorialTest)
  }

  test("Fibonacci Example") {
    performTest(fibonacciTest)
  }

  test("Field Examples") {
    performTests(fieldTests)
  }

  test("Constructor ") {
    performTest(constructorTest)
  }

  test("Null Example") {
    performTest(nullTest)
  }

  test("Equals Example") {
    performTest(equalsTest)
  }

  test("InstanceOf Example") {
    performTest(instanceOfTest)
  }

  test("TypeCast Example") {
    performTest(typeCastTest)
  }

  test("TypeCast Failure Example") {
    val caught = intercept[TypeCastException] {
      performTest(typeCastFailureTest)
    }
    assert(caught.typ == "B")
    assert(caught.obj.typ == "A")
  }

  test("DynamicDispatch Example") {
    performTest(dynamicDispatchTest)
  }

  test("Object as Parameter Example") {
    performTest(objectAsParamTest)
  }

  test("Method Inheritance Example") {
    performTest(methodInheritanceTest)
  }

  test("Binary Tree Example") {
    performTest(binaryTreeSumTest)
  }

  test("Plus Example") {
    performTest(plusTest)
  }

  test("Mutability Example") {
    performTest(mutabilityTest)
  }

  test("Var Assignment Example") {
    performTest(varAssignmentTest)
  }

  test("If Constant Example") {
    performTests(ifConstantTests)
  }

  test("If Nested Example") {
    performTests(ifNestedTests)
  }

  test("If Duplicate Example") {
    performTest(ifDuplicateTest)
  }

  test("Return Example") {
    performTests(returnTests)
  }

  test("Return Implicit Example") {
    performTests(returnImplicitTests)
  }

  test("Return Unit Example") {
    performTests(returnUnitTests)
  }

  test("Super Example") {
    performTest(superTest)
  }

  test("Tuple Example") {
    performTest(tupleTest)
  }

  test("Set Simple Example") {
    performTests(simpleSetTests)
  }

  test("Set Tuple Example") {
    performTest(tupleSetTest)
  }

  test("Set Union Intersection Example") {
    performTests(unionIntersectionSetTest)
  }

  test("Set Advanced Example") {
    performTests(advancedSetTest)
  }

  test("Set Comprehension Example") {
    performTests(comprehensionSetTest)
  }

  test("Set Empty Example") {
    performTest(emptySetTest)
  }

  test("Set Recursive Example") {
    performTest(recursiveSetTest)
  }

  test("Casestudy Example") {
    performTests(caseStudyTest)
  }
}