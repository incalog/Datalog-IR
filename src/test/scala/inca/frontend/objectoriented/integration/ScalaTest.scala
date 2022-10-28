package inca.frontend.objectoriented.integration

import inca.frontend.objectoriented.executor.{Executor, ScalaObjectExecutor}
import inca.frontend.objectoriented.integration.core.GenericTest
import inca.frontend.objectoriented.integration.TestDefinition._

import java.lang.reflect.InvocationTargetException

class ScalaTest extends GenericTest {
  val executor: Executor = ScalaObjectExecutor

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

  /*test("Null Example") {
    performTest(nullTest)
  }*/

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
    val caught = intercept[InvocationTargetException] {
      performTest(typeCastFailureTest)
    }
    assert(caught.getCause.isInstanceOf[ClassCastException])
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
}