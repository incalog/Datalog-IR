package inca.frontend.objectoriented.integration

import inca.backend.optimize.EliminateNonproductiveRelations
import inca.backend.transform.magic.demand.{DemandTransformation, DeriveDemandPatterns}
import inca.compiler.Compiler
import inca.frontend.objectoriented.compiler.ObjectOptions
import inca.frontend.objectoriented.datalog.{ObjectOrientedDatalog, TypeCastException}
import inca.frontend.objectoriented.integration.TestDefinition._
import inca.util.FileUtil.readFile
import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite

class DatalogTest extends AnyFunSuite {
  def options: ObjectOptions = ObjectOptions() //  Seq(DeriveDemandPatterns, DemandTransformation))

  def performTests(tests: TestDefinition[_]*): Seq[Assertion] = {
    tests.map { test =>
      val code = readFile(test.filePath)
      val module = Compiler.compileObject(code, options)

      val datalog = new ObjectOrientedDatalog(module)
      val rel = datalog.run(test.mainClass, test.mainMethod, test.input:_*)
      datalog.readAll.foreach { rel =>
        println()
        println(rel.asTable)
      }
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

  test("Plus Example") {
    performTests(plusTest)
  }

  test("Mutability Example") {
    performTests(mutabilityTest)
  }

  test("Var Assignment Example") {
    performTests(assignmentTest)
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

  test("Set Union Example") {
    performTests(unionSetTest: _*)
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
  }*/

  test("Case Class") {
    performTests(caseClassTest)
  }

  test("Case Class - Transitive Closure") {
    performTests(caseClassTransitiveClosureTest)
  }

  test("Set fold") {
    performTests(foldSetTests: _*)
  }

  /*test("Set fold - Projection") {
    performTests(foldSetProjectionTest)
  }*/

  test("CGFVisitor") {
    performTests(cfgVisitorTest)
  }

  test("While lang case study") {
    performTests(whileLangTest)
  }

  test("Binary Tree Example") {
    performTests(binaryTreeTest)
  }

  test("Tree Example") {
    performTests(treeTest)
  }

  test("DoubleLinkedList Example") {
    performTests(doubleLinkedTest)
  }

  test("Transitive closure") {
    performTests(transitiveClosureTest)
  }

  test("No Demand") {
    performTests(noDemandTest)
  }

  test("Abstract Syntax Graph") {
    performTests(abstractSyntaxGraphTest)
  }

  test("Loop") {
    performTests(loopTest)
  }

  /*test("Primitive Monotone") {
    performTests(primitiveMonotone)
  }*/

  test("Path measurement") {
    performTests(pathMeasurementTest)
  }

  test("Path with dummy measurement") {
    performTests(pathWithDummyMeasurementTest)
  }


  // Tests for generics
  test("Simple Generic Class") {
    performTests(simpleGenericClass)
  }

  test("Simple Generic Method") {
    performTests(SimpleGenericMethod)
  }

  test("Instances with Different Types") {
    performTests(instancesWithDifferentTypes)
  }

  test("Generic Linked List (nodes use same type parameter)") {
    performTests(linkedList)
  }

  test("Generic Linked List") {
    performTests(LinkedList2)
  }

  test("Class with Multiple Generic Types") {
    performTests(multipleGenericTypes)
  }

  test("Generics with Inheritance") {
    performTests(GenericsWithInheritance)
  }
  test("Inheritance With Fixed Type Parameter") {
    performTests(InheritanceWithFixedTypeParameter)
  }
  test("Inheritance With Multiple Type Parameters") {
    performTests(InheritanceWithMultipleTypeParameters)
  }
  test("Inheritance With Multiple Classes") {
    performTests(InheritanceWithMultipleClasses)
  }


  test("Custom Class as Type") {
    performTests(CustomClassAsType)
  }

  test("Unknown Type-parameter") {
    assertThrows[Exception](
      performTests(UnknownTypeParameter)
    )
//    TODO maybe create own Exception to assert error like above ?
//    val caught = intercept[Exception] {
//      performTests(UnknownTypeParameter)
//    }
//    assert(caught.??? == ???)

  }

  test("Generic Param Duplicate"){
    assertThrows[Exception](  // TODO
      performTests(GenericParamDuplicate)
    )
  }



}