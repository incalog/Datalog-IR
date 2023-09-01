package inca.frontend.objectoriented.typecheck

import inca.frontend.objectoriented.core.Module
import inca.frontend.objectoriented.parser.Parser
import inca.frontend.objectoriented.typechecker.Typechecker
import inca.util.FileUtil
import org.scalatest.funsuite.AnyFunSuite

class TypecheckerTest extends AnyFunSuite {

  def newTypechecker(): Typechecker = new Typechecker {}

  def checkModule(mod: String): Unit = {
    checkModule(Parser.parse(mod))
  }

  def checkModule(mod: Module): Unit = {
    val checker = newTypechecker()
    checker.typecheck(mod)
    checker.getErrors.foreach(println)
    assert(checker.getErrors.isEmpty, s"Found type errors ${checker.getErrors}")
  }

  def checkModuleErrors(mod: String): Unit = {
    checkModuleErrors(Parser.parse(mod))
  }

  def checkModuleErrors(mod: Module): Unit = {
    val checker = newTypechecker()
    checker.typecheck(mod)
    checker.getErrors.foreach(println)
    assert(checker.getErrors.nonEmpty, s"Expected type errors, but none found")
  }


  test("Base 1") {
    val code = FileUtil.readFile("objectoriented/unittests/base/Base1.oinca")
    checkModule(code)
  }

  test("Base 2") {
    val code = FileUtil.readFile("objectoriented/unittests/base/Base2.oinca")
    checkModule(code)
  }

  test("Base 3") {
    val code = FileUtil.readFile("objectoriented/unittests/base/Base3.oinca")
    checkModule(code)
  }

  test("Fact") {
    val code = FileUtil.readFile("objectoriented/unittests/Fact.oinca")
    checkModule(code)
  }

  test("Fib") {
    val code = FileUtil.readFile("objectoriented/unittests/Fib.oinca")
    checkModule(code)
  }

  test("Plus") {
    val code = FileUtil.readFile("objectoriented/unittests/Plus.oinca")
    checkModule(code)
  }

  test("Tuple") {
    val code = FileUtil.readFile("objectoriented/unittests/Tuple.oinca")
    checkModule(code)
  }

  test("BinaryTree") {
    val code = FileUtil.readFile("objectoriented/graphs/BinaryTree.oinca")
    checkModule(code)
  }

  /*---------------*/
  /* Test Generics */
  /*---------------*/

  test("Simple Generic Class") {
    val code = FileUtil.readFile("objectoriented/generics/basic/GenericClass.oinca")
    checkModule(code)
  }

  test("Addition of Generic Param Failure") {
    val code = FileUtil.readFile("objectoriented/generics/basic/GenericClassGenericParamAdditionFailure.oinca")
    checkModuleErrors(code)
  }

  test("Generic Class Instances with different types") {
    val code = FileUtil.readFile("objectoriented/generics/basic/GenericClassInstancesWithDifferentTypes.oinca")
    checkModule(code)
  }

  test("Generic Class inner Shadowing") {
    val code = FileUtil.readFile("objectoriented/generics/basic/GenericClassMethodInnerShadowing.oinca")
    checkModule(code)
  }

  test("Generic Class with wrong type annotation") {
    val code = FileUtil.readFile("objectoriented/generics/basic/GenericClassTypeAnnotationFailure.oinca")
    checkModuleErrors(code)
  }

  test("Generic Class with multiple generic Types") {
    val code = FileUtil.readFile("objectoriented/generics/basic/GenericClassWithMultipleTypes.oinca")
    checkModule(code)
  }

  test("Simple generic method") {
    val code = FileUtil.readFile("objectoriented/generics/basic/GenericMethod.oinca")
    checkModule(code)
  }

  test("Generic method with wrong Out-Type annotation") {
    val code = FileUtil.readFile("objectoriented/generics/basic/GenericMethodOutTypeFailure.oinca")
    checkModuleErrors(code)
  }

  test("Generic method with wrong given Generic Type") {
    val code = FileUtil.readFile("objectoriented/generics/basic/GenericMethodTypeFailure.oinca")
    checkModuleErrors(code)
  }

  test("Generic method with wrong given Generic Type (multiple Types)") {
    val code = FileUtil.readFile("objectoriented/generics/basic/GenericMethodTypeFailure.oinca")
    checkModuleErrors(code)
  }

  test("unknown and unbound Type Parameter") {
    val code = FileUtil.readFile("objectoriented/generics/basic/UnknownTypeParameter.oinca")
    checkModuleErrors(code)
  }

  test("unknown and unbound Type Parameter scoping") {
    val code = FileUtil.readFile("objectoriented/generics/basic/UnknownTypeParamScoping.oinca")
    checkModuleErrors(code)
  }


  test("Generic Linked List") {
    val code = FileUtil.readFile("objectoriented/generics/collections/GenericLinkedList.oinca")
    checkModule(code)
  }

  test("Generic Linked List (nodes use same type parameter)") {
    val code = FileUtil.readFile("objectoriented/generics/collections/GenericLinkedListShadowing.oinca")
    checkModule(code)
  }


  test("Simple generic class inheritance") {
    val code = FileUtil.readFile("objectoriented/generics/inheritance/GenericClassInheritance.oinca")
    checkModule(code)
  }

  test("Generic class inheritance chain") {
    val code = FileUtil.readFile("objectoriented/generics/inheritance/GenericClassInheritanceChain.oinca")
    checkModule(code)
  }

  test("Addition of Generic Param with Inheritance Failure") {
    val code = FileUtil.readFile("objectoriented/generics/inheritance/GenericClassInheritanceGenericParamAdditionFailure.oinca")
    checkModuleErrors(code)
  }

  test("Generic class inheritance method wrong param type Failure") {
    val code = FileUtil.readFile("objectoriented/generics/inheritance/GenericClassInheritanceMethodTypeFailure.oinca")
    checkModuleErrors(code)
  }

  test("Generic class inheritance method with arguments") {
    val code = FileUtil.readFile("objectoriented/generics/inheritance/GenericClassInheritanceMethodWithArgument.oinca")
    checkModule(code)
  }

  test("Generic class inheritance with fixed Type Parameter") {
    val code = FileUtil.readFile("objectoriented/generics/inheritance/genericClassInheritanceWithFixedTypeParameter.oinca")
    checkModule(code)
  }

  test("Generic class inheritance with multiple Type Parameters") {
    val code = FileUtil.readFile("objectoriented/generics/inheritance/GenericClassInheritanceWithMultipleTypeParameters.oinca")
    checkModule(code)
  }

  test("Simple generic method inheritance") {
    val code = FileUtil.readFile("objectoriented/generics/inheritance/GenericMethodInheritance.oinca")
    checkModule(code)
  }


  test("Simple nested generics") {
    val code = FileUtil.readFile("objectoriented/generics/nested/NestedGenerics.oinca")
    checkModule(code)
  }

  test("Nested generics") {
    val code = FileUtil.readFile("objectoriented/generics/nested/NestedGenerics2.oinca")
    checkModule(code)
  }

  test("Nested generics with inheritance") {
    val code = FileUtil.readFile("objectoriented/generics/nested/NestedGenericsInheritance.oinca")
    checkModule(code)
  }






}