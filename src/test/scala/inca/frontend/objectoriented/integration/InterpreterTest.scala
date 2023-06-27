package inca.frontend.objectoriented.integration

import inca.frontend.objectoriented.integration.TestDefinition._
import inca.frontend.objectoriented.interpreter.{Interpreter, ScalaInterpreter, ScalaValue, TypeCastException}
import inca.frontend.objectoriented.parser.Parser
import inca.frontend.objectoriented.transformations.AddMissingDefinitions
import inca.frontend.objectoriented.typechecker.Typechecker
import inca.util.FileUtil
import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite

class InterpreterTest extends AnyFunSuite {
  val parser: Parser = new Parser {}
  val typechecker: Typechecker = new Typechecker {}
  val scalaInterpreter: ScalaInterpreter = new ScalaInterpreter {}

  def runProg(path: String, mainClass: String, mainMethod: String, args: Seq[Any]): Any = {
    val code = FileUtil.readFile(path)
    var mod = Parser.parse(code)
    mod = AddMissingDefinitions.transformModule(mod)
    typechecker.typecheck(mod)

    val mainClasses = mod.classes.filter(_.name.raw == mainClass)
    val mainMethods = mainClasses.flatMap(c => c.methods.filter(_.name.raw == mainMethod))
    if (mainMethods.size < 1) {
      throw new IllegalArgumentException(s"No main method with name $mainMethod for class $mainClass found!")
    } else if (mainMethods.size > 1) {
      throw new IllegalArgumentException(s"Ambiguous method with name $mainMethod for class $mainClass found!")
    }

    val main = mainMethods.head
    if (!main.isMain) {
      throw new IllegalArgumentException(s"Method $mainMethod for class $mainClass is not a main method!")
    }

    val res = new Interpreter(mod).interp(main, args.map(ScalaValue))
    res.asScala
  }

  def performTests(tests: TestDefinition[_]*): Seq[Assertion] = {
    tests.map { test =>
      val input = test.input.map(arg => scalaInterpreter.interp(arg.syntax))
      val actual = runProg(test.filePath, test.mainClass, test.mainMethod, input)
      assert(actual == test.expectedResult)
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
    caught.obj.asObject match {
      case (cls, _, _) => assertResult("A")(cls)
    }
    assertResult("B")(caught.typ)
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

  test("Case Class") {
    performTests(caseClassTest)
  }

  // Set - Fixpoint
  test("Case Class - Transitive Closure") {
    performTests(caseClassTransitiveClosureTest)
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

  // Set - Fixpoint
  test("Set Recursive Example") {
    performTests(recursiveSetTest)
  }

  test("Set fold") {
    performTests(foldSetTests: _*)
  }

  // TODO: support this ?
  /*test("Set fold - Projection") {
    performTests(foldSetProjectionTest)
  }*/
}
