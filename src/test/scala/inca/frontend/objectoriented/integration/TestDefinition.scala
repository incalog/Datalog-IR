package inca.frontend.objectoriented.integration

import scala.meta.XtensionQuasiquoteTerm

case class TestDefinition[O](fileName: String, mainClass: String, mainMethod: String, input: Seq[meta.Term], expectedResult: O)(implicit subdir: Option[String] = None) {
  private val testDir: String = "objectoriented/unittests/"
  private val fileExtension: String = "oinca"

  val filePath: String = {
    val dir = if (subdir.isDefined) {
      val d = subdir.get
      if (!d.endsWith("/")) d + "/" else d
    } else {
      ""
    }
    s"$testDir$dir$fileName.$fileExtension"
  }

  val testName: String = s"$fileName Test"
  val main: String = mainClass + "$" + mainMethod
}

object TestDefinition {

  // type alias for tuple and set results
  // TODO: Replace this with actual classes ?
  type TupleResult[T] = Seq[T]

  object TupleResult {
    def apply(values: Any*): Seq[Any] = values
  }

  object UnitResult {
    def apply(): TupleResult[Any] = TupleResult()
  }

  type SetResult[T] = Set[T]

  object SetResult {
    def apply(values: Any*): Set[Any] = values.toSet
  }

  def baseTests: Seq[TestDefinition[Int]] = {
    implicit val subdir: Option[String] = Some("base")
    Seq(
      TestDefinition("Base1", "Base1", "main", Seq(), 43),
      TestDefinition("Base2", "Base2", "main", Seq(), 43),
      TestDefinition("Base3", "Base3", "main", Seq(), 43)
    )
  }

  def factorialTest: TestDefinition[Int] = {
    TestDefinition("Fact", "Factorial", "main", Seq(q"5"), 120)
  }

  def fibonacciTest: TestDefinition[Int] = {
    TestDefinition("Fib", "Fibonacci", "main", Seq(q"11"), 89)
  }

  def fieldTests: Seq[TestDefinition[Int]] = {
    implicit val subdir: Option[String] = Some("field")
    Seq(
      TestDefinition("FieldAccess", "Fraction", "main", Seq(q"16", q"8"), 2),
      TestDefinition("FieldAccessNested", "A", "main", Seq(), 3),
      TestDefinition("FieldDeclare", "A", "main", Seq(), 3),
      TestDefinition("FieldInheritance", "A", "main", Seq(), 10)
    )
  }

  def constructorTest: TestDefinition[Int] = {
    TestDefinition("Constructor", "Fraction", "main", Seq(q"16", q"8", q"1"), 3)
  }

  def nullTest: TestDefinition[Boolean] = {
    TestDefinition("Null", "NullTest", "main", Seq(), true)
  }

  def equalsTest: TestDefinition[Boolean] = {
    TestDefinition("Equals", "EqualsTest", "main", Seq(), true)
  }

  def instanceOfTest: TestDefinition[Boolean] = {
    TestDefinition("InstanceOf", "A", "main", Seq(), true)
  }

  def typeCastTest: TestDefinition[Boolean] = {
    TestDefinition("TypeCast", "A", "main", Seq(), true)
  }

  def typeCastFailureTest: TestDefinition[Seq[Unit]] = {
    TestDefinition("TypeCastFail", "A", "main", Seq(), Seq())
  }

  def dynamicDispatchTest: TestDefinition[String] = {
    TestDefinition("DynamicDispatch", "A", "main", Seq(), "BBC")
  }

  def objectAsParamTest: TestDefinition[Int] = {
    TestDefinition("ParamObject", "A", "main", Seq(), 1)
  }

  def methodInheritanceTest: TestDefinition[Int] = {
    TestDefinition("MethodInheritance", "A", "main", Seq(), 3)
  }

  def binaryTreeSumTest: TestDefinition[Int] = {
    TestDefinition("BinaryTree", "DefinedNode", "main", Seq(), 20)
  }

  def plusTest: TestDefinition[Int] = {
    TestDefinition("Plus", "Nat", "main", Seq(), 5)
  }

  def mutabilityTest: TestDefinition[Boolean] = {
    TestDefinition("Mutability", "A", "main", Seq(), true)
  }

  def varAssignmentTest: TestDefinition[Boolean] = {
    TestDefinition("VarAssignment", "A", "main", Seq(q"3"), true)
  }

  def ifConstantTests: Seq[TestDefinition[Boolean]] = {
    implicit val subdir: Option[String] = Some("if")
    Seq(
      TestDefinition("IfTrue", "IfTest", "main", Seq(), true),
      TestDefinition("IfFalse", "IfTest", "main", Seq(), true)
    )
  }

  def ifNestedTests: Seq[TestDefinition[Int]] = {
    implicit val subdir: Option[String] = Some("if")
    Seq(
      TestDefinition("If", "IfTest", "main", Seq(q"true", q"true"), 11),
      TestDefinition("If", "IfTest", "main", Seq(q"true", q"false"), 7),
      TestDefinition("If", "IfTest", "main", Seq(q"false", q"false"), 6)
    )
  }

  def ifDuplicateTest: TestDefinition[Int] = {
    implicit val subdir: Option[String] = Some("if")
    TestDefinition("IfDuplicate", "IfTest", "main", Seq(q"true", q"true"), 10)
  }

  def returnTests: Seq[TestDefinition[Int]] = {
    implicit val subdir: Option[String] = Some("return")
    Seq(
      TestDefinition("Return", "ReturnTest", "main", Seq(q"true"), 1),
      TestDefinition("Return", "ReturnTest", "main", Seq(q"false"), 2),
      TestDefinition("ReturnTwice", "ReturnTest", "main", Seq(), 1)
    )
  }

  def returnImplicitTests: Seq[TestDefinition[Int]] = {
    implicit val subdir: Option[String] = Some("return")
    Seq(
      TestDefinition("ReturnImplicit", "ReturnTest", "main", Seq(), 1),
      TestDefinition("ReturnImplicitIf", "ReturnTest", "main", Seq(q"true"), 1)
    )
  }

  def returnUnitTests: Seq[TestDefinition[TupleResult[Any]]] = {
    implicit val subdir: Option[String] = Some("return")
    Seq(
      TestDefinition("ReturnImplicitUnit", "ReturnTest", "main", Seq(), UnitResult()),
      TestDefinition("Unit", "A", "main", Seq(), UnitResult())
    )
  }

  def superTest: TestDefinition[Int] = {
    TestDefinition("Super", "A", "main", Seq(), 10)
  }

  def tupleTest: TestDefinition[TupleResult[Any]] = {
    TestDefinition("Tuple", "A", "main", Seq(), TupleResult(true, TupleResult(true, true)))
  }

  def simpleSetTests: Seq[TestDefinition[SetResult[Any]]] = {
    implicit val subdir: Option[String] = Some("set")
    Seq(
      TestDefinition("Set", "A", "main", Seq(), SetResult(1, 2, 3)),
      TestDefinition("SetConst", "A", "main", Seq(), SetResult(1, 2, 3)),
      TestDefinition("SetConstVar", "A", "main", Seq(), SetResult(1, 2, 3)),
      TestDefinition("SetMultiVar", "A", "main", Seq(), SetResult(1, 2, 3)),
      TestDefinition("SetParam", "A", "main", Seq(), SetResult(1, 2, 3)),
      TestDefinition("SetParamSquare", "A", "main", Seq(), SetResult(1, 4, 9)),
      TestDefinition("SetFieldDeclare", "A", "main", Seq(), SetResult(1, 2, 3)),
      TestDefinition("SetFieldSet", "A", "main", Seq(), SetResult(1, 2, 3))
    )
  }

  def tupleSetTest: TestDefinition[SetResult[Any]] = {
    implicit val subdir: Option[String] = Some("set")
    TestDefinition("SetTuple", "A", "main", Seq(), SetResult(TupleResult(1, "A"), TupleResult(2, "B"), TupleResult(3, "C")))
  }

  def unionIntersectionSetTest: Seq[TestDefinition[SetResult[Any]]] = {
    implicit val subdir: Option[String] = Some("set")
    Seq(
      TestDefinition("SetIntersection", "A", "main", Seq(), SetResult(1, 3)),
      TestDefinition("SetUnion", "A", "main", Seq(), SetResult(1, 2, 3, 4)),
      TestDefinition("SetUnionMixed", "A", "main", Seq(), SetResult(1, 2, 3, 4)),
      TestDefinition("SetUnionIntersection", "A", "main", Seq(), SetResult(1, 2, 3, 4))
    )
  }

  def advancedSetTest: Seq[TestDefinition[SetResult[Any]]] = {
    implicit val subdir: Option[String] = Some("set")
    Seq(
      TestDefinition("SetMethodNested", "A", "main", Seq(), SetResult(1, 2, 3, 4)),
      TestDefinition("SetClassSimple", "A", "main", Seq(), SetResult(1, 2)),
      TestDefinition("SetClass", "A", "main", Seq(), SetResult(10, 3)),
      TestDefinition("SetClass2", "A", "main", Seq(), SetResult(5, 10)),
      TestDefinition("SetIf", "A", "main", Seq(q"true"), SetResult(1, 2)),
      TestDefinition("SetIf", "A", "main", Seq(q"false"), SetResult(1, 3, 4))
    )
  }

  def comprehensionSetTest: Seq[TestDefinition[SetResult[Any]]] = {
    implicit val subdir: Option[String] = Some("set")
    Seq(
      TestDefinition("SetComprehension", "A", "main", Seq(), SetResult(TupleResult(1, 3, 5), TupleResult(1, 4, 5))),
      TestDefinition("SetComprehensionTuple", "A", "main", Seq(), SetResult(TupleResult("A", 2), TupleResult("C", 2)))
    )
  }
}
