package inca.frontend.objectoriented.integration

import inca.frontend.datalog.Relation

import scala.meta.XtensionQuasiquoteTerm

case class TestDefinition[O](fileName: String, mainClass: String, mainMethod: String, input: Seq[meta.Term], expectedResult: O)(implicit subdir: Option[String] = None) {
  private val testDir: String = "objectoriented/"
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

  def expectedRelation: Relation = {
    val entries = expectedResult match {
      case s: Set[Any] => s.map {
          case e: Seq[_] => e
          case e => Seq(e)
        }.toSeq
      case s: Seq[Any] if s.nonEmpty => Seq(s)
      case s: Seq[Any] if s.isEmpty => Seq()
      case e => Set(Seq(e))
    }
    val paramNames =
      if (entries.isEmpty)
        Seq()
      else
        Range(0, entries.head.size).map(i => "return$" + i)
    Relation.from(main, paramNames, entries)
  }

  val testName: String = s"$fileName Test"
  val main: String = mainClass + "$" + mainMethod
}

object TestDefinition {

  // type alias for tuple and set results
  // TODO: Remove this after the ScalaExecutor is replaced
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
    implicit val subdir: Option[String] = Some("unittests/base")
    Seq(
      TestDefinition("Base1", "Base", "main", Seq(), 43),
      TestDefinition("Base2", "Base", "main", Seq(), 43),
      TestDefinition("Base3", "Base", "main", Seq(), 43)
    )
  }

  def factorialTest: TestDefinition[Int] = {
    implicit val subdir: Option[String] = Some("unittests")
    TestDefinition("Fact", "Factorial", "main", Seq(q"5"), 120)
  }

  def fibonacciTest: TestDefinition[Int] = {
    implicit val subdir: Option[String] = Some("unittests")
    TestDefinition("Fib", "Fibonacci", "main", Seq(q"11"), 89)
  }

  def fieldTests: Seq[TestDefinition[Int]] = {
    implicit val subdir: Option[String] = Some("unittests/field")
    Seq(
      TestDefinition("FieldAccess", "Fraction", "main", Seq(q"16", q"8"), 2),
      TestDefinition("FieldAccessNested", "A", "main", Seq(), 3),
      TestDefinition("FieldDeclare", "A", "main", Seq(), 3),
      TestDefinition("FieldInheritance", "A", "main", Seq(), 10)
    )
  }

  def constructorTest: TestDefinition[Int] = {
    implicit val subdir: Option[String] = Some("unittests")
    TestDefinition("Constructor", "Fraction", "main", Seq(q"16", q"8", q"1"), 3)
  }

  def nullTest: TestDefinition[Boolean] = {
    implicit val subdir: Option[String] = Some("unittests")
    TestDefinition("Null", "C", "main", Seq(), true)
  }

  def equalsTest: TestDefinition[Boolean] = {
    implicit val subdir: Option[String] = Some("unittests")
    TestDefinition("Equals", "D", "main", Seq(), true)
  }

  def instanceOfTest: TestDefinition[Boolean] = {
    implicit val subdir: Option[String] = Some("unittests")
    TestDefinition("InstanceOf", "A", "main", Seq(), true)
  }

  def typeCastTest: TestDefinition[Boolean] = {
    implicit val subdir: Option[String] = Some("unittests")
    TestDefinition("TypeCast", "A", "main", Seq(), true)
  }

  def typeCastFailureTest: TestDefinition[Seq[Unit]] = {
    implicit val subdir: Option[String] = Some("unittests")
    TestDefinition("TypeCastFail", "A", "main", Seq(), Seq())
  }

  def dynamicDispatchTest: TestDefinition[String] = {
    implicit val subdir: Option[String] = Some("unittests")
    TestDefinition("DynamicDispatch", "A", "main", Seq(), "BBC")
  }

  def objectAsParamTest: TestDefinition[Int] = {
    implicit val subdir: Option[String] = Some("unittests")
    TestDefinition("ParamObject", "A", "main", Seq(), 1)
  }

  def methodInheritanceTest: TestDefinition[Int] = {
    implicit val subdir: Option[String] = Some("unittests")
    TestDefinition("MethodInheritance", "A", "main", Seq(), 3)
  }

  def plusTest: TestDefinition[Int] = {
    implicit val subdir: Option[String] = Some("unittests")
    TestDefinition("Plus", "Nat", "main", Seq(), 5)
  }

  def mutabilityTest: TestDefinition[Boolean] = {
    implicit val subdir: Option[String] = Some("unittests")
    TestDefinition("Mutability", "A", "main", Seq(), true)
  }

  def varAssignmentTest: TestDefinition[Boolean] = {
    implicit val subdir: Option[String] = Some("unittests")
    TestDefinition("VarAssignment", "A", "main", Seq(q"3"), true)
  }

  def ifConstantTests: Seq[TestDefinition[Boolean]] = {
    implicit val subdir: Option[String] = Some("unittests/if")
    Seq(
      TestDefinition("IfTrue", "IfTest", "main", Seq(), true),
      TestDefinition("IfFalse", "IfTest", "main", Seq(), true)
    )
  }

  def ifNestedTests: Seq[TestDefinition[Int]] = {
    implicit val subdir: Option[String] = Some("unittests/if")
    Seq(
      TestDefinition("If", "IfTest", "main", Seq(q"true", q"true"), 11),
      TestDefinition("If", "IfTest", "main", Seq(q"true", q"false"), 7),
      TestDefinition("If", "IfTest", "main", Seq(q"false", q"false"), 6)
    )
  }

  def ifDuplicateTest: TestDefinition[Int] = {
    implicit val subdir: Option[String] = Some("unittests/if")
    TestDefinition("IfDuplicate", "IfTest", "main", Seq(q"true", q"true"), 10)
  }

  def returnTests: Seq[TestDefinition[Int]] = {
    implicit val subdir: Option[String] = Some("unittests/return")
    Seq(
      TestDefinition("Return", "ReturnTest", "main", Seq(q"true"), 1),
      TestDefinition("Return", "ReturnTest", "main", Seq(q"false"), 2),
      TestDefinition("ReturnTwice", "ReturnTest", "main", Seq(), 1)
    )
  }

  def returnImplicitTests: Seq[TestDefinition[Int]] = {
    implicit val subdir: Option[String] = Some("unittests/return")
    Seq(
      TestDefinition("ReturnImplicit", "ReturnTest", "main", Seq(), 1),
      TestDefinition("ReturnImplicitIf", "ReturnTest", "main", Seq(q"true"), 1)
    )
  }

  def returnUnitTests: Seq[TestDefinition[TupleResult[Any]]] = {
    implicit val subdir: Option[String] = Some("unittests/return")
    Seq(
      TestDefinition("ReturnImplicitUnit", "ReturnTest", "main", Seq(), UnitResult()),
      TestDefinition("Unit", "A", "main", Seq(), UnitResult())
    )
  }

  def superTest: TestDefinition[Int] = {
    implicit val subdir: Option[String] = Some("unittests")
    TestDefinition("Super", "A", "main", Seq(), 10)
  }

  def tupleTest: TestDefinition[TupleResult[Any]] = {
    implicit val subdir: Option[String] = Some("unittests")
    TestDefinition("Tuple", "A", "main", Seq(), TupleResult(true, true, true))
  }

  def simpleSetTests: Seq[TestDefinition[SetResult[Any]]] = {
    implicit val subdir: Option[String] = Some("unittests/set")
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
    implicit val subdir: Option[String] = Some("unittests/set")
    TestDefinition("SetTuple", "A", "main", Seq(), SetResult(TupleResult(1, "A"), TupleResult(2, "B"), TupleResult(3, "C")))
  }

  def unionIntersectionSetTest: Seq[TestDefinition[SetResult[Any]]] = {
    implicit val subdir: Option[String] = Some("unittests/set")
    Seq(
      TestDefinition("SetIntersection", "A", "main", Seq(), SetResult(1, 3)),
      TestDefinition("SetIntersection2", "A", "main", Seq(), SetResult(1, 3)),
      TestDefinition("SetUnion", "A", "main", Seq(), SetResult(1, 2, 3, 4)),
      TestDefinition("SetUnionMixed", "A", "main", Seq(), SetResult(1, 2, 3, 4)),
      TestDefinition("SetUnionIntersection", "A", "main", Seq(), SetResult(1, 2, 3, 4))
    )
  }

  def advancedSetTest: Seq[TestDefinition[SetResult[Any]]] = {
    implicit val subdir: Option[String] = Some("unittests/set")
    Seq(
      TestDefinition("SetMethodNested", "A", "main", Seq(), SetResult(1, 2)),
      TestDefinition("SetClassSimple", "A", "main", Seq(), SetResult(1, 2)),
      TestDefinition("SetClass", "A", "main", Seq(), SetResult(10, 3)),
      TestDefinition("SetClass2", "A", "main", Seq(), SetResult(5, 10)),
      TestDefinition("SetClassTuple", "A", "main", Seq(), SetResult(TupleResult(1, "A"), TupleResult(2, "B"), TupleResult(3, "C"))),
      TestDefinition("SetIf", "A", "main", Seq(q"true"), SetResult(1, 2)),
      TestDefinition("SetIf", "A", "main", Seq(q"false"), SetResult(1, 3, 4))
    )
  }

  def comprehensionSetTest: Seq[TestDefinition[SetResult[Any]]] = {
    implicit val subdir: Option[String] = Some("unittests/set")
    Seq(
      TestDefinition("SetComprehension", "A", "main", Seq(), SetResult(TupleResult(1, 1), TupleResult(2, 1))),
      TestDefinition("SetComprehension2", "A", "main", Seq(), SetResult(TupleResult(1, 3, 5), TupleResult(1, 4, 5))),
      TestDefinition("SetComprehension3", "A", "main", Seq(), SetResult(TupleResult(1, 3), TupleResult(1, 4), TupleResult(2, 3), TupleResult(2, 4))),
      TestDefinition("SetComprehensionTuple", "A", "main", Seq(), SetResult(TupleResult("A", 2), TupleResult("C", 2)))
    )
  }

  def emptySetTest: TestDefinition[SetResult[Any]] = {
    implicit val subdir: Option[String] = Some("unittests/set")
    TestDefinition("SetEmpty", "A", "main", Seq(), SetResult(TupleResult(1)))
    TestDefinition("SetEmptyVar", "A", "main", Seq(), SetResult(TupleResult(1)))
    TestDefinition("SetEmptyConst", "A", "main", Seq(), SetResult())
    TestDefinition("SetEmptyParam", "A", "main", Seq(), SetResult(TupleResult(2)))
  }

  def recursiveSetTest: TestDefinition[SetResult[Any]] = {
    implicit val subdir: Option[String] = Some("unittests/set")
    TestDefinition("SetRecursive", "Graph", "main", Seq(), SetResult("W", "Y", "Z", "X"))
  }

  def caseClassTests: Seq[TestDefinition[SetResult[Any]]] = {
    implicit val subdir: Option[String] = Some("unittests/caseclass")
    Seq(
      TestDefinition("CaseClass", "A", "main", Seq(), SetResult(true)),
      TestDefinition("TransitiveClosure", "Graph", "main", Seq(), SetResult(
        TupleResult("X", "X"), TupleResult("X", "Y"), TupleResult("X", "Z"), TupleResult("X", "W"),
        TupleResult("Y", "X"), TupleResult("Y", "Y"), TupleResult("Y", "Z"), TupleResult("Y", "W"),
        TupleResult("Z", "X"), TupleResult("Z", "Y"), TupleResult("Z", "Z"), TupleResult("Z", "W"),
        TupleResult("A", "W"),
        TupleResult("B", "A"), TupleResult("B", "C"), TupleResult("B", "W")
      )),
    )
  }

  def monotoneTests: Seq[TestDefinition[SetResult[Any]]] = {
    implicit val subdir: Option[String] = Some("unittests/monotone")
    Seq(
      //TestDefinition("Avg", "Example", "main", Seq(), SetResult(3.5)),
      TestDefinition("Map", "Example", "main", Seq(), SetResult(Map("Zero" -> 0, "One" -> 1, "Two" -> 2))),
    )
  }

  def foldSetTests: Seq[TestDefinition[SetResult[Any]]] = {
    implicit val subdir: Option[String] = Some("unittests/setfold")
    Seq(
      TestDefinition("SetFoldMax", "Num", "main", Seq(), SetResult(16)),
      TestDefinition("SetFoldSum", "Num", "main", Seq(), SetResult(23)),
      TestDefinition("SetFoldSumProjection", "Num", "main", Seq(), SetResult(TupleResult(55, 110, 4, 2, 1, 4))),
      TestDefinition("SetFoldMaxObject", "Num", "main", Seq(), SetResult(5)),
      TestDefinition("SetFoldSumObject", "Num", "main", Seq(), SetResult(15)),
      TestDefinition("SetFoldTupleField", "Num", "main", Seq(), SetResult(5)),
      TestDefinition("SetFoldNull", "Num", "main", Seq(), SetResult(7))
    )
  }

  def transitiveClosureTest: TestDefinition[SetResult[Any]] = {
    implicit val subdir: Option[String] = Some("graphs")
    TestDefinition("TransitiveClosure", "Graph", "main", Seq(), SetResult("X", "Z", "Y"))
  }

  def treeTest: TestDefinition[SetResult[Any]] = {
    implicit val subdir: Option[String] = Some("graphs")
    TestDefinition("Tree", "Tree", "main", Seq(), SetResult(1.to(20):_*))
  }

  def binaryTreeTest: TestDefinition[Int] = {
    implicit val subdir: Option[String] = Some("graphs")
    TestDefinition("BinaryTree", "DefinedNode", "main", Seq(), 20)
  }

  def doubleLinkedTest: TestDefinition[TupleResult[Any]] = {
    implicit val subdir: Option[String] = Some("graphs")
    TestDefinition("DoubleLinkedList", "DoubleLinkedList", "main", Seq(q"5"), TupleResult(31, 36, 4))
  }

  def cfgVisitorTest: TestDefinition[SetResult[Any]] = {
    val expectedRes = SetResult(
      TupleResult("VarDef", "While"), TupleResult("Assign", "While"), TupleResult("While", "Assign"),
      TupleResult("VarDef", "VarDef"), TupleResult("Skip", "Skip"), TupleResult("Assign", "Assign")
    )

    implicit val subdir: Option[String] = Some("casestudy")
    TestDefinition("CfgVisitor", "Examples", "main", Seq(), expectedRes)
  }

  def whileLangTest: TestDefinition[SetResult[Any]] = {
    implicit val subdir: Option[String] = Some("casestudy")
    TestDefinition("WhileLang", "ConstantPropagation", "factorial", Seq(), SetResult(
      TupleResult("m", "SomeConstant(3)"),
      TupleResult("n", "NoConstant"),
      TupleResult("acc", "NoConstant")
    ))
    //TestDefinition("WhileLang", "AdvancedConstantPropagation", "factorial", Seq(), SetResult())
  }

  def noDemandTest: TestDefinition[Int] = {
    implicit val subdir: Option[String] = Some("unittests")
    TestDefinition("NoDemand", "A", "main", Seq(), 3)
  }

  def abstractSyntaxGraphTest: TestDefinition[Any] = {
    implicit val subdir: Option[String] = Some("syntax")
    TestDefinition("AbstractSyntaxGraph", "Main", "main", Seq(), SetResult("a", "c"))
  }

  def loopTest: TestDefinition[Any] = {
    implicit val subdir: Option[String] = Some("graphs")
    TestDefinition("Loop", "Main", "main", Seq(), SetResult(0.5, 2.0))
  }

  def monoMapTest: TestDefinition[Any] = {
    implicit val subdir: Option[String] = Some("unittests/monotone")
    TestDefinition("Builtin", "Main", "main", Seq(), SetResult(9, 7))
  }
}
