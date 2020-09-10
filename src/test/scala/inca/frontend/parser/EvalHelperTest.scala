package inca.frontend.parser

import inca.frontend.core.Core.{Continue, Name}
import org.scalatest.funsuite.AnyFunSuite

import scala.meta.Term._
import scala.meta.{Case, Defn, Enumerator, Init, Lit, Mod, Pat, Term, Tree, Type, XtensionParseInputLike}

class EvalHelperTest extends AnyFunSuite {

  private val tInt = Type.Name("Int")

  private val defValue = Defn.Val(Nil, List(Pat.Var(Name("value"))), None, Name("free"))
  private val defVar = Defn.Var(Nil, List(Pat.Var(Name("value"))), None, Some(Name("free")))
  private val undefinedVar = Defn.Var(Nil, List(Pat.Var(Name("value"))),Some(Type.Name("Int")), None)

  private val paramN = Param(Nil, Name("n"), Some(tInt), None)

  test("test freeVars Name") {
    checkVars(Term.Name("x"), Set("x"))
  }

  test("test freeVars select") {
    checkVars(Term.Select(Name("x"), Name("prop")), Set("x"))
  }

  test("test freeVars apply unary") {
    checkVars(ApplyUnary(Name("!"), Name("ten")), Set("ten"))
  }

  test("test freeVars apply infix") {
    checkVars(ApplyInfix(Name("x"), Name("-"), Nil, List(Name("y"))), Set("x", "y"))
  }

  test("test freeVars return") {
    checkVars(Return(Name("value")), Set("value"))
  }

  test("test freeVars ascribe") {
    checkVars(Ascribe(Name("value"), tInt), Set("value"))
  }

  test("test freeVars throw") {
    checkVars(Throw(Name("value")), Set("value"))
  }

  test("test freeVars apply") {
    checkVars(Apply(Name("fun"), List(Name("arg"))), Set("fun", "arg"))
  }

  test("test freeVars tuple") {
    checkVars(Tuple(List(Name("val1"), Name("val2"), Name("val3"))), Set("val1", "val2", "val3"))
  }

  test("test freeVars definition val") {
    checkVars(defValue, Set("free"))
  }

  test("test freeVars definition var no rhs") {
    checkVars(undefinedVar, Set())
  }

  test("test freeVars definition var with rhs") {
    checkVars(defVar, Set("free"))
  }

  test("test freeVars function") {
    checkVars(Function(List(paramN), Name("n")), Set())
  }

  test("test freeVars assign free var") {
    val code = Assign(Name("free"), Name("free1"))
    checkVars(code, Set("free", "free1"))
  }

  test("test freeVars block single global block") {
    val code = Block(
      List(
        Name("stat1"),
        Name("stat2")
      )
    )
    checkVars(code, Set("stat1", "stat2"))
  }

  test("test freeVars block code after local block") {
    val code = Block(
      List(
        Block(
          List(
            Name("free1"),
            defValue
          )
        ),
        Name("value")
      )
    )
    checkVars(code, Set("free1", "free", "value"))
  }

  test("test freeVars block nested blocks") {
    val code = Block(
      List(
        defValue,
        Block(
          List(
            defVar,
            Name("value")
          )
        ),
        Name("value")
      )
    )
    checkVars(code, Set("free"))
  }

  test("test freeVars assign bound var") {
    val code = Block(
      List(
        undefinedVar,
        Assign(Name("value"), Name("free"))
      )
    )
    checkVars(code, Set("free"))
  }

  test("test freeVars if") {
    val code = If(Name("cond"), Name("free"), Name("free1"))
    checkVars(code, Set("cond", "free", "free1"))
  }

  test("test freeVars try no catch no finally") {
    val code = Try(
      Name("free"),
      Nil,
      None
    )
    checkVars(code, Set("free"))
  }

  test("test freeVars try one catch no finally") {
    val code = Try(
      Name("free"),
      List(
        Case(
          Pat.Var(Name("e")),
          None,
          Name("e")
        )
      ),
      None
    )
    checkVars(code, Set("free"))
  }

  test("test freeVars try no catch finally") {
    val code = Try(
      Name("free"),
      Nil,
      Some(
        Name("free1")
      )
    )
    checkVars(code, Set("free", "free1"))
  }

  test("test freeVars try multiple catch finally") {
    val code = Try(
      Name("free"),
      List(
        Case(
          Pat.Var(Name("e1")),
          None,
          Name("e2")
        ),
        Case(
          Pat.Var(Name("e")),
          Some(Name("cond")),
          Name("e")
        )
      ),
      Some(
        Name("free1")
      )
    )
    checkVars(code, Set("free", "free1", "e2", "cond"))
  }

  test("test freeVars try with handler") {
    val code = TryWithHandler(
      Name("free"),
      Name("e"),
      None
    )
    checkVars(code, Set("free", "e"))
  }

  test("test freeVars for") {
    val code = For(
      List(
        Enumerator.Generator(Pat.Var(Name("x")), Name("xs")),
        Enumerator.Val(Pat.Var(Name("ys")), Name("free"))
      ),
      Name("x")
    )
    checkVars(code, Set("free", "xs"))
  }

  test("test freeVars for yield") {
    val code = ForYield(
      List(
        Enumerator.Generator(Pat.Var(Name("x")), Name("xs")),
        Enumerator.Val(Pat.Var(Name("ys")), Name("free"))
      ),
      Name("x")
    )
    checkVars(code, Set("free", "xs"))
  }

  test("test freeVars while") {
    val code = While(
      Name("cond"),
      Name("body")
    )
    checkVars(code, Set("cond", "body"))
  }

  test("test freeVars do") {
    val code = Do(
      Name("body"),
      Name("cond")
    )
    checkVars(code, Set("body", "cond"))
  }

  test("test freeVars match single case") {
    val code = Match(
      Name("free"),
      List(
        Case(
          Pat.Var(Name("x")),
          Some(Name("cond")),
          Name("x")
        )
      )
    )
    checkVars(code, Set("free", "cond"))
  }

  test("test freeVars match multiple cases") {
    val code = Match(
      Name("free"),
      List(
        Case(
          Pat.Typed(Pat.Var(Name("i")), tInt),
          None,
          Name("i")
        ),
        Case(
          Pat.Var(Name("x")),
          Some(Name("cond")),
          Name("x")
        )
      )
    )
    checkVars(code, Set("free", "cond"))
  }

  test("test freeVars partial function") {
    val code = PartialFunction(
      List(
        Case(
          Pat.Typed(Pat.Var(Name("name")), tInt),
          None,
          Name("name")
        ),
        Case(
          Pat.Wildcard(),
          Some(Name("cond")),
          Name("x")
        )
      )
    )
    checkVars(code, Set("cond", "x"))
  }

  test("test freeVars new") {
    val code = New(
      Init(
        tInt,
        Name("Int"),
        List(
          List(
            Name("arg1"),
            Name("arg2")
          ),
          List(
            Name("arg3"),
            Name("arg4")
          )
        )
      )
    )
    checkVars(code, Set("arg1", "arg2", "arg3", "arg4"))
  }

  test("test freeVars interpolate") {
    val code = Interpolate(
      Name("s"),
      List(
        Lit.String("hello "),
        Lit.String("body"),
        Lit.String("")
      ),
      List(
        Name("x"),
        Name("y")
      )
    )
    checkVars(code, Set("s", "x", "y"))
  }

  test("test freeVars annotate") {
    val code = Annotate(
      Name("free"),
      List(
        Mod.Annot(
          Init(
            tInt,
            Name("hello"),
            List(
              List(
                Name("arg")
              )
            )
          )
        )
      )
    )
    checkVars(code, Set("free", "arg"))
  }

  test("test freeVars repeated") {
    val code = Repeated(
      Name("x")
    )
    checkVars(code, Set("x"))
  }

  test("test freeVars complex") {
    val code1 =
      """
        |{
        |  val (a, b) = x
        |  var Set(str1, str2) = a
        |  val fun: Any => Any = _ => b
        |  var bar = "fun"
        |  {
        |    var foo = 10
        |    foo match {
        |      case 0 => 0
        |      case num => num + 1
        |    }
        |    bar = i
        |  }
        |  foo = 42
        |  this.anno = 12
        |}
        |""".stripMargin
    val tree1 = code1.parse[Term].get
    checkVars(tree1, Set("x", "i", "foo", "Set"))

    val code2 =
      """
        |{
        |  val fac = num match {
        |    case 0 => 1
        |    case _ => num * factorial(num -1)
        |  }
        |}
        |""".stripMargin
    val tree2 = code2.parse[Term].get
    checkVars(tree2, Set("num", "factorial"))

    val code3 =
      """
        |{
        |  var option: Option[Int] = None
        |  if(true){
        |    option = Some(42)
        |    do_smth(option)
        |  }
        |  else {
        |    option = None
        |  }
        |  val seq = for(init <- newAnon.inits; args <- init.argss; arg <- args if arg.isMandatory) yield {
        |    println(arg.desc)
        |    val data = {
        |      process(arg)
        |      arg.ctx.value
        |    }
        |    data
        |  }
        |}
        |""".stripMargin

    val tree3 = code3.parse[Term].get
    checkVars(tree3, Set("do_smth", "newAnon", "process", "println", "Some", "None"))

    val code4 =
      """
        |list match {
        |  case Nil => Nil
        |  case Some(x :: ys) :: xs => x
        |}
        |""".stripMargin

    val tree4 = code4.parse[Term].get
    checkVars(tree4, Set("list", "Nil", "Some", "::"))
  }

  private def checkVars(code: Tree, expectedFree: Set[Name]): Unit = {
    val free = EvalHelper.freeVars(code)
    assert(free == expectedFree)
  }

}
