package inca.frontend.util

import inca.frontend.core.{Name => _, Param => _, _}
import inca.frontend.parser.EvalHelper
import inca.frontend.{BaseFrontend, core}
import inca.runtime.context.LanguageMetaInfo
import inca.util.Meta.Scala
import org.scalatest.funsuite.AnyFunSuite

import scala.meta.Term._
import scala.meta.{Case, Defn, Enumerator, Init, Lit, Mod, Pat, Term, Tree, Type, XtensionParseInputLike, XtensionQuasiquoteTerm}

class EvalHelperTest extends AnyFunSuite {

  private val tInt = Type.Name("Int")

  private val defValue = Defn.Val(Nil, List(Pat.Var(Name("value"))), None, Name("free"))
  private val defVar = Defn.Var(Nil, List(Pat.Var(Name("value"))), None, Some(Name("free")))
  private val undefinedVar = Defn.Var(Nil, List(Pat.Var(Name("value"))),Some(Type.Name("Int")), None)

  private val paramN = Param(Nil, Name("n"), Some(tInt), None)

  test("test freeVars Name") {
    val code = q"x"
    checkVars(code, Set("x"))
  }

  test("test freeVars select") {
    val code = q"x.prop"
    checkVars(code, Set("x"))
  }

  test("test freeVars apply unary") {
    val code = q"!ten"
    checkVars(code, Set("ten"))
  }

  test("test freeVars apply infix") {
    val code = q"x - y"
    checkVars(code, Set("x", "y"))
  }

  test("test freeVars return") {
    val code = q"return value"
    checkVars(code, Set("value"))
  }

  test("test freeVars ascribe") {
    val code = q"value: Int"
    checkVars(code, Set("value"))
  }

  test("test freeVars throw") {
    val code = q"throw value"
    checkVars(code, Set("value"))
  }

  test("test freeVars apply") {
    val code = q"fun(arg)"
    checkVars(code, Set("fun", "arg"))
  }

  test("test freeVars tuple") {
    val code = q"(val1, val2, val3)"
    checkVars(code, Set("val1", "val2", "val3"))
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
    val code = q"free = free1"
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
        Term.Assign(Name("value"), Name("free"))
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
    val code = q"new Int(arg1, new Int(arg2))(arg3, arg4)"
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

  test("test freeVars pattern extract infix nested") {
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

  test("test freeVars pattern nested complex") {
    val code =
      """
        |matchee match {
        |  case Some(Some(x :: ys) :: Some(z :: (Some(w :: ws))) :: tail) => x + z + w + ws
        |}
        |""".stripMargin

    val tree = code.parse[Term].get
    checkVars(tree, Set("::", "Some", "matchee"))
  }

  test("test freeVars complex 1") {
    val code =
      q"""
        {
          val (a, b) = x
          var Set(str1, str2) = a
          val fun: Any => Any = _ => b
          var bar = "fun"
          {
            var foo = 10
            foo match {
              case 0 => 0
              case num => num + 1
            }
            bar = i
          }
          foo = 42
          this.anno = 12
        }
        """
    checkVars(code, Set("x", "i", "foo", "Set"))
  }

  test("test freeVars complex 2") {
    val code =
      q"""
          {
            type Indexed[E] = Map[Int, E]
            val mapper: Indexed[String] = Map()
            val personMultimap = {
              type MultiIndexed[E] = Map[Int, List[E]]
              type PersonOrder = Ordering[Person]
              val multimap: MultiIndexed[PersonOrder] = mutable.Map()
              var unused: Indexed[Int] = Map()
              loadInto(multimap)
              multimap
            }
          }
       """

    checkVars(code, Set("mutable", "loadInto", "Map"))
  }

  test("test freeVars complex 3") {
    val code =
      q"""
        {
          val fac = num match {
            case 0 => 1
            case _ => num * factorial(num -1)
          }
        }
        """
    checkVars(code, Set("num", "factorial"))
  }

  test("test freeVars complex 4") {
    val code =
      q"""
        {
          var option: Option[Int] = None
          if(true){
            option = Some(42)
            do_smth(option)
          }
          else {
            option = None
          }
          val seq = for(init <- newAnon.inits; args <- init.argss; arg <- args if arg.isMandatory) yield {
            println(arg.desc)
            val data = {
              process(arg)
              arg.ctx.value
            }
            data
          }
        }
        """

    checkVars(code, Set("do_smth", "newAnon", "process", "println", "Some", "None"))
  }

  test("test freeVars complex 5") {
    val code =
      q"""
        {
          val heap: Heap[Node] = Heap(start)
          val seen: Set[Node] = Set()
          while(!heap.isEmpty) {
            val min = heap.extractMin()
            for(edge <- min.edges; if !seen(edge.end)) {
              val end = edge.end
              if(min.distance + edge.weight < end.distance) {
                end.distance = min.distance + edge.weight
                heap.push(end, edge.distance)
              }
            }
            seen.add(min)
          }
          seen
        }
       """

    checkVars(code, Set("start", "Heap", "Set"))
  }

  private def checkEval(eval: Eval, vars: Map[String, core.Type] = Map()): core.Type = {
    val typer = new BaseFrontend(new LanguageMetaInfo()) {}
    vars.foreach(vt => typer.bindVar(core.Name(vt._1), new Var.Target {}, vt._2))
    typer.typecheck(eval)
  }
  
  test("test typecheck simple") {


    def check(eval: Eval, expected: core.Type, vars: Map[String, core.Type] = Map()): Unit = {
      val actual = checkEval(eval, vars)
      assert(actual == expected)
    }
    check(Eval(Seq(EvalParam(core.Name("x")), EvalParam(core.Name("y"))), Scala(q"x + y")), TScalaInt, Map("x" -> TInt, "y" -> TInt))
    check(Eval(Seq.empty, Scala(q"Math.PI")), TScalaDouble)
    check(Eval(Seq(EvalParam(core.Name("x")), EvalParam(core.Name("y"))), Scala(q"x == y")), TScalaBoolean, Map("x" -> TBool, "y" -> TBool))
    check(Eval(Seq.empty, Scala(q""" "hello world" """)), TScalaString)
    check(Eval(Seq.empty, Scala(q"{val s: Short = 1; s}")), TScala("Short"))
    check(Eval(Seq.empty, Scala(q"println()")), TUnit)
    check(Eval(Seq(EvalParam(core.Name("s"))), Scala(q"s")), TScalaString, Map("s" -> TString))
  }


  test("test typecheck tuple") {
    val eval = Eval(Seq.empty, Scala(q"(1, 1.0, true)"))
    val typ = checkEval(eval)
    assert(typ == TTuple(Seq(TScalaInt, TScalaDouble, TScalaBoolean)))
  }

  test("test typecheck tuple nested") {
    val code = q"(1, 3.4, (true, 'h'))"
    val eval = Eval(Seq.empty, Scala(code))
    val typ = checkEval(eval)
    assert(typ == TTuple(Seq(TScalaInt, TScalaDouble, TTuple(Seq(TScalaBoolean, TScala("Char"))))))
  }

  test("test typecheck tuple nested with string literals") {
    val code = q"""(42, 6.9, true, ("hello", 2), "world") """
    val eval = Eval(Seq.empty, Scala(code))
    val typ = checkEval(eval)
    assert(typ == TTuple(Seq(TScalaInt, TScalaDouble, TScalaBoolean, TTuple(Seq(TScalaString, TScalaInt)), TScalaString)))
  }

  test("test typecheck subtyping") {
    val code = q"""if(true) 42 else new Object()"""
    val eval = Eval(Seq.empty, Scala(code))
    val typ = checkEval(eval)
    assert(typ == TScala("Any"))
  }

  test("test typecheck list") {
    val code = q"List(inca.analyzedData.Nat.Zero)"
    val eval = Eval(Seq.empty, Scala(code))
    val typ = checkEval(eval)
    assert(typ == TScala("List[inca.analyzedData.Nat.Zero.type]"))
  }

  test("test typecheck extern types") {
    val eval = Eval(Seq.empty, Scala(q"inca.analyzedData.Nat.Zero"))
    val typ = checkEval(eval)
    assert(typ == TScala("inca.analyzedData.Nat.Zero.type"))
  }

  test("test typecheck extern types 2") {
    val code =
      q"""num match {
            case inca.analyzedData.Nat.Zero => inca.analyzedData.Nat.Zero
            case inca.analyzedData.Nat.Succ(n) => n
          }
       """

    val eval = Eval(Seq(EvalParam(core.Name("num"))), Scala(code))
    val typ = checkEval(eval, Map("num" -> TScala("inca.analyzedData.Nat.Nat")))
    assert(typ == TScala("inca.analyzedData.Nat.Nat"))
  }

  test("test typecheck complex") {
    val code =
      q"""
        num match {
          case inca.analyzedData.Nat.Zero => (0, inca.analyzedData.Nat.Zero)
          case inca.analyzedData.Nat.Succ(n) => (1, n)
        }
       """

    val eval = Eval(Seq(EvalParam(core.Name("num"))), Scala(code))
    val typ = checkEval(eval, Map("num" -> TScala("inca.analyzedData.Nat.Nat")))
    assert(typ == TTuple(Seq(TScalaInt, TScala("inca.analyzedData.Nat.Nat"))))
  }

  private def checkVars(code: Tree, expectedFree: Set[String]): Unit = {
    val free = EvalHelper.freeVars(code).map(_.name)
    assert(free == expectedFree)
  }

}
