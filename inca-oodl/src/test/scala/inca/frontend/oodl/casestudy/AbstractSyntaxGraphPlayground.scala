package inca.frontend.oodl.casestudy

import inca.frontend.oodl.executor.OODLExecutor
import inca.ir.*
import inca.ir.execution.{Relation1, Relation2, Relation3, Relation4, UnitRelation}
import inca.ir.extension.*
import inca.ir.extension.arithmetic.*
import inca.ir.extension.data.*
import inca.ir.extension.demand.*
import inca.ir.extension.string.*
import inca.ir.typing.{DependencyGraph, IRTypechecker}
import inca.ir.util.SourceLocation
import inca.util.compileroptions.CompilerOptions
import inca.viatra.runtime.EnginePool
import org.scalatest.Ignore
import org.scalatest.funsuite.AnyFunSuiteLike

import scala.language.implicitConversions

@Ignore
class AbstractSyntaxGraphPlayground extends AnyFunSuiteLike:

  val TDefList = TAny
  val TDef = TAny
  val TExp = TAny

  def v(s: String) = Var(s)

  implicit def embed[A](a: A): Seq[A] = Seq(a)

  val defList = ExtensionalRelation("_defList", Seq(Param("list", TDefList)))
  val nils = ExtensionalRelation("_nil", Seq(Param("list", TDefList)))
  val cons = ExtensionalRelation("_con", Seq(Param("list", TDefList), Param("hd", TDef), Param("tail", TDefList)))
  val defs = ExtensionalRelation("_def", Seq(Param("def", TDef), Param("name", TString), Param("exp", TExp)))
  val nums = ExtensionalRelation("_num", Seq(Param("exp", TExp), Param("value", TInt)))
  val vars = ExtensionalRelation("_var", Seq(Param("exp", TExp), Param("name", TString)))
  val adds = ExtensionalRelation("_add", Seq(Param("exp", TExp), Param("lhs", TExp), Param("rhs", TExp)))

  /** Manual demand */

  /*val edgesDefs = Relation("edgesDefs",
    Seq(
      Param("defs", TDefList), // Deflist
      Param("from", TDef), // Def
      Param("to", TDef) // Def
    ),
    Seq(
      Body(Seq(
        Call("edgesDefs$input", Seq(v("defs"))),
        ExtensionalCall("_con", Seq(v("defs"), v("hd"), v("tl"))),
        Call("edgesDef", Seq(v("defs"), v("hd"), v("from"), v("to")))
      )),
      Body(Seq(
        Call("edgesDefs$input", Seq(v("defs"))),
        ExtensionalCall("_con", Seq(v("defs"), v("hd"), v("tl"))),
        Call("edgesDefs", Seq(v("tl"), v("from"), v("to")))
      ))
    )
  )
  val edgesDef = Relation("edgesDef",
    Seq(
      Param("defs", TDefList), // DefList
      Param("def", TDef), // Def
      Param("from", TDef), // Def
      Param("to", TDef) // Def
    ),
    Seq(
      Body(Seq(
        Call("edgesDef$input", Seq(v("defs"), v("def"))),
        ExtensionalCall("_def", Seq(v("def"), WildcardArg(), v("e"))),
        Call("target", Seq(v("defs"), v("e"), v("to"))),
        Eq(v("from"), v("def"))
      )),
      Body(Seq(
        Call("edgesDef$input", Seq(v("defs"), v("def"))),
        ExtensionalCall("_def", Seq(v("def"), WildcardArg(), v("e"))),
        Call("target", Seq(v("defs"), v("e"), v("trg"))),
        Call("edgesDef", Seq(v("defs"), v("trg"), v("from"), v("to")))
      ))
    )
  )

  val target = Relation("target",
    Seq(
      Param("defs", TDefList), // DefList
      Param("e", TExp), // Exp
      Param("def", TDef) // Def
    ),
    Seq(
      Body(Seq(
        Call("target$input", Seq(v("defs"), v("e"))),
        ExtensionalCall("_var", Seq(v("e"), v("name"))),
        Call("findDef", Seq(v("defs"), v("name"), v("def")))
      )),
      Body(Seq(
        Call("target$input", Seq(v("defs"), v("e"))),
        ExtensionalCall("_add", Seq(v("e"), v("e1"), v("e2"))),
        Call("target", Seq(v("defs"), v("e1"), v("def")))
      )),
      Body(Seq(
        Call("target$input", Seq(v("defs"), v("e"))),
        ExtensionalCall("_add", Seq(v("e"), v("e1"), v("e2"))),
        Call("target", Seq(v("defs"), v("e2"), v("def")))
      ))
    )
  )

  val findDef = Relation("findDef",
    Seq(
      Param("defs", TDefList),
      Param("name", TString),
      Param("def", TDef)
    ),
    Seq(
      Body(Seq(
        Call("findDef$input", Seq(v("defs"), v("name"))),
        ExtensionalCall("_con", Seq(v("defs"), v("hd"), v("tl"))),
        ExtensionalCall("_def", Seq(v("hd"), v("defname"), WildcardArg())),
        Eq(v("defname"), v("name")),
        Eq(v("def"), v("hd"))
      )),
      Body(Seq(
        //Call("findDef$input", Seq(v("defs"), v("name"))),
        ExtensionalCall("_con", Seq(v("defs"), v("hd"), v("tl"))),
        ExtensionalCall("_def", Seq(v("hd"), v("defname"), WildcardArg())),
        Eq(v("defname"), v("name"), neg = true),
        Call("findDef", Seq(v("tl"), v("name"), v("def")))
      ))
    )
  )
  */

  /** Group values */

  val collectEdgesDefs = Relation("collect$edgesDefs",
    Seq(
      Param("token", TDemand(TDefList)),
      Param("from", TDemand(TDef)),
      Param("to", TDemand(TDef)),
    ),
    Seq(
      Body(Seq())
    )
  )

  val edgesDefs = Relation("edgesDefs",
    Seq(
      Param("token", TDemand(TDefList)), // Deflist
      Param("defs", TDemand(TDefList)), // Deflist
    ),
    Seq(
      Body(Seq(
        ExtensionalCall("_con", Seq(v("defs"), v("hd"), v("tl"))),
        Call("edgesDef", Seq(v("defs"), v("hd"), v("defs"), v("hd"))),
        Call("collect$edgesDef", Seq(v("defs"), v("hd"), v("from"), v("to"))).addHint(DemandIgnoreCallHint),
        Call("collect$edgesDefs", Seq(v("token"), v("from"), v("to")))
      )),
      Body(Seq(
        ExtensionalCall("_con", Seq(v("defs"), v("hd"), v("tl"))),
        Call("edgesDefs", Seq(v("token"), v("tl")))
      ))
    )
  )

  val collectEdgesDef = Relation("collect$edgesDef",
    Seq(
      Param("token$1", TDemand(TDefList)),
      Param("token$2", TDemand(TDef)),
      Param("from", TDemand(TDef)),
      Param("to", TDemand(TDef)),
    ),
    Seq(
      Body(Seq())
    )
  )

  val edgesDef = Relation("edgesDef",
    Seq(
      Param("token$1", TDemand(TDefList)),
      Param("token$2", TDemand(TDef)),
      Param("defs", TDemand(TDefList)),
      Param("def", TDemand(TDef))
    ),
    Seq(
      Body(Seq(
        ExtensionalCall("_def", Seq(v("def"), WildcardArg(), v("e"))),
        Call("target", Seq(v("defs"), v("e"), v("to"))),
        Eq(v("from"), v("def")),
        Call("collect$edgesDef", Seq(v("token$1"), v("token$2"), v("from"), v("to")))
      )),
      Body(Seq(
        ExtensionalCall("_def", Seq(v("def"), WildcardArg(), v("e"))),
        Call("target", Seq(v("defs"), v("e"), v("trg"))),
        Call("edgesDef", Seq(v("token$1"), v("token$2"), v("defs"), v("trg"))),
      ))
    )
  )

  val target = Relation("target",
    Seq(
      Param("defs", TDemand(TDefList)), // DefList
      Param("e", TDemand(TExp)), // Exp
      Param("def", TDef) // Def
    ),
    Seq(
      Body(Seq(
        ExtensionalCall("_var", Seq(v("e"), v("name"))),
        Call("findDef", Seq(v("defs"), v("name"), v("def")))
      )),
      Body(Seq(
        ExtensionalCall("_add", Seq(v("e"), v("e1"), v("e2"))),
        Call("target", Seq(v("defs"), v("e1"), v("def")))
      )),
      Body(Seq(
        ExtensionalCall("_add", Seq(v("e"), v("e1"), v("e2"))),
        Call("target", Seq(v("defs"), v("e2"), v("def")))
      ))
    )
  )

  val findDef = Relation("findDef",
    Seq(
      Param("defs", TDemand(TDefList)), // DefList
      Param("name", TDemand(TString)),
      Param("def", TDef) // Def
    ),
    Seq(
      Body(Seq(
        ExtensionalCall("_con", Seq(v("defs"), v("hd"), v("tl"))),
        ExtensionalCall("_def", Seq(v("hd"), v("defname"), WildcardArg())),
        Eq(v("defname"), v("name")),
        Eq(v("def"), v("hd"))
      )),
      Body(Seq(
        ExtensionalCall("_con", Seq(v("defs"), v("hd"), v("tl"))),
        ExtensionalCall("_def", Seq(v("hd"), v("defname"), WildcardArg())),
        Eq(v("defname"), v("name"), neg = true),
        Call("findDef", Seq(v("tl"), v("name"), v("def")))
      ))
    )
  )


  val main = Relation("main",
    Seq(
      Param("from", TDef), // Def
      Param("to", TDef) // Def
    ),
    Seq(
      Body(Seq(
        ExtensionalCall("_defList", Seq(v("defs"))),
        Eq(v("token"), v("defs")),
        Call("edgesDefs", Seq(v("token"), v("defs"))),
        Call("collect$edgesDefs", Seq(v("token"), v("from"), v("to"))).addHint(DemandIgnoreCallHint)
      ))
    )
  )

  /*val edgesDefsInput = Relation("edgesDefs$input",
    Seq(
      Param("defs$0", TDefList)
  ), Seq(
    Body(Seq(
      Call("edgesDefs$input", Seq(v("defs"))),
      ExtensionalCall("_con", Seq(v("defs"), v("hd"), v("tl"))),
      Eq(v("defs$0"), v("tl"))
    )),
    Body(Seq(
      ExtensionalCall("_defList", Seq(v("defs"))),
      Eq(v("defs$0"), v("defs"))
    ))
  ))

  val edgesDefInput = Relation("edgesDef$input",
    Seq(
      Param("defs$0", TDefList),
      Param("def$0", TDef),
    ), Seq(
      Body(Seq(
        Call("edgesDefs$input", Seq(v("defs"))),
        ExtensionalCall("_con", Seq(v("defs"), v("hd"), v("tl"))),
        Eq(v("defs$0"), v("defs")),
        Eq(v("def$0"), v("hd"))
      )),
      Body(Seq(
        Call("edgesDef$input", Seq(v("defs"), v("def"))),
        ExtensionalCall("_def", Seq(v("def"), WildcardArg(), v("e"))),
        Call("target", Seq(v("defs"), v("e"), v("trg"))),
        Eq(v("defs$0"), v("defs")),
        Eq(v("def$0"), v("trg"))
      ))
    ))

  val targetInput = Relation("target$input",
    Seq(
      Param("defs$0", TDefList),
      Param("e$0", TExp),
    ), Seq(
      Body(Seq(
        Call("edgesDef$input", Seq(v("defs"), v("def"))),
        ExtensionalCall("_def", Seq(v("def"), WildcardArg(), v("e"))),
        Eq(v("defs$0"), v("defs")),
        Eq(v("e$0"), v("e"))
      )),
      Body(Seq(
        Call("target$input", Seq(v("defs"), v("e"))),
        ExtensionalCall("_add", Seq(v("e"), v("e1"), v("e2"))),
        Eq(v("defs$0"), v("defs")),
        Eq(v("e$0"), v("e1"))
      )),
      Body(Seq(
        Call("target$input", Seq(v("defs"), v("e"))),
        ExtensionalCall("_add", Seq(v("e"), v("e1"), v("e2"))),
        Eq(v("defs$0"), v("defs")),
        Eq(v("e$0"), v("e2"))
      )),
    ))

  val findDefInput = Relation("findDef$input",
    Seq(
      Param("defs$0", TDefList),
      Param("name$0", TString),
    ), Seq(
      Body(Seq(
        Call("target$input", Seq(v("defs"), v("e"))),
        ExtensionalCall("_var", Seq(v("e"), v("name"))),
        Eq(v("defs$0"), v("defs")),
        Eq(v("name$0"), v("name"))
      )),
      Body(Seq(
        Call("findDef$input", Seq(v("defs"), v("name"))),
        ExtensionalCall("_con", Seq(v("defs"), v("hd"), v("tl"))),
        ExtensionalCall("_def", Seq(v("hd"), v("defname"), WildcardArg())),
        Eq(v("defname"), v("name"), true),
        Eq(v("defs$0"), v("tl")),
        Eq(v("name$0"), v("name"))
      )),
    ))*/

  /** Inlined variables */

  /*val edgesDefsInput = Relation("edgesDefs$input",
    Seq(
      Param("defs$0", TDefList)
    ), Seq(
      Body(Seq(
        Call("edgesDefs$input", Seq(v("defs"))),
        ExtensionalCall("_con", Seq(v("defs"), v("hd"), v("defs$0")))
      )),
      Body(Seq(
        ExtensionalCall("_defList", Seq(v("defs$0")))
      ))
    ))

  val edgesDefInput = Relation("edgesDef$input",
    Seq(
      Param("defs$0", TDefList),
      Param("def$0", TDef),
    ), Seq(
      Body(Seq(
        Call("edgesDefs$input", Seq(v("defs$0"))),
        ExtensionalCall("_con", Seq(v("defs$0"), v("def$0"), v("tl")))
      )),
      Body(Seq(
        Call("edgesDef$input", Seq(v("defs$0"), v("def"))),
        ExtensionalCall("_def", Seq(v("def"), WildcardArg(), v("e"))),
        Call("target", Seq(v("defs$0"), v("e"), v("def$0")))
      ))
    ))

  val targetInput = Relation("target$input",
    Seq(
      Param("defs$0", TDefList),
      Param("e$0", TExp),
    ), Seq(
      Body(Seq(
        Call("edgesDef$input", Seq(v("defs$0"), v("def"))),
        ExtensionalCall("_def", Seq(v("def"), WildcardArg(), v("e$0")))
      )),
      Body(Seq(
        Call("target$input", Seq(v("defs$0"), v("e"))),
        ExtensionalCall("_add", Seq(v("e"), v("e$0"), v("e2")))
      )),
      Body(Seq(
        Call("target$input", Seq(v("defs$0"), v("e"))),
        ExtensionalCall("_add", Seq(v("e"), v("e1"), v("e$0")))
      )),
    ))

  val findDefInput = Relation("findDef$input",
    Seq(
      Param("defs$0", TDefList),
      Param("name$0", TString),
    ), Seq(
      Body(Seq(
        Call("target$input", Seq(v("defs$0"), v("e"))),
        ExtensionalCall("_var", Seq(v("e"), v("name$0")))
      )),
      Body(Seq(
        Call("findDef$input", Seq(v("defs"), v("name$0"))),
        ExtensionalCall("_con", Seq(v("defs"), v("hd"), v("defs$0"))),
        ExtensionalCall("_def", Seq(v("hd"), v("defname"), WildcardArg())),
        Eq(v("defname"), v("name$0"), true)
      )),
    ))*/


  val mod = Module("AbstractSyntaxGraph", BaseIR.language + arithmetic.IR + data.IR + demand.IR + string.IR,
    Seq(
      // EBD
      defList,
      nils,
      cons,
      defs,
      vars,
      nums,
      adds,
      // IDB
      edgesDefs,
      edgesDef,
      target,
      findDef,
      main,
      collectEdgesDef,
      collectEdgesDefs
      // demand
      //edgesDefsInput,
      //edgesDefInput,
      //targetInput,
      //findDefInput
    )
  )


  /**
   * Data structures
   */

  var nextId: Int = 0
  def freshId(): Int =
    val id = nextId
    nextId += 1
    id

  enum Exp:
    case Num(value: Int)
    case Var(name: String)
    case Add(lhs: Exp, rhs: Exp)

    val id: Int = freshId()

    def args: Seq[Any] = this match
      case Num(value) => Seq(this, value)
      case Var(name) => Seq(this, name)
      case Add(lhs, rhs) => Seq(this, lhs, rhs)

    def collect(filter: (ele: Exp) => Boolean): Seq[Exp] =
      val res = if filter(this) then Seq(this) else Seq()
      this match
        case Add(lhs, rhs) => res ++ lhs.collect(filter) ++ rhs.collect(filter)
        case _ => res

  class Def(val name: String, val exp: Exp):
    def args: Seq[Any] = Seq(this, name, exp)

    override def toString: String =
      s"${this.getClass.getSimpleName}${this.args.tail.mkString("(", ",", ")")}"

    def collectNums(): Seq[Exp] =
      exp.collect {
        case n: Exp.Num => true
        case _ => false
      }

    def collectVars(): Seq[Exp] =
      exp.collect {
        case n: Exp.Var => true
        case _ => false
      }

    def collectAdds(): Seq[Exp] =
      exp.collect {
        case n: Exp.Add => true
        case _ => false
      }

  enum DefList:
    case Nil()
    case Cons(hd: Def, tl: DefList)

    val id: Int = freshId()

    def args: Seq[Any] = this match
      case Nil() => Seq(this)
      case Cons(hd, tl) => Seq(this, hd, tl)

    def concat(d: DefList): DefList = this match
      case Cons(hd, tl) => Cons(hd, tl.concat(d))
      case _ => d

    def collectDefs(): Seq[Def] =
      this match
        case Cons(hd, tl) => Seq(hd) ++ tl.collectDefs()
        case _ => Seq()

    def collect(filter: (ele: DefList) => Boolean): Seq[DefList] =
      val res = if filter(this) then Seq(this) else Seq()
      this match
        case Cons(hd, tl) => res ++ tl.collect(filter)
        case _ => res

    def collectNils(): Seq[DefList] =
      this.collect {
        case d: DefList.Nil => true
        case _ => false
      }

    def collectCons(): Seq[DefList] =
      this.collect {
        case d: DefList.Cons => true
        case _ => false
      }

  /**
   * Helper to generate Programs
   */

  object GenerateProgram {
    def mkDef(i: Int): Def = {
      val n: Int = i + 1
      new Def("a" + i.toString(), Exp.Add(Exp.Var("a" + n.toString()), Exp.Num(i)))
    }

    def line(i: Int, to: Int): DefList = {
      if (i < to) {
        val d: Def = this.mkDef(i)
        val ds: DefList = this.line(i + 1, to)
        DefList.Cons(d, ds)
      } else {
        DefList.Nil()
      }
    }

    def circle(from: Int, to: Int): Def = {
      new Def("a" + to.toString(), Exp.Add(Exp.Var("a" + from.toString()), Exp.Num(to)))
    }

    def prog(from: Int, to: Int, step: Int): DefList = {
      if (from < to) {
        val ds1: DefList = this.line(from, from + step)
        val d: Def = this.circle(from + step, from)
        val ds2: DefList = this.prog(from + step, to, step)
        ds1.concat(DefList.Cons(d, ds2))
      } else {
        DefList.Cons(this.circle(0, to), DefList.Nil())
      }
    }
  }


  def compiled = new CompiledModule:
    override def name: Name = "AbstractSyntaxGraph"
    override def sourceLocation: SourceLocation = SourceLocation.NoSourceLocation
    override def ir: Module = mod
    override def compilerOptions: CompilerOptions = CompilerOptions.fromResource("objectoriented/Options.ini")
    setPipeline(List(() => new demand.Lowering {}))

  test("AbstractSyntaxGraph is well-typed") {
    //println(mod)
    try
      compiled.checked
      //println(compiled.dependencyGraph.toGraphViz)
    //finally println(mod)
  }

  test("AbstractSyntaxGraph can be lowered") {
    try
      compiled.lowered
      val typechecker = new IRTypechecker
      typechecker.checkProgram(Seq(compiled.lowered))
      //println(typechecker.getDependencyGraph.toGraphViz)
    //finally println(compiled.lowered)
  }

  test("AbstractSyntaxGraph can be run") {
    val prog = GenerateProgram.prog(0, 50, 10)

    val defs = prog.collectDefs()

    val edbDefList = Relation1("_defList", Seq("list"), Seq(Seq(prog)))
    val edbNils = Relation1("_nil", Seq("list"), prog.collectNils().map(_.args))
    val edbCons = Relation3("_con", Seq("list", "hd", "tl"), prog.collectCons().map(_.args))
    val edbDefs = Relation3("_def", Seq("def", "name", "exp"), defs.map(_.args))
    val edbNums = Relation2("_num", Seq("exp", "value"), defs.flatMap(_.collectNums()).map(_.args))
    val edbVars = Relation2("_var", Seq("exp", "name"), defs.flatMap(_.collectVars()).map(_.args))
    val edbAdds = Relation3("_add", Seq("exp", "lhs", "rhs"), defs.flatMap(_.collectAdds()).map(_.args))

    val edbs = Seq(edbDefList, edbNils, edbCons, edbDefs, edbNums, edbVars, edbAdds)

    val runs = 1
    val executionTimes = (0 until runs).map { _ =>
      val engine = new inca.viatra.Executor().instantiate(compiled)
      edbs.foreach(engine.insert)
      //edbs.foreach(e => println(e.asTable))
      val start = System.nanoTime()
      val relation1 = engine.read(Relation2("main", Seq("from", "to"), Seq()))
//      val relation2 = engine.read(Relation4("makeProg", Seq("from", "to", "step", "defs"), Seq()))
      val end = System.nanoTime()
      //println(relation1.asTable)
      val executionTimeInMs = (end - start) / 1000 / 1000

      /*println(s"Number of tuples: ${engine.readAll().map(_.size).sum}")
      engine.readAll().foreach { r =>
        println(s"${r.name}: ${r.size}")
      }
      println(engine.read(UnitRelation("collect$edgesDefs")).asTable)*/

      executionTimeInMs
    }
    //println(s"Execution times in ms: $executionTimes")
    //println(s"Execution average: ${executionTimes.drop(5).sum / (runs - 5)}")
  }
  