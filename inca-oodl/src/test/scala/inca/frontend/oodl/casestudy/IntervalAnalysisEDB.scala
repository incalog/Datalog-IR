package inca.frontend.oodl.casestudy

import inca.frontend.oodl.executor.OODLExecutor
import inca.frontend.oodl.syntax.TTuple
import inca.ir.*
import inca.ir.execution.{Relation1, Relation2, Relation3, Relation4}
import inca.ir.extension.*
import inca.ir.extension.arithmetic.*
import inca.ir.extension.data.*
import inca.ir.extension.demand.*
import inca.ir.extension.mono.ArithmeticMonoDefinition.SumInt
import inca.ir.extension.mono.{MapMonoDefinition, TMono}
import inca.ir.extension.string.*
import inca.ir.extension.tuple.{Project, TupleLit}
import inca.ir.typing.{DependencyGraph, IRTypechecker}
import inca.ir.util.SourceLocation
import inca.util.compileroptions.CompilerOptions
import inca.viatra.runtime.EnginePool
import org.scalatest.Ignore
import org.scalatest.funsuite.AnyFunSuiteLike

import scala.language.implicitConversions
import inca.foreign.scala.ir.primitive.{ConversionElimination, ForeignScalaLowering, ScalaMonoDefinition, ScalaType}
import inca.ir.extension.map.TMap


class IntervalAnalysisEDB extends AnyFunSuiteLike:

  val TStmt = TAny
  val TExp = TAny
  val TInterval = TTuple(Seq(TInt, TInt))
  
  val intervalMono = ScalaMonoDefinition(
    "IntervalMono",
    initCode = "(0, 0)",
    addCode = "(st: (Int, Int), a: (Int, Int)) => (min(st._1, a._1), max(st._2, a._2))",
    resultCode = "(st: (Int, Int)) => st",
    constructorParamTypes = Seq(),
    typ = MonoTypes(ScalaType("(Int, Int)"), ScalaType("(Int, Int)"), ScalaType("(Int, Int)"))
  )
  val mapMono = MapMonoDefinition(TStmt, MapMonoDefinition(TString, intervalMono))
  val TMonoMap = TMono(TTuple(TStmt, TTuple(TString, TInterval)), TMap(TStmt, TMap(TString, TInterval)), Seq())


  def t(s: String) = TData(s)
  def v(s: String) = Var(s)

  implicit def embed[A](a: A): Seq[A] = Seq(a)

  private def makeTp[T](K: Seq[T] => T, ts: T*): T =
    if ts.size == 1 then ts.head
    else if ts.size == 2 then K(ts.toSeq)
    else K(Seq(ts.head, makeTp(K, ts.tail: _*)))
  def pmatch(ty: String, args: Term*) = ExtensionalCall(s"_$ty", args)
  def mkIv(l: Term, r: Term): Term = TupleLit(l, r)

  val cflow = ExtensionalRelation("cflow", Seq(Param("from", TStmt), Param("to", TStmt)))
  val addIv = Relation("addIv", Seq(
    Param("iv1", TDemand(TInterval)),
    Param("iv2", TDemand(TInterval))
  ), Seq(
    Body(
      mkIv(
        Min(
          Project(Var("iv1"), 0),
          Project(Var("iv2"), 0)
        ),
        Max(
          Project(Var("iv1"), 1),
          Project(Var("iv2"), 1)
        )
      )
    )
  ))
  val aeval = Relation("aeval",
    Seq(
      Param("m", TDemand(TMonoMap)),
      Param("exp", TDemand(TExp)),
      Param("iv", TInterval)
    ),
    Seq(
      Body(Seq(
        pmatch("Var", Var("exp"), Var("v")),
        Eq(Var("iv"), mono.ReadMono(Var("m")))
      )),
      Body(Seq(
        pmatch("Num", Var("exp"), Var("n")),
        Eq(Var("iv"), mkIv(n, n))
      )),
      Body(Seq(
        pmatch("Add", Var("exp"), Var("lhs"), Var("rhs")),
        Call("aeval", Seq(Var("m"), Var("iv1"))),
        Call("aeval", Seq(Var("m"), Var("iv2"))),
        Eq(Var("iv"), Call("addIv", Seq(Var("iv1"), Var("iv2"))))
      ))
    )
  )

  val interval = Relation("interval", Seq(
    Param("stmt", TDemand(TStmt)),
    Param("before", TDemand(TMonoMap)),
    Param("after", TDemand(TMonoMap)),
  ), Seq(
    Body(
      pmatch("Assign", Var("stmt"), Var("v"), Var("exp")),
      Call("aeval", Seq(Var("exp"), mono.ReadMono(Var("before")), Var("iv"))),
      mono.WriteMono(TTuple(Var("stmt"), TTuple(Var("v"), Var("iv"))))
    )
  ))

  val main = Relation("main",
    Seq(
      Param("out", TMonoMap)
    ),
    Seq(
      Body(Seq(
        pmatch("Stmt", Seq(Var("s"))),
        Eq(Var("before"), mono.NewMono(mapMono)),
        Eq(Var("after"), mono.NewMono(mapMono))
        Call("interval", Seq(v("s"), v("before"), v("after")))
      ))
    )
  )


  val mod = Module("AbstractSyntaxGraph", BaseIR.language + arithmetic.IR + data.IR + demand.IR + string.IR,
    Seq(
      cflow,
      addIv,
      aeval,
      interval,
      main
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

  enum Stmt:
    case Assign(name: String, exp: Exp)

    val id: Int = freshId()

    def args: Seq[Any] = this match
      case Assign(name, exp) => Seq(this, name, exp)

    def collect(filter: (ele: Exp) => Boolean): Seq[Exp] =
      val res = if filter(this) then Seq(this) else Seq()
      this match
        case Assign(lhs, rhs) => res ++ lhs.collect(filter) ++ rhs.collect(filter)
        case _ => res        
  
  /**
   * Helper to generate Programs
   */

  object GenerateProgram {
    
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
      /*println(relation1.asTable)
      println(s"Number of tuples: ${engine.readAll().map(_.size).sum}")
      engine.readAll().foreach { r =>
        println(s"${r.name}: ${r.size}")
      }

      println(engine.read(Relation1("edgesDef", Seq("deflist"), Seq(Seq(prog)))).size)*/

      val executionTimeInMs = (end - start) / 1000 / 1000
      executionTimeInMs
    }
    //println(s"Execution times in ms: $executionTimes")
    //println(s"Execution average: ${executionTimes.drop(5).sum / (runs - 5)}")
  }
  