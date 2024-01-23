package inca.frontend.oodl.casestudy

import inca.frontend.oodl.executor.OODLExecutor
import inca.ir.*
import inca.ir.execution.{Relation2, Relation4}
import inca.ir.extension.*
import inca.ir.extension.arithmetic.*
import inca.ir.extension.data.*
import inca.ir.extension.demand.*
import inca.ir.extension.string.*
import inca.ir.util.SourceLocation
import inca.util.compileroptions.CompilerOptions
import inca.viatra.runtime.EnginePool
import org.scalatest.funsuite.AnyFunSuiteLike

import scala.language.implicitConversions

class AbstractSyntaxGraph extends AnyFunSuiteLike:


  def t(s: String) = TData(s)
  def v(s: String) = Var(s)

  implicit def embed[A](a: A): Seq[A] = Seq(a)

  val datas = Seq(
    DataDefinition("TProg"),
    CaseDefinition("Prog", t("TDefList"), t("TProg")),

    DataDefinition("TDefList"),
    CaseDefinition("Nil", Seq(), t("TDefList")),
    CaseDefinition("Cons", Seq(t("TDef"), t("TDefList")), t("TDefList")),

    DataDefinition("TDef"),
    CaseDefinition("Def", Seq(TString, t("TExp")), t("TDef")),

    DataDefinition("TExp"),
    CaseDefinition("Num", TInt, t("TExp")),
    CaseDefinition("Var", TString, t("TExp")),
    CaseDefinition("Add", Seq(t("TExp"), t("TExp")), t("TExp")),
  )

  val edgesDefs = Relation("edgesDefs",
    Seq(
      Param("defs", TDemand(t("TDefList"))),
      Param("from", t("TDef")),
      Param("to", t("TDef"))
    ),
    Seq(
      Body(Seq(
        Deconstruct(v("defs"), "Cons", Seq(v("hd"), v("tl"))),
        Call("edgesDef", Seq(v("defs"), v("hd"), v("from"), v("to")))
      )),
      Body(Seq(
        Deconstruct(v("defs"), "Cons", Seq(v("hd"), v("tl"))),
        Call("edgesDefs", Seq(v("tl"), v("from"), v("to")))
      ))
    )
  )
  val edgesDef = Relation("edgesDef",
    Seq(
      Param("defs", TDemand(t("TDefList"))),
      Param("def", TDemand(t("TDef"))),
      Param("from", t("TDef")),
      Param("to", t("TDef"))
    ),
    Seq(
      Body(Seq(
        Deconstruct(v("def"), "Def", Seq(WildcardArg(), v("e"))),
        Call("target", Seq(v("defs"), v("e"), v("to"))),
        Eq(v("from"), v("def"))
      )),
      Body(Seq(
        Deconstruct(v("def"), "Def", Seq(WildcardArg(), v("e"))),
        Call("target", Seq(v("defs"), v("e"), v("trg"))),
        Call("edgesDef", Seq(v("defs"), v("trg"), v("from"), v("to")))
      ))
    )
  )

  val target = Relation("target",
    Seq(
      Param("defs", TDemand(t("TDefList"))),
      Param("e", TDemand(t("TExp"))),
      Param("def", t("TDef"))
    ),
    Seq(
      Body(Seq(
        Deconstruct(v("e"), "Var", Seq(v("name"))),
        Call("findDef", Seq(v("defs"), v("name"), v("def")))
      )),
      Body(Seq(
        Deconstruct(v("e"), "Add", Seq(v("e1"), v("e2"))),
        Call("target", Seq(v("defs"), v("e1"), v("def")))
      )),
      Body(Seq(
        Deconstruct(v("e"), "Add", Seq(v("e1"), v("e2"))),
        Call("target", Seq(v("defs"), v("e2"), v("def")))
      ))
    )
  )

  val findDef = Relation("findDef",
    Seq(
      Param("defs", TDemand(t("TDefList"))),
      Param("name", TDemand(TString)),
      Param("def", t("TDef"))
    ),
    Seq(
      Body(Seq(
        Deconstruct(v("defs"), "Cons", Seq(v("hd"), v("tl"))),
        Deconstruct(v("hd"), "Def", Seq(v("defname"), WildcardArg())),
        Eq(v("defname"), v("name")),
        Eq(v("def"), v("hd"))
      )),
      Body(Seq(
        Deconstruct(v("defs"), "Cons", Seq(v("hd"), v("tl"))),
        Deconstruct(v("hd"), "Def", Seq(v("defname"), WildcardArg())),
        Eq(v("defname"), v("name"), neg = true),
        Call("findDef", Seq(v("tl"), v("name"), v("def")))
      ))
    )
  )

  val makeProg = Relation("makeProg",
    Seq(
      Param("from", TDemand(TInt)),
      Param("to", TDemand(TInt)),
      Param("step", TDemand(TInt)),
      Param("defs", t("TDefList"))
    ),
    Seq(
      Body(Seq(
        LT(v("from"), v("to")),
        Eq(v("next"), Add(v("from"), v("step"))),
        Call("makeLine", Seq(v("from"), v("next"), v("line"))),
        Eq(v("circle"), Construct("Def", Seq(
          StringConcat(StringLit("a"), ToString(v("from"))),
          Construct("Add", Seq(
            Construct("Var", StringConcat(StringLit("a"), ToString(v("next")))),
            Construct("Num", v("from"))
          ))
        ))),
        Call("makeProg", Seq(v("next"), v("to"), v("step"), v("rec"))),
        Call("concat", Seq(
          v("line"),
          Construct("Cons", Seq(v("circle"), v("rec"))),
          v("defs")
        ))
      )),
      Body(Seq(
        GE(v("from"), v("to")),
        Eq(v("circle"), Construct("Def", Seq(
          StringConcat(StringLit("a"), ToString(v("from"))),
          Construct("Add", Seq(
            Construct("Var", StringConcat(StringLit("a"), ToString(IntNum(0)))),
            Construct("Num", v("from"))
          ))
        ))),
        Eq(v("defs"), Construct("Cons", Seq(v("circle"), Construct("Nil", Seq()))))
      ))
    )
  )

  val makeLine = Relation("makeLine",
    Seq(
      Param("i", TDemand(TInt)),
      Param("to", TDemand(TInt)),
      Param("defs", t("TDefList"))
    ),
    Seq(
      Body(Seq(
        LT(v("i"), v("to")),
        Eq(v("n"), Add(v("i"), IntNum(1))),
        Eq(v("d"), Construct("Def", Seq(
          StringConcat(StringLit("a"), ToString(v("i"))),
          Construct("Add", Seq(
            Construct("Var", StringConcat(StringLit("a"), ToString(v("n")))),
            Construct("Num", v("i"))
          ))
        ))),
        Call("makeLine", Seq(v("n"), v("to"), v("ds"))),
        Eq(v("defs"), Construct("Cons", Seq(v("d"), v("ds"))))
      )),
      Body(Seq(
        GE(v("i"), v("to")),
        Eq(v("defs"), Construct("Nil", Seq()))
      ))
    )
  )

  val concat = Relation("concat",
    Seq(
      Param("l1", TDemand(t("TDefList"))),
      Param("l2", TDemand(t("TDefList"))),
      Param("res", t("TDefList"))
    ),
    Seq(
      Body(Seq(
        Deconstruct(v("l1"), "Nil", Seq()),
        Eq(v("res"), v("l2"))
      )),
      Body(Seq(
        Deconstruct(v("l1"), "Cons", Seq(v("hd"), v("tl"))),
        Call("concat", Seq(v("tl"), v("l2"), v("tmp"))),
        Eq(v("res"), Construct("Cons", Seq(v("hd"), v("tmp"))))
      ))
    )
  )

  val main = Relation("main",
    Seq(
      Param("from", t("TDef")),
      Param("to", t("TDef"))
    ),
    Seq(
      Body(Seq(
        Eq(v("endNode"), IntNum(50)),
        Eq(v("step"), IntNum(10)),
        Call("makeProg", Seq(IntNum(0), v("endNode"), v("step"), v("defs"))),
        Call("edgesDefs", Seq(v("defs"), v("from"), v("to")))
      ))
    )
  )


  val mod = Module("AbstractSyntaxGraph", BaseIR.language + arithmetic.IR + data.IR + demand.IR + string.IR,
    datas ++
    Seq(
      edgesDefs,
      edgesDef,
      target,
      findDef,
      makeProg,
      makeLine,
      concat,
      main
    )
  )

  def compiled = new CompiledModule:
    override def name: Name = "AbstractSyntaxGraph"
    override def sourceLocation: SourceLocation = SourceLocation.NoSourceLocation
    override def ir: Module = mod
    override def compilerOptions: CompilerOptions = CompilerOptions.fromResource("objectoriented/Options.ini")
    setPipeline(List(() => new demand.Lowering {}))

  test("AbstractSyntaxGraph is well-typed") {
    println(mod)
    try compiled.checked
    finally println(mod)
  }

  test("AbstractSyntaxGraph can be lowered") {
    try compiled.lowered
    finally println(compiled.lowered)
  }

  test("AbstractSyntaxGraph can be run") {
    for (i <- 0 until 1) {
      val engine = new inca.viatra.Executor().instantiate(compiled)
      val start = System.nanoTime()
      val relation1 = engine.read(Relation2("main", Seq("from", "to"), Seq()))
//      val relation2 = engine.read(Relation4("makeProg", Seq("from", "to", "step", "defs"), Seq()))
      val end = System.nanoTime()

      //engine.readAll().foreach(r => println(r.asTable))
      println(s"Execution time ${(end - start) / 1000 / 1000}ms")
      println(s"Number of tuples: ${engine.readAll().map(_.size).sum}")
      engine.readAll().foreach { r =>
        println(s"${r.name}: ${r.size}")
      }
      println(relation1.asTable)
    }
  }

  