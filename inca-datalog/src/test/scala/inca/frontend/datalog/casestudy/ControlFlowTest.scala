package inca.frontend.datalog.casestudy

import inca.frontend.datalog.compile.DatalogCompilerOptions
import inca.frontend.datalog.executor.DatalogExecutor
import inca.frontend.datalog.executor.DatalogExecutor.?
import inca.ir.execution.{Relation, Relation1, Relation2, RelationUpdateListener}
import inca.util.FileUtil
import org.eclipse.viatra.query.runtime.rete.matcher.DRedReteBackendFactory
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.mutable.ListBuffer

class ControlFlowTest extends AnyFunSuite:
  val pipeline = List()
  val options = DatalogCompilerOptions.fromResource("datalog/Options.ini")
  options.irLogging.logModule = false
  val exec: DatalogExecutor = new DatalogExecutor(new inca.viatra.Executor(DRedReteBackendFactory.INSTANCE))


  var nextId: Int = 0

  case class Function(name: String, body: Stmt):
    def toEdbRelations: Seq[Relation] =
      // edb functionBody(String, Any).
      Relation2("functionBody", Seq("f", "b"), firstNonBlock(body).map(b => Seq(name, b)).toSeq)
      +: stmtToEdbRelations(body)

    override def toString: String = s"function $name = $body"

  import Stmt.*
  enum Stmt:
    case Simple(lab: String)
    case Call(name: String)
    case While(body: Stmt)
    case Block(list: List[Stmt])

    val id: Int = nextId
    nextId += 1

    override def toString: String = this match
      case Simple(lab) => lab
      case Call(name) => s"$name()"
      case While(b) => s"While($b)"
      case Block(ss) => ss.mkString("; ")

    override def equals(obj: Any): Boolean = obj match
      case that: Stmt => this.id == that.id
      case _ => false
    override def hashCode(): Int = id

    def foreach(f: Stmt => Unit): Unit =
      f(this)
      this match
        case Simple(_) | Call(_) => // nothing
        case While(body) => body.foreach(f)
        case Block(list) => list.foreach(_.foreach(f))

  def firstNonBlock(s: Stmt): Option[Stmt] = s match
    case _: (Simple | Call | While) => Some(s)
    case Block(ss) => ss.headOption.flatMap(firstNonBlock)

  def lastNonBlock(s: Stmt): Option[Stmt] = s match
    case _: (Simple | Call | While) => Some(s)
    case Block(ss) => ss.lastOption.flatMap(lastNonBlock)

  def stmtToEdbRelations(s: Stmt): Seq[Relation] =
    val simples = ListBuffer.empty[Stmt.Simple]
    val calls = ListBuffer.empty[Stmt.Call]
    val whiles = ListBuffer.empty[Stmt.While]
    val blocks = ListBuffer.empty[Stmt.Block]
    def collect(s: Stmt) = s match
      case s: Stmt.Simple => simples += s
      case s: Stmt.Call => calls += s
      case s: Stmt.While => whiles += s
      case s: Stmt.Block => blocks += s
    s.foreach(collect)

    // edb simpleStmt(Int).
    val edbSimple = Relation1("simple", Seq("s"), simples.toSeq.map(s => Seq(s)))

    // edb callName(Any, String).
    val edbCall = Relation2("call", Seq("s", "n"), calls.toSeq.map(s => Seq(s, s.name)))

    //edb whileBody(Int, Int).
    val edbWhile = Relation2("while", Seq("w", "b"), whiles.toSeq.flatMap(s =>
      firstNonBlock(s.body).map(b => Seq(s, b))
    ))

    //edb next(Int, Int).
    val edbNext = Relation2("next", Seq("s1", "s2"), blocks.flatMap { block =>
      block.list.zip(block.list.tail).flatMap((pred, succ) =>
        for (from <- lastNonBlock(pred); to <- firstNonBlock(succ)) yield
          Seq(from, to))
    })


    Seq(edbSimple, edbCall, edbWhile, edbWhile, edbNext)


  test("control flow seq") {
    val code = FileUtil.readFileFromResource("datalog/casestudy/controlFlow.dl")
    val compiled = exec.compileDatalog(code, options)
    compiled.setPipeline(pipeline)
    val loaded = exec.loadDatalog(compiled)

    val s = Block(List(Simple("a"), Simple("b"), Simple("c"), Simple("d")))
    val edbs = stmtToEdbRelations(s)
//    edbs.foreach(t => println(t.asTable))
    edbs.foreach(loaded.engine.insert)

    val stmt = loaded.query("stmt")
    println(stmt.asTable)

    val cflow = loaded.query("cflow")
    println(cflow.asTable)
  }

  test("control flow while flat") {
    val code = FileUtil.readFileFromResource("datalog/casestudy/controlFlow.dl")
    val compiled = exec.compileDatalog(code, options)
    compiled.setPipeline(pipeline)
    val loaded = exec.loadDatalog(compiled)

    val s = Block(List(Simple("a"), While(Simple("b")), Simple("d"), Simple("e")))
    val edbs = stmtToEdbRelations(s)
//    edbs.foreach(t => println(t.asTable))
    edbs.foreach(loaded.engine.insert)

    println(s)
    val cflow = loaded.query("cflow")
    println(cflow.asTable)
  }

  test("control flow while deep") {
    val code = FileUtil.readFileFromResource("datalog/casestudy/controlFlow.dl")
    val compiled = exec.compileDatalog(code, options)
    compiled.setPipeline(pipeline)
    val loaded = exec.loadDatalog(compiled)

    val s = Block(List(Simple("a"), While(Block(List(Simple("b"), Simple("c")))), Simple("d"), Simple("e")))
    val edbs = stmtToEdbRelations(s)
//    edbs.foreach(t => println(t.asTable))
    edbs.foreach(loaded.engine.insert)

    println(s)
//    loaded.engine.readAll().map(_.asTable).foreach(println)
    val cflow = loaded.query("cflow")
    println(cflow.asTable)
  }

  test("control flow large") {
    val code = FileUtil.readFileFromResource("datalog/casestudy/controlFlow.dl")
    val compiled = exec.compileDatalog(code, options)
    compiled.setPipeline(pipeline)
    val loaded = exec.loadDatalog(compiled)

    val ss = for (i <- 0 until 10) yield
      Block(List(Simple(s"a$i"), While(Block(List(Simple(s"b$i"), Simple(s"c$i")))), Simple(s"d$i"), Simple(s"e$i")))
    val s = Block(ss.toList)
    val edbs = stmtToEdbRelations(s)
    //    edbs.foreach(t => println(t.asTable))
    edbs.foreach(loaded.engine.insert)

    println(s)
    //    loaded.engine.readAll().map(_.asTable).foreach(println)
    val cflow = loaded.query("cflow")
    println(cflow.asTable)

    val reachable = loaded.query("reachable")
    println(s"Computed ${reachable.size} reachable entries")
    val deadCode = loaded.query("deadCode")
    println(s"Found ${deadCode.size} dead-code statements")
  }

  test("control flow functions") {
    val code = FileUtil.readFileFromResource("datalog/casestudy/controlFlow.dl")
    val compiled = exec.compileDatalog(code, options)
    compiled.setPipeline(pipeline)
    val loaded = exec.loadDatalog(compiled)

    val funs = for (i <- 0 until 10) yield
      Function(s"fun$i", Block(List(Simple(s"a$i"), While(Block(List(Simple(s"b$i"), Simple(s"c$i")))), Simple(s"d$i"), Simple(s"e$i"))))
    val main = Function("main", Block(List(
      Simple("main0"),
      Call("fun0"),
      Simple("main2"),
    )))

    val edbs = funs.flatMap(_.toEdbRelations) ++ main.toEdbRelations
    edbs.foreach(loaded.engine.insert)

    println((funs :+ main).mkString("\n"))
    //    loaded.engine.readAll().map(_.asTable).foreach(println)
    val cflow = loaded.query("cflow")
    println(cflow.asTable)

    val reachable = loaded.query("reachable")
    println(s"Computed ${reachable.size} reachable entries")
    val deadCode = loaded.query("deadCode")
    println(s"Found ${deadCode.size} dead-code statements")


    println("\n#####################\n")
    println("Incremental update\n")
    loaded.engine.addUpdateListener(new RelationUpdateListener(deadCode) {
      override def tupleAdded(tup: rel.Tuple): Unit = println(s"New tuple $tup")
      override def tupleRemoved(tup: rel.Tuple): Unit = println(s"Deleted tuple $tup")
    })

    lastNonBlock(funs.head.body).foreach( last =>
      val call = Call("fun1")
      val s = Simple("f0")
      loaded.engine.insert(Relation2("next", Seq("s1", "s2"), Seq(Seq(last, call), Seq(call, s))))
      stmtToEdbRelations(call).foreach(loaded.engine.insert)
      stmtToEdbRelations(s).foreach(loaded.engine.insert)
    )

  }
