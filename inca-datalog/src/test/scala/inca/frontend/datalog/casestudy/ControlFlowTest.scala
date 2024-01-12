package inca.frontend.datalog.casestudy

import inca.frontend.datalog.compile.DatalogCompilerOptions
import inca.frontend.datalog.executor.DatalogExecutor
import inca.frontend.datalog.executor.DatalogExecutor.?
import inca.ir.execution.{Relation1, Relation, Relation2}
import inca.util.FileUtil
import org.eclipse.viatra.query.runtime.rete.matcher.DRedReteBackendFactory
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.mutable.ListBuffer

class ControlFlowTest extends AnyFunSuite:
  val pipeline = List()
  val options = DatalogCompilerOptions.fromResource("datalog/Options.ini")
  val exec: DatalogExecutor = new DatalogExecutor(new inca.viatra.Executor(DRedReteBackendFactory.INSTANCE))


  var nextId: Int = 0

  import Stmt.*
  enum Stmt:
    case Simple(lab: String)
    case If(thn: Stmt, els: Stmt)
    case While(body: Stmt)
    case Block(list: List[Stmt])

    val id: Int = nextId
    nextId += 1

    def foreach(f: Stmt => Unit): Unit =
      f(this)
      this match
        case Simple(_) => // nothing
        case If(thn, els) => thn.foreach(f); els.foreach(f)
        case While(body) => body.foreach(f)
        case Block(list) => list.foreach(_.foreach(f))

  def stmtToEdbRelations(s: Stmt): Seq[Relation] =
    val simples = ListBuffer.empty[Stmt.Simple]
    val whiles = ListBuffer.empty[Stmt.While]
    val ifs = ListBuffer.empty[Stmt.If]
    val blocks = ListBuffer.empty[Stmt.Block]
    def collect(s: Stmt) = s match
      case s: Stmt.Simple => simples += s
      case s: Stmt.While => whiles += s
      case s: Stmt.If => ifs += s
      case s: Stmt.Block => blocks += s
    s.foreach(collect)

    // edb simpleStmt(Int).
    val edbSimple = Relation1("simpleStmt", Seq("s"), simples.toSeq.map(s => Seq(s.id)))

    //edb whileStmt(Int).
    val edbWhile = Relation1("whileStmt", Seq("s"), whiles.toSeq.map(s => Seq(s.id)))
    //edb whileBody(Int, Int).
    val edbWhileBody = Relation2("whileBody", Seq("w", "b"), whiles.toSeq.map(s => Seq(s.id, s.body.id)))

    //edb ifStmt(Int).
    val edbIf = Relation1("ifStmt", Seq("s"), ifs.toSeq.map(s => Seq(s.id)))
    //edb ifThen(Int, Int).
    //edb ifElse(Int, Int).

    //edb blockStmt(Int).
    val edbBlock = Relation1("blockStmt", Seq("s"), blocks.toSeq.map(s => Seq(s.id)))
    //edb blockFirst(Int, Int).
    val edbBlockFirst = Relation2("blockStmt", Seq("s", "f"), blocks.toSeq.flatMap(s =>
      s.list.headOption.map(f => Seq(s.id, f.id)))
    )
    //edb blockNext(Int, Int).
    val edbBlockNext = Relation2("blockNext", Seq("s1", "s2"), blocks.flatMap { block =>
      block.list.zip(block.list.tail).map((pred, succ) => Seq(pred.id, succ.id))
    })


    Seq(edbSimple, edbWhile, edbWhileBody, edbIf, edbBlock, edbBlockFirst, edbBlockNext)


  test("control flow seq") {
    val code = FileUtil.readFileFromResource("datalog/casestudy/controlFlow.dl")
    val compiled = exec.compileDatalog(code, options)
    compiled.setPipeline(pipeline)
    val loaded = exec.loadDatalog(compiled)

    val s = Block(List(Simple("a"), Simple("b"), Simple("c"), Simple("d")))
    val edbs = stmtToEdbRelations(s)
    edbs.foreach(loaded.engine.insert)

    val stmt = loaded.query("stmt")
    println(stmt.asTable)

    val cflow = loaded.query("cflow")
    println(cflow.asTable)
  }

  test("control flow while") {
    val code = FileUtil.readFileFromResource("datalog/casestudy/controlFlow.dl")
    val compiled = exec.compileDatalog(code, options)
    compiled.setPipeline(pipeline)
    val loaded = exec.loadDatalog(compiled)

    val s = Block(List(Simple("a"), While(Simple("b")), Simple("d"), Simple("e")))
    val edbs = stmtToEdbRelations(s)
    edbs.foreach(t => println(t.asTable))
    edbs.foreach(loaded.engine.insert)

    println(s)
    s.foreach(s => println(s"${s.id}\t = $s"))

    val cflow = loaded.query("cflow")
    println(cflow.asTable)
  }
