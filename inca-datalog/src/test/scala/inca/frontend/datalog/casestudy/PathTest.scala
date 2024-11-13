package inca.frontend.datalog.casestudy

import inca.frontend.datalog.compile.DatalogCompilerOptions
import inca.frontend.datalog.executor.DatalogExecutor
import inca.frontend.datalog.executor.DatalogExecutor.?
import inca.ir.execution.{Relation, Relation1, Relation2, RelationUpdateListener}
import inca.util.FileUtil
import inca.viatra.backend.Executor
import org.eclipse.viatra.query.runtime.rete.matcher.DRedReteBackendFactory
import org.scalatest.Ignore
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.mutable.ListBuffer

@Ignore
class PathTest extends AnyFunSuite:
  val pipeline = List()
  val options = DatalogCompilerOptions.fromResource("datalog/Options.ini")
  options.irLogging.logModule = false
  val exec: DatalogExecutor = new DatalogExecutor(new Executor(DRedReteBackendFactory.INSTANCE))


  val edges =
    (for (i <- 0 until 1000) yield Seq(i, i + 1))
    ++
    (for (i <- 0 until 1000 by 50) yield Seq(i + 50, i))
  val edgeRelation = Relation2("edge", Seq("x", "y"), edges)

  test("path non-incremental") {
    val code = FileUtil.readFileFromResource("datalog/casestudy/path.dl")
    val compiled = exec.compileDatalog(code, options)
    compiled.setPipeline(pipeline)

    var times = List[Long]()
    for (i <- 0 until 4) {
      val loaded = exec.loadDatalog(compiled)
      val start = System.currentTimeMillis()
      loaded.engine.insert(edgeRelation)
      val path = loaded.query("path")
      val end = System.currentTimeMillis()
      times = (end - start) +: times
      println(s"Derived ${path.size} path tuples")
    }
    val afterWarmpup = times.take(times.size / 2)
    val avg = afterWarmpup.sum / afterWarmpup.size
    println(times)
    println(s"Average running time ${avg}ms")
  }

  test("path incremental") {
    val code = FileUtil.readFileFromResource("datalog/casestudy/path.dl")
    val compiled = exec.compileDatalog(code, options)
    compiled.setPipeline(pipeline)

    val loaded = exec.loadDatalog(compiled)
    loaded.engine.insert(edgeRelation)
    val pathInitial = loaded.query("path")
    println(s"Derived ${pathInitial.size} path tuples initially")

    val newEdges =
      (for (i <- 980 until 1020) yield Seq(i, i + 1))
    val newEdgeRelation = Relation2("edge", Seq("x", "y"), newEdges)

    var insertTimes = List[Long]()
    var deleteTimes = List[Long]()
    for (i <- 0 until 4) {
      val startInsert = System.currentTimeMillis()
      loaded.engine.insert(newEdgeRelation)
      val pathInserted = loaded.query("path")
      val endInsert = System.currentTimeMillis()
      insertTimes = (endInsert - startInsert) +: insertTimes
      println(s"Derived ${pathInserted.size} path tuples incrementally, added ${pathInserted.size - pathInitial.size} tuples")

      val startDelete = System.currentTimeMillis()
      loaded.engine.remove(newEdgeRelation)
      val pathDeleted = loaded.query("path")
      val endDelete = System.currentTimeMillis()
      deleteTimes = (endDelete - startDelete) +: deleteTimes
      println(s"Derived ${pathDeleted.size} path tuples incrementally, removed ${pathInserted.size - pathDeleted.size} tuples")
    }
    val insertAfterWarmpup = insertTimes.take(insertTimes.size / 2)
    val avgInsert = insertAfterWarmpup.sum / insertAfterWarmpup.size
    println(s"Average insertion time ${avgInsert}ms")

    val deleteAfterWarmpup = deleteTimes.take(deleteTimes.size / 2)
    val avgDelete = deleteAfterWarmpup.sum / deleteAfterWarmpup.size
    println(s"Average deletion time ${avgDelete}ms")

    loaded.engine.addUpdateListener(new RelationUpdateListener(pathInitial) {
      override def tupleAdded(tup: rel.Tuple): Unit = println(s"Added tuple $tup")

      override def tupleRemoved(tup: rel.Tuple): Unit = println(s"Removed tuple $tup")
    })
    loaded.engine.insert(new Relation2("edge", Seq("x", "y"),
      Seq(Seq(990, 987))
    ))
  }


