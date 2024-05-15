package inca.viatra.incremental

import inca.ir.*
import inca.ir.execution.Relation2
import inca.ir.extension.arithmetic
import inca.ir.extension.arithmetic.TInt
import inca.ir.util.SourceLocation
import inca.util.compileroptions.CompilerOptions
import inca.viatra.backend.Executor
import org.scalatest.funsuite.AnyFunSuiteLike

import scala.collection.mutable.ListBuffer

class IncrementalPathTest extends AnyFunSuiteLike:

  def edgeRel: ExtensionalRelation = ExtensionalRelation("edge", Seq(Param("X", TInt), Param("Y", TInt)))
  def pathRel: Relation = Relation("path", Seq(Param("X", TInt), Param("Y", TInt)),
    Seq(
      Body(Seq(ExtensionalCall("edge", Seq(Var("X"), Var("Y"))))),
      Body(Seq(ExtensionalCall("edge", Seq(Var("X"), Var("Z"))), Call("path", Seq(Var("Z"), Var("Y"))))),
    )
  )
  def pathModule: Module =
    Module("Path", Language.Datalog + arithmetic.IR, Seq(edgeRel, pathRel))

  def initialEdges(nodes: Int, loopDistance: Int): Relation2[Integer, Integer] = Relation2("edge", Seq("from", "to"),
    (for (i <- 0 until nodes by 3) yield
      Seq(i, i+3))
    ++
    (for (i <- 0 until nodes by 5) yield
      Seq(i, i + 5))
    ++
    (for (i <- loopDistance until nodes by loopDistance) yield
      Seq(i, i - loopDistance))
  )

  def moreEdges(from: Int, nodes: Int, loopDistance: Int): Relation2[Integer, Integer] = Relation2("edge", Seq("from", "to"),
    (for (i <- from until nodes by 3) yield
      Seq(i, i + 3))
      ++
      (for (i <- from until nodes by 5) yield
        Seq(i, i + 5))
      ++
      (for (i <- from + loopDistance until nodes by loopDistance) yield
        Seq(i, i - loopDistance))
  )

  test("Non-incremental Path"):
    val mod = pathModule
    val compiled = new Compiled(mod)
    for (i <- 0 until 5) {
      val engine = new Executor().instantiate(compiled)
      val start = System.currentTimeMillis()
      engine.insert(initialEdges(1000, 100))
      val path = engine.read(Relation2("path", Seq("from", "to"), Seq()))
      val end = System.currentTimeMillis()

      println(s"Execution time ${end - start}ms")
      println(s"Found ${path.size} paths")

      assert(path.contains((900, 800)))
      assert(path.contains((900, 805)))
      assert(path.contains((900, 900)))
    }

  test("Incremental Path"):
    val mod = pathModule
    val compiled = new Compiled(mod)

    val engine = new Executor().instantiate(compiled)
    val initialStart = System.currentTimeMillis()
    engine.insert(initialEdges(1000, 100))
    val path = engine.read(Relation2("path", Seq("from", "to"), Seq()))
    val initialEnd = System.currentTimeMillis()

    println(s"Initial execution time ${initialEnd - initialStart}ms")
    println(s"Found ${path.size} paths initially")

    val extra = 10
    for (i <- 0 until 5) {
      val start = System.currentTimeMillis()
      engine.insert(moreEdges(1000 + i * extra, 1000 + i * extra + extra, 100))
      val path = engine.read(Relation2("path", Seq("from", "to"), Seq()))
      val end = System.currentTimeMillis()

      println(s"Execution time ${end - start}ms")
      println(s"Found ${path.size} paths")
    }



  class Compiled(val ir: Module) extends CompiledUnit:
    override def compilerOptions: CompilerOptions =
      val opt = CompilerOptions.default
      opt.irLogging.logModule = false
      opt
    override def name: Name = ir.name
    override def sourceLocation: SourceLocation = SourceLocation.NoSourceLocation
