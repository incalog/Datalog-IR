package inca.viatra.simple

import inca.ir.*
import inca.ir.extension.arithmetic.{IntNum, TInt, IR as arithIR}
import inca.ir.util.SourceLocation
import inca.util.compileroptions.CompilerOptions
import org.scalatest.funsuite.AnyFunSuite

class SimpleTest extends AnyFunSuite:
  private def createAndLowerCompiledUnit(mod: Module) =
    val compiledUnit = new CompiledUnit {
      def compilerOptions: CompilerOptions = CompilerOptions.default

      def name: Name = mod.name

      def sourceLocation: SourceLocation = SourceLocation.NoSourceLocation

      def isClosedWorld: Boolean = true

      def irModules: Seq[Module] = Seq(mod)

      def otherUnits: Seq[CompiledUnit] = Seq()
    }
    compiledUnit.lowered

  test("Test ") {
    case class Edge(from: String, to: String)

    case class Path(from: String, to: String)

    class Graph {
      val edges: Set[Edge] = Set(
        Edge("X", "Y"),
        Edge("Y", "Z"),
        Edge("Z", "X"),
        Edge("Z", "W"),
        Edge("A", "W"),
        Edge("B", "A"),
        Edge("B", "C")
      )

      def path(x: String, visited: Set[String] = Set()): Set[Path] = {
        // Avoid revisiting nodes that are already visited
        if (visited.contains(x)) return Set()

        val newVisited = visited + x

        (for (d <- this.edges if d.from == x) yield Path(d.from, d.to)) ++
        (for (i <- this.edges if i.from == x; y <- this.path(i.to, newVisited)) yield Path(x, y.to))
      }
    }

    val g = new Graph()
    println(g.path("X"))
  }

  test("Simple 1") {
    val mod = Module("Simple 1", BaseIR.language + arithIR, Seq(
      Relation("test", Seq(Param("x", TInt), Param("y", TInt)), Seq(
        Body(Seq(
          Eq(Var("x"), IntNum(3)),
          Eq(Var("y"), IntNum(2))
        )),
        Body(Seq(
          Eq(Var("x"), IntNum(4)),
          Eq(Var("y"), IntNum(2))
        ))
      )),
      Relation("main", Seq(Param("a", TInt), Param("b", TInt)), Seq(
        Body(Seq(
          Call("test", Seq(Var("a"), Var("b")))
        ))
      )).addHint(MainHint),
    ))
    createAndLowerCompiledUnit(mod)
  }

  test("Simple 2") {
    val mod = Module("Simple 1", BaseIR.language + arithIR, Seq(
      Relation("test", Seq(Param("x", TInt), Param("y", TInt)), Seq(
        Body(Seq(
          Eq(Var("x"), IntNum(3)),
          Eq(Var("y"), IntNum(2))
        )),
        Body(Seq(
          Eq(Var("x"), IntNum(4)),
          Eq(Var("y"), IntNum(2))
        ))
      )),
      Relation("main", Seq(Param("a", TInt), Param("b", TInt)), Seq(
        Body(Seq(
          Call("test", Seq(Var("a"), IntNum(2))),
          //Eq(Var("b"), IntNum(1)),
          Call("test", Seq(Var("b").arg, Var("d").arg))
        ))
      )).addHint(MainHint),
    ))
    createAndLowerCompiledUnit(mod)
  }
