package inca.frontend.datalog.integration

import inca.frontend.datalog.executor.DatalogExecutor
import org.scalatest.funsuite.AnyFunSuite

import scala.meta.quasiquotes._

class GraphTest extends AnyFunSuite {
  test("graph -- intensional edges") {
    val engine = DatalogExecutor.loadDatalog(
      s"""module Graph
         |
         |relation Edge(`Int`, `Int`)
         |Edge(1,2).
         |Edge(2,3).
         |Edge(3,4).
         |Edge(4,5).
         |Edge(3,1).
         |
         |relation Path(`Int`, `Int`)
         |Path(x,y) :- Edge(x, y).
         |Path(x,y) :- Path(x, z), Edge(z, y).
         |""".stripMargin
    )
    assert(engine.output("Edge").size == 5)
    // 12, 23, 34, 45, 31
    // 13, 24, 35, 21, 32
    // 11, 25, 33, 14, 22
    // 15
    assert(engine.output("Path").size == 16)
  }

  test("graph -- extensional edges") {
    val engine = DatalogExecutor.loadDatalog(
      s"""module Graph
         |
         |@extensional relation Edge(Int, Int)
         |
         |relation Path(Int, Int)
         |Path(x,y) :- Edge(x, y).
         |Path(x,y) :- Path(x, z), Edge(z, y).
         |""".stripMargin
    )
    assert(engine.output("Path").isEmpty)

    engine.insert("Edge", 1, 2)
    engine.insert("Edge", 2, 3)
    assert(engine.output("Path").size == 3)

    engine.insert("Edge", 3, 4)
    assert(engine.output("Path").size == 6)

    engine.insert("Edge", 4, 5)
    assert(engine.output("Path").size == 10)

    engine.insert("Edge", 3, 1)
    assert(engine.output("Path").size == 16)
  }

  test("graph -- extensional data edges") {
    val engine = DatalogExecutor.loadDatalog(
      s"""module Graph
         |
         |data Node = Station(name: String)
         |
         |@extensional relation Edge(Node, Node)
         |
         |relation Path(Node, Node)
         |Path(x,y) :- Edge(x, y).
         |Path(x,y) :- Path(x, z), Edge(z, y).
         |
         |@main relation DestinationsFromMainz(String)
         |DestinationsFromMainz(dest) :- Path(from, to), Station("Mainz", from), Station(dest, to).
         |""".stripMargin
    )

    assert(engine.output("Path").isEmpty)

    val mainz = engine.load(q"""Station("Mainz")""")
    val wiesbaden = engine.load(q"""Station("Wiesbaden")""")
    engine.insert("Edge", mainz, wiesbaden)

    val mainzKastel = engine.load(q"""Station("Mainz-Kastel")""")
    engine.insert("Edge", wiesbaden, mainzKastel)
    assert(engine.output("Path").size == 3)

    val bischofsheim = engine.load(q"""Station("Bischofsheim")""")
    engine.insert("Edge", mainzKastel, bischofsheim)
    assert(engine.output("Path").size == 6)

    val frankfurt = engine.load(q"""Station("Frankfurt")""")
    engine.insert("Edge", bischofsheim, frankfurt)
    assert(engine.output("Path").size == 10)

    engine.insert("Edge", mainzKastel, mainz)
    assert(engine.output("Path").size == 16)

    engine.output("Path").foreach(println)
    engine.output("DestinationsFromMainz").foreach(println)

    assert(engine.output("DestinationsFromMainz").size == 5)
  }

  test("graph -- extensional data edges with filter") {
    val engine = DatalogExecutor.loadDatalog(
      s"""module Graph
         |
         |data Node = Station(name: String, functional: Boolean)
         |
         |@extensional relation Edge(Node, Node)
         |
         |relation FunctionalStation(Station)
         |FunctionalStation(s) :- Station(_, true, s).
         |
         |relation Path(Node, Node)
         |Path(x,y) :- Edge(x, y), FunctionalStation(x), FunctionalStation(y).
         |Path(x,y) :- Path(x, z), Edge(z, y), FunctionalStation(y).
         |
         |@main relation DestinationsFromMainz(String)
         |DestinationsFromMainz(dest) :- Path(from, to), Station("Mainz", _, from), Station(dest, _, to).
         |""".stripMargin
    )

    assert(engine.output("Path").isEmpty)

    val mainz = engine.load(q"""Station("Mainz", true)""")
    val wiesbaden = engine.load(q"""Station("Wiesbaden", true)""")
    engine.insert("Edge", mainz, wiesbaden)

    val mainzKastel = engine.load(q"""Station("Mainz-Kastel", true)""")
    engine.insert("Edge", wiesbaden, mainzKastel)
    assert(engine.output("Path").size == 3)

    val bischofsheim = engine.load(q"""Station("Bischofsheim", true)""")
    engine.insert("Edge", mainzKastel, bischofsheim)
    assert(engine.output("Path").size == 6)

    val frankfurt = engine.load(q"""Station("Frankfurt", true)""")
    engine.insert("Edge", bischofsheim, frankfurt)
    assert(engine.output("Path").size == 10)

    engine.insert("Edge", mainzKastel, mainz)
    assert(engine.output("Path").size == 16)
    assert(engine.output("DestinationsFromMainz").size == 5)

    engine.replace(bischofsheim, q"""Station("Bischofsheim", false)""")
    assert(engine.output("Path").size == 9)
    assert(engine.output("DestinationsFromMainz").size == 3)
  }
}