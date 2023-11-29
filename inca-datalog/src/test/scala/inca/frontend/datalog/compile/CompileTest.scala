package inca.frontend.datalog.compile

import inca.frontend.datalog.compile.GenerateIR
import inca.frontend.datalog.syntax.*
import inca.frontend.datalog.typecheck.Typechecker
import inca.ir.typing.IRTypechecker
import org.scalatest.funsuite.AnyFunSuite

class CompileTest extends AnyFunSuite {

  def compile(r: Relation): Unit =
    compile(Module(Seq(r)))

  def compile(m: Module): Unit =
    val typechecker = new Typechecker
    typechecker.checkModule(m)
    typechecker.failOnError()
    val compiler = new GenerateIR
    val c = compiler.compileModule(m)
    val irtypechecker = new IRTypechecker
    try irtypechecker.checkModule(c)
    finally println(c)


  test("Edge") {
    val m = s"""Edge(Int, Int).
               |Edge(1,2).
               |""".stripMargin
    compile(Parser.relation.parseAll(m).getOrElse(???))

    val m2 =
      s"""Edge(Int, Int).
         |Edge(1,2).
         |Edge(2,3).
         |Edge(3,4).
         |Edge(4,5).
         |Edge(3,1).
         |""".stripMargin
    compile(Parser.relation.parseAll(m2).getOrElse(???))

    val m3 =
      s"""Edge(Int, Int).
         |Edge(1,2).
         |Edge(1,2).
         |""".stripMargin
    compile(Parser.relation.parseAll(m3).getOrElse(???))
  }

  test("Path") {
    val m =
      s"""Edge(Int, Int).
         |Edge(1,2).
         |
         |Path(Int, Int).
         |Path(X,Y) :- Edge(X,Y)
         |          :- Edge(X,Z), Path(Z,Y).
         |""".stripMargin
    compile(Parser.module.parseAll(m).getOrElse(???))

    val m2 =
      s"""Edge(Int, Int).
         |Edge(1,2).
         |Edge(2,3).
         |Edge(3,4).
         |Edge(4,5).
         |Edge(3,1).
         |
         |Path(Int, Int).
         |Path(X,Y) :- Edge(X,Y)
         |          :- Edge(X,Z), Path(Z,Y).
         |""".stripMargin
    compile(Parser.module.parseAll(m2).getOrElse(???))

    val m3 =
      s"""Edge(Int, Int).
         |Edge(1,2).
         |Edge(2,3).
         |Edge(3,4).
         |Edge(4,5).
         |Edge(3,1).
         |
         |Path(Int, Int).
         |Path(X,Y) :- Edge(X,Y).
         |Path(X,Z) :- Edge(X,Y), Path(Y,Z).
         |""".stripMargin
    compile(Parser.module.parseAll(m3).getOrElse(???))
  }

  test("ShortestPath") {
    val m =
      s"""Edge(Int, Int, Int).
         |Edge(1,2,20).
         |
         |SPath(Int, Int, Int).
         |SPath(X,Y,min(n)) :- Edge(X,Y,n)
         |                  :- Edge(X,Z,n1), SPath(Z,Y,n2), n == n1 + n2.
         |""".stripMargin
    compile(Parser.module.parseAll(m).getOrElse(???))
  }
}
