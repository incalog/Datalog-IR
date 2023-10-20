package inca.frontend.datalog.typechecker

import inca.frontend.datalog.syntax.*
import inca.frontend.datalog.typecheck.Typechecker
import org.scalatest.funsuite.AnyFunSuite

class TypecheckerTest extends AnyFunSuite {

  def check(l: Literal): Unit =
    val typechecker = new Typechecker
    typechecker.inferLiteral(l)
    typechecker.failOnError()

  def check(r: Relation): Unit =
    check(Module(Seq(r)))

  def check(m: Module): Unit =
    val typechecker = new Typechecker
    typechecker.checkModule(m)
    typechecker.failOnError()

  test("various") {
    check(Parser.literal.parseAll("1").getOrElse(???))
    check(Parser.literal.parseAll("12").getOrElse(???))
    check(Parser.literal.parseAll("12").getOrElse(???))
    check(Parser.literal.parseAll("123").getOrElse(???))
    check(Parser.literal.parseAll("123.456").getOrElse(???))
  }

  test("Edge") {
    val m = s"""Edge(Int, Int).
               |Edge(1,2).
               |""".stripMargin
    check(Parser.relation.parseAll(m).getOrElse(???))

    val m2 =
      s"""Edge(Int, Int).
         |Edge(1,2).
         |Edge(2,3).
         |Edge(3,4).
         |Edge(4,5).
         |Edge(3,1).
         |""".stripMargin
    check(Parser.relation.parseAll(m2).getOrElse(???))

    val m3 =
      s"""Edge(Int, Int).
         |Edge(1,2).
         |Edge(1,2).
         |""".stripMargin
    check(Parser.relation.parseAll(m3).getOrElse(???))
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
    check(Parser.module.parseAll(m).getOrElse(???))

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
    check(Parser.module.parseAll(m2).getOrElse(???))

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
    check(Parser.module.parseAll(m3).getOrElse(???))
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
    check(Parser.module.parseAll(m).getOrElse(???))
  }
}
