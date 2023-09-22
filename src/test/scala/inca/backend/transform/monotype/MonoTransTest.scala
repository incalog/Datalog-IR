package inca.backend.transform.monotype

import collection.mutable
import scala.meta._
import inca.backend.ir.Datalog
import inca.util.Scala
import org.scalatest.funsuite.AnyFunSuiteLike


class MonoTransTest extends AnyFunSuiteLike {
  private lazy val treeSize: Datalog.Module = {
    // size(t, m) :- leaf(t), m <- 1
    lazy val pat1Body1: Datalog.Body = Datalog.Body(Seq(
      Datalog.ExtensionalCall("leaf", Seq(Datalog.Var("t"))),
      Datalog.UpdateMono(Datalog.Var("m"),
        Datalog.Constant(Datalog.IntLiteral(1)), "CountMono")
    ))
    // size(t, m) :- btree(t, l, r), size(l, m), size(r, m), label(t, v), m <- v
    lazy val pat1Body2: Datalog.Body = Datalog.Body(Seq(
      Datalog.ExtensionalCall("btree", Seq(
        Datalog.Var("t"),
        Datalog.Var("l"),
        Datalog.Var("r")
      )),
      Datalog.Call("height", Seq(
        Datalog.Var("l"),
        Datalog.Var("m")
      )),
      Datalog.Call("height", Seq(
        Datalog.Var("r"),
        Datalog.Var("m")
      )),
      Datalog.UpdateMono(Datalog.Var("m"), Datalog.Constant(Datalog.IntLiteral(1)), "CountMono")
    ))

    // main(t, v) :- MkMono(m, SumCountMono), size(t, m), m -> v
    lazy val pat2Body: Datalog.Body = Datalog.Body(Seq(
      Datalog.MkMono(Datalog.Var("m"), "CountMono"),
      Datalog.Call("size", Seq(
        Datalog.Var("t"),
        Datalog.Var("m")
      )),
      Datalog.ReadMono(Datalog.Var("m"), Datalog.Var("v"), "CountMono"),
    ))

    lazy val pat1: Datalog.Pattern = Datalog.Pattern(
      None, "size",
      Seq(Datalog.Param("t", Datalog.TScalaString),
        Datalog.Param("m", Datalog.TScala(Scala(t"Mono[Int, (Int, Int)]")))),
      Seq(pat1Body1, pat1Body2)
    )

    lazy val pat2: Datalog.Pattern = Datalog.Pattern(
      None, "main",
      Seq(Datalog.Param("t", Datalog.TScalaString),
        Datalog.Param("v", Datalog.TScalaInt)),
      Seq(pat2Body)
    )


    Datalog.Module(
      "treeSize", Seq(), Seq(pat1, pat2), Seq()
    )
  }


  test("Tree size") {
    println(treeSize)
    val init : meta.Lit = q"0"
    val add : meta.Term.Function = q"(x : Int, y : Int) => x + 1"
    val result : meta.Term.Function = q"(x : Int) => x"
    val countMono = MonoDef(init, add, result)
    val transformer = MonoTrans(Map(("CountMono", countMono)))
    println("------------------")
    println(transformer.transModule(treeSize))
  }
}
