package inca.runtime.aggregate

import inca.backend.ir.Datalog
import inca.backend.ir.Datalog.{Atom, Body, MkMono, Module, Param, Pattern, ReadMono, TScalaInt, UpdateMono, Eq}
import inca.util.Scala

import scala.meta._
import org.scalatest.funsuite.AnyFunSuiteLike

class MonoAggregationTest extends AnyFunSuiteLike {

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


  private lazy val treeSizeTrans: Datalog.Module = {
    // size(t, m) :- leaf(t)
    lazy val pat1Body1: Datalog.Body = Datalog.Body(Seq(
      Datalog.ExtensionalCall("leaf", Seq(Datalog.Var("t")))
    ))


    // size(t, m) :- btree(t, l, r), size(l, m), size(r, m)
    lazy val pat1Body2: Datalog.Body = Datalog.Body(Seq(
      Datalog.ExtensionalCall("btree", Seq(
        Datalog.Var("t"),
        Datalog.Var("l"),
        Datalog.Var("r")
      )),
      Datalog.Call("size", Seq(
        Datalog.Var("l"),
        Datalog.Var("m")
      )),
      Datalog.Call("size", Seq(
        Datalog.Var("r"),
        Datalog.Var("m")
      ))
    ))

    lazy val pat1: Datalog.Pattern = Datalog.Pattern(
      None, "size",
      Seq(Datalog.Param("t", Datalog.TScalaString),
        Datalog.Param("m", Datalog.TScalaString)),
      Seq(pat1Body1, pat1Body2)
    )

    // main(t, v) :- size(t, m), m -> v
    lazy val pat2Body: Datalog.Body = Datalog.Body(Seq(
      Datalog.MkMono(Datalog.Var("m"), "CountMono"),
      Datalog.Call("size", Seq(
        Datalog.Var("t"),
        Datalog.Var("m")
      )),
      Datalog.Call("ReadMono", Seq(Datalog.Var("m"), Datalog.Var("v")))
    ))

    lazy val pat2 : Datalog.Pattern = Datalog.Pattern(None, "main", Seq(
      Datalog.Param("t", Datalog.TScalaString),
      Datalog.Param("v", Datalog.TScalaInt)
    ), Seq(pat2Body))

    // Update(m, k) :- leaf(t), k = 1
    lazy val pat3Body1: Datalog.Body = Datalog.Body(Seq(
      Datalog.ExtensionalCall("leaf", Seq(Datalog.Var("t"))),
      Datalog.Eq(Datalog.Var("k"), Datalog.IntConstant(1))
    ))

    // Update(m, k) :- btree(t, l, r), size(l, m), size(r, m), k = 1
    lazy val pat3Body2: Datalog.Body = Datalog.Body(Seq(
      Datalog.ExtensionalCall("btree", Seq(
        Datalog.Var("t"),
        Datalog.Var("l"),
        Datalog.Var("r")
      )),
      Datalog.Call("size", Seq(
        Datalog.Var("l"),
        Datalog.Var("m")
      )),
      Datalog.Call("size", Seq(
        Datalog.Var("r"),
        Datalog.Var("m")
      )),
      Datalog.Eq(Datalog.Var("k"), Datalog.IntConstant(1))
    ))

    lazy val pat3: Datalog.Pattern = Datalog.Pattern(
      None, "Update",
      Seq(Datalog.Param("m", Datalog.TScalaString),
        Datalog.Param("k", Datalog.TScalaInt)),
      Seq(pat3Body1, pat3Body2)
    )

    // ReadMono(m, b) :- State(m, st), b = st
    lazy val pat4Body : Datalog.Body = Datalog.Body(Seq(
      Datalog.Call("State", Seq(Datalog.Var("m"), Datalog.Var("st"))),
      Datalog.Eq(Datalog.Var("st"), Datalog.Var("b"))
    ))

    lazy val pat4 : Datalog.Pattern = Datalog.Pattern(None, "ReadMono", Seq(
      Datalog.Param("m", Datalog.TScalaString),
      Datalog.Param("b", Datalog.TScalaInt),
    ), Seq(pat4Body))

    // State(m, st) :- st = 0
    lazy val pat5Body1 : Datalog.Body = Datalog.Body(Seq(
      Datalog.Eq(Datalog.Var("st"), Datalog.IntConstant(0))
    ))

    // State(m, st1) :- State(m, st), Update(m, a), st1 = st + 1
    lazy val pat5Body2 : Datalog.Body = Datalog.Body(Seq(
      Datalog.Call("State", Seq(Datalog.Var("m"), Datalog.Var("st"))),
      Datalog.Call("Update", Seq(Datalog.Var("m"), Datalog.Var("a"))),
      Datalog.Computed(Datalog.Var("st1"), Datalog.Evaluation(
        Seq(Datalog.Var("st") -> Datalog.TScalaInt),
        Datalog.TScalaInt,
        Scala(q"(x : Int) => x + 1")
      ))
    ))

    lazy val pat5 : Datalog.Pattern = Datalog.Pattern(None, "State", Seq(
      Datalog.Param("m", Datalog.TScalaString),
      Datalog.Param("st1", Datalog.TScalaInt)
    ), Seq(pat5Body1, pat5Body2))


    Datalog.Module(
      "treeSize", Seq(), Seq(pat1, pat2, pat3, pat4, pat5), Seq()
    )
  }


  test("Tree size"){
    println(treeSizeTrans)
  }
}
