package inca.backend.transform.monotype

import collection.mutable
import scala.meta._
import inca.backend.ir.Datalog
import inca.backend.ir.Datalog.TScala
import inca.util.Scala
import org.scalatest.funsuite.AnyFunSuiteLike
import inca.runtime.aggregate.MonoAggregation

case class CountMono() extends MonoAggregation[Map[String, Int], (String, Int), Int] {
  override val name: String = ""

  override def init: Map[String, Int] = Map[String, Int]()

  override def add(m : Map[String, Int], p : (String, Int)) : Map[String, Int] = {
    if (m.contains(p._1)) {
      for ((k, v) <- m) yield {
        if (k != p._1) (k, v) else (k, m(p._1) + 1)
      }
    } else{
      m + p
    }
  }

  override def result(st : Map[String, Int]) : Int = {
    st.foldLeft(0)((size, kv) => size + kv._2)
  }
}

class MonoTransTest extends AnyFunSuiteLike {
  // a = ("t", 1)
  private lazy val bindPair : Datalog.Atom = Datalog.Computed(Datalog.Var("a"), Datalog.Evaluation(
    Seq(),
    Datalog.TScala(Scala(t"(String, Int)")),
    Scala(q"()=> (t, 1)")
  ))

  private lazy val treeSize1: Datalog.Module = {
    // size(t, m) :- leaf(t), m <- (t, 1).
    lazy val pat1Body1: Datalog.Body = Datalog.Body(Seq(
      Datalog.ExtensionalCall("leaf", Seq(Datalog.Var("t"))),
      bindPair,
      Datalog.Add(Datalog.Var("m"), Datalog.Var("a"))
    ))
    // size(t, m) :- btree(t, l, r), size(l, m), size(r, m), m <- (t, 1)
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
      )),
      bindPair,
      Datalog.Add(Datalog.Var("m"), Datalog.Var("a"))
    ))

    // main(t, v) :- MkMono(m, CountMono), size(t, m),t = "A", m -> v
    lazy val pat2Body: Datalog.Body = Datalog.Body(Seq(
      Datalog.MkMono(Datalog.Var("m"), "CountMono"),
      Datalog.Call("size", Seq(
        Datalog.Var("t"),
        Datalog.Var("m")
      )),
      Datalog.Eq(Datalog.Var("t"), Datalog.StringConstant("A")),
      Datalog.Result(Datalog.Var("m"), Datalog.Var("v")),
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
      "treeSize1", Seq(), Seq(pat1, pat2), Seq()
    )
  }

  // tree size program after transformation
  private lazy val treeSize2 : Datalog.Module = {
    // size(t, m) :- leaf(t), input_size(t, m)
    lazy val pat1Body1: Datalog.Body = Datalog.Body(Seq(
      Datalog.ExtensionalCall("leaf", Seq(Datalog.Var("t"))),
      Datalog.Call("input_size", Seq(Datalog.Var("t"), Datalog.Var("m")))
    ))

    // size(t, m) :- btree(t, l, r), size(l, m), size(r, m), input_size(t, m)
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
      )),
      Datalog.Call("input_size", Seq(Datalog.Var("t"), Datalog.Var("m")))
    ))

    lazy val pat1 : Datalog.Pattern = Datalog.Pattern(
      None,
      "size",
      Seq(Datalog.Param("t", Datalog.TScalaString), Datalog.Param("m", Datalog.TScalaString)),
      Seq(pat1Body1, pat1Body2)
    )

    // main(t, b) :- size(t, m), t = "A", tmp = Agg(m, st), b = m.result(tmp)
    lazy val pat2Body: Datalog.Body = Datalog.Body(Seq(
      Datalog.Call("size", Seq(
        Datalog.Var("t"),
        Datalog.Var("m")
      )),
      Datalog.Eq(Datalog.Var("t"), Datalog.StringConstant("A")),
      Datalog.Computed(Datalog.Var("tmp"), Datalog.CustomAggregation(
        Datalog.TScalaInt,
        None,
        Scala(q"""new inca.backend.transform.monotype.CountMono()"""),
        "Coll",
        Seq(Datalog.Var("m"), Datalog.Var("st")),
        1
      )),
      Datalog.Computed(Datalog.Var("b"), Datalog.Evaluation(
        Seq(
          Datalog.Var("m") -> TScala(Scala(t"inca.backend.transform.monotype.CountMono")),
          Datalog.Var("tmp") -> TScala(Scala(t"Map[Int, String]"))
        ),
        Datalog.TScalaInt,
        Scala(q"(m : MonoAggregation[Map[String, Int], (String, Int), Int], tmp: Map[String, Int]) => m.result(st)")
      ))
    ))

    lazy val pat2 : Datalog.Pattern = Datalog.Pattern(
      None,
      "main",
      Seq(Datalog.Param("t", Datalog.TScalaString), Datalog.Param("b", Datalog.TScalaInt)),
      Seq(pat2Body)
    )

    // input_size(l, m) :- btree(t, l, r), input_size(t, m)
    lazy val pat3Body1 : Datalog.Body = Datalog.Body(Seq(
      Datalog.ExtensionalCall("btree", Seq(
        Datalog.Var("t"), Datalog.Var("l"), Datalog.Var("r")
      )),
      Datalog.Call("input_size", Seq(Datalog.Var("t"), Datalog.Var("m")))
    ))

    // input_size(l, m) :- btree(t, r, l), input_size(t, m)
    lazy val pat3Body2: Datalog.Body = Datalog.Body(Seq(
      Datalog.ExtensionalCall("btree", Seq(
        Datalog.Var("t"), Datalog.Var("l"), Datalog.Var("r")
      )),
      Datalog.Call("input_size", Seq(Datalog.Var("t"), Datalog.Var("m")))
    ))

    lazy val pat3 : Datalog.Pattern = Datalog.Pattern(
      None,
      "input_size",
      Seq(Datalog.Param("l", Datalog.TScalaString), Datalog.Param("m", Datalog.TScalaString)),
      Seq(pat3Body1, pat3Body2)
    )

    // Coll(m, v) :- leaf(t), v = (t, 1), input_size(t, m)
    lazy val pat4Body1 : Datalog.Body = Datalog.Body(Seq(
      Datalog.ExtensionalCall("leaf", Seq(Datalog.Var("t"))),
      Datalog.Computed(Datalog.Var("a"), Datalog.Evaluation(
        Seq(),
        Datalog.TScala(Scala(t"(String, Int)")),
        Scala(q"() => (t, 1)")
      )),
      Datalog.Call("input_size", Seq(Datalog.Var("t"), Datalog.Var("m")))
    ))

    // Coll(m, v) :- btree(t, l, r), v = (t, 1), size(l, m),
    //               size(r, m), inputSize(t, m)
    lazy val pat4Body2 : Datalog.Body = Datalog.Body(Seq(
      Datalog.Call("btree", Seq(Datalog.Var("t"), Datalog.Var("l"), Datalog.Var("r"))),
      Datalog.Computed(Datalog.Var("a"), Datalog.Evaluation(
        Seq(),
        Datalog.TScala(Scala(t"(String, Int)")),
        Scala(q"() => (t, 1)")
      )),
      Datalog.Call("size", Seq(Datalog.Var("l"), Datalog.Var("m"))),
      Datalog.Call("size", Seq(Datalog.Var("r"), Datalog.Var("m"))),
      Datalog.Call("input_size", Seq(Datalog.Var("t"), Datalog.Var("m")))
    ))

    lazy val pat4 : Datalog.Pattern = Datalog.Pattern(
      None,
      "Coll",
      Seq(Datalog.Param("m", Datalog.TScalaString), Datalog.Param("v", TScala(Scala(t"(String, Int)")))),
      Seq(pat4Body1, pat4Body2)
    )

    Datalog.Module("treeSize", Seq(), Seq(pat1, pat2, pat3, pat4), Seq())
  }


  test("Tree size") {
    println(treeSize2)
  }
}
