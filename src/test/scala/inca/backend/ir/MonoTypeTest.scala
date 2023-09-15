package inca.backend.ir

import org.scalatest.funsuite.AnyFunSuiteLike

import scala.meta._
import inca.util.Scala




class MonoTypeTest extends AnyFunSuiteLike {

  private lazy val maxMono : meta.Defn =
    q"""class MaxMono extends MT[Int, Int]:
          private var state = 0
          override def add (a : Int) : Unit =
            if this.state < a then this.state = a
          override def result() : Int = this.state
        end MaxMono"""

  private lazy val sumMono : meta.Defn =
    q"""class SumMono extends MT[Int, Int]:
          private var state: Int = 0
          override def add(a: Int): Unit = state = state + a
          override def result(): Int = state
        end SumMono"""

  private lazy val countMono : meta.Defn =
    q"""class CountMono extends MT[Int, Int]:
         private var state : Int  = 0
         override def add (a : Int) : Unit = state = state + 1
         override def result() : Int = state
       end CountMono"""

  private lazy val countSumMono : meta.Defn =
    q"""class SumCountMono extends MT[Int, (Int, Int)]:
          private val m1 = new SumMono()
          private val m2 = new CountMono()
          override def add (a : Int) : Unit =
            m1.add(a) ; m2.add(a)
          override def result() : (Int, Int) = (m1.result(), m2.result())
        end SumCountMono
     """

  // Example 1 : compute the height of binary tree
  private lazy val treeHeight : Datalog.Module = {
    // height(t, m) :- leaf(t), m <- 1
    lazy val pat1Body1: Datalog.Body = Datalog.Body(Seq(
      Datalog.ExtensionalCall("leaf", Seq(Datalog.Var("t"))),
      Datalog.UpdateMono(Datalog.MonoVar("m"),
        Datalog.Constant(Datalog.IntLiteral(1)))
    ))

    // height(t, m) :- btree(t, l, r), height(l, m),
    //                 m -> v, k = v + 1, m <- k
    lazy val pat1Body2: Datalog.Body = Datalog.Body(Seq(
      Datalog.ExtensionalCall("btree", Seq(
        Datalog.Var("t"),
        Datalog.Var("l"),
        Datalog.Var("r")
      )),
      Datalog.Call("height", Seq(
        Datalog.Var("l"),
        Datalog.MonoVar("m")
      )),
      Datalog.ReadMono(Datalog.MonoVar("m"), Datalog.Var("v")),
      Datalog.Computed(
        Datalog.Var("k"),
        Datalog.Evaluation(
          Seq(Datalog.Var("v") -> Datalog.TScalaInt),
          Datalog.TScalaInt,
          Scala(q"((v: Int) => v + 1)")
        )
      )))

    // height(t, m) :- btree(t, l, r), height(r, m),
    //                 m -> v, k = v + 1, m <- k
    lazy val pat1Body3: Datalog.Body = Datalog.Body(Seq(
      Datalog.ExtensionalCall("btree", Seq(
        Datalog.Var("t"),
        Datalog.Var("l"),
        Datalog.Var("r")
      )),
      Datalog.Call("height", Seq(
        Datalog.Var("r"),
        Datalog.MonoVar("m")
      )),
      Datalog.ReadMono(Datalog.MonoVar("m"), Datalog.Var("v")),
      Datalog.Computed(
        Datalog.Var("k"),
        Datalog.Evaluation(
          Seq(Datalog.Var("v") -> Datalog.TScalaInt),
          Datalog.TScalaInt,
          Scala(q"((v: Int) => v + 1)")
        )
      )))

    // main(t, v) :- MkMono(m, MaxMono), height(t, m), m -> v
    lazy val pat2Body: Datalog.Body = Datalog.Body(Seq(
      Datalog.MkMono(Datalog.MonoVar("m"), "MaxMono"),
      Datalog.Call(
        "height", Seq(Datalog.Var("t"), Datalog.MonoVar("m"))),
      Datalog.ReadMono(Datalog.MonoVar("m"), Datalog.Var("v"))
    ))

    lazy val pattern1: Datalog.Pattern = Datalog.Pattern(
      None, "height",
      Seq(
        Datalog.Param("t", Datalog.TScalaString),
        Datalog.Param("m", Datalog.TScala(Scala(t"Mono[Int, Int]")))
      ),
      Seq(pat1Body1, pat1Body2, pat1Body3)
    )

    lazy val pattern2: Datalog.Pattern = Datalog.Pattern(
      None, "main",
      Seq(
        Datalog.Param("t", Datalog.TScalaString),
        Datalog.Param("v", Datalog.TScalaInt)
      ),
      Seq(pat2Body)
    )

    Datalog.Module("TreeHeight", Seq(),
      Seq(pattern1, pattern2), Seq(Scala(maxMono)))
  }

  // Example2 : compute the average of labels of binary tree
  private lazy val treeAvg : Datalog.Module = {
    // avg(t, m) :- leaf(t), label(t, v), m <- v
    lazy val pat1Body1 : Datalog.Body = Datalog.Body(Seq(
      Datalog.ExtensionalCall("leaf", Seq(Datalog.Var("t"))),
      Datalog.ExtensionalCall("label", Seq(
        Datalog.Var("t"),
        Datalog.Var("v")
      )),
      Datalog.UpdateMono(Datalog.MonoVar("m"), Datalog.Var("v"))
    ))
    // avg(t, m) :- btree(t, l, r), avg(l, m), avg(r, m), label(t, v), m <- v
    lazy val pat1Body2 : Datalog.Body = Datalog.Body(Seq(
      Datalog.ExtensionalCall("btree", Seq(
        Datalog.Var("t"),
        Datalog.Var("l"),
        Datalog.Var("r")
      )),
      Datalog.Call("height", Seq(
        Datalog.Var("l"),
        Datalog.MonoVar("m")
      )),
      Datalog.Call("height", Seq(
        Datalog.Var("r"),
        Datalog.MonoVar("m")
      )),
      Datalog.ExtensionalCall("label", Seq(
        Datalog.Var("t"),
        Datalog.Var("v")
      )),
      Datalog.UpdateMono(Datalog.MonoVar("m"), Datalog.Var("v"))
    ))

    // main(t, v) :- MkMono(m, SumCountMono), avg(t, m), m -> (n, d), v = n / d
    lazy val pat2Body : Datalog.Body = Datalog.Body(Seq(
      Datalog.MkMono(Datalog.MonoVar("m"), "SumCountMono"),
      Datalog.Call("avg", Seq(
        Datalog.Var("t"),
        Datalog.MonoVar("m")
      )),
      Datalog.ReadMono(Datalog.MonoVar("m"), Datalog.Var("frac")),
      Datalog.Computed(
        Datalog.Var("v"),
        Datalog.Evaluation(
          Seq(Datalog.Var("frac") -> Datalog.TScala(Scala(t"(Int, Int)"))),
          Datalog.TScalaInt,
          Scala(q"((frac : (Int, Int)) => frac(0) / frac(1))")
        ))
    ))

    lazy val pat1 : Datalog.Pattern = Datalog.Pattern(
      None, "avg",
      Seq(Datalog.Param("t", Datalog.TScalaString),
        Datalog.Param("m", Datalog.TScala(Scala(t"Mono[Int, (Int, Int)]")))),
      Seq(pat1Body1, pat1Body2)
    )

    lazy val pat2 : Datalog.Pattern = Datalog.Pattern(
      None, "main",
      Seq(Datalog.Param("t", Datalog.TScalaString),
        Datalog.Param("v", Datalog.TScalaInt)),
      Seq(pat2Body)
    )


    Datalog.Module(
      "AvgLabel", Seq(), Seq(pat1, pat2), Seq(
        Scala(countMono),
        Scala(sumMono),
        Scala(countSumMono)
      )
    )
  }

  private lazy val treeDiameter = {
    // height(t, v) :- leaf(t), v = 1
    val pat1Body1 : Datalog.Body = Datalog.Body(Seq(
      Datalog.ExtensionalCall("leaf", Seq(Datalog.Var("t"))),
      Datalog.Eq(Datalog.Var("v"), Datalog.Constant(Datalog.IntLiteral(1)))
    ))

    // height(t, v) :- btree(t, l, r), height(l, v1), height(r, v2), v = max(v1, v2) + 1
    val pat1Body2 : Datalog.Body = Datalog.Body(Seq(
      Datalog.ExtensionalCall("btree", Seq(
        Datalog.Var("t"),
        Datalog.Var("l"),
        Datalog.Var("r")
      )),
      Datalog.Call("height", Seq(
        Datalog.Var("l"),
        Datalog.Var("v1")
      )),
      Datalog.Call("height", Seq(
        Datalog.Var("r"),
        Datalog.Var("v2")
      )),
      Datalog.Computed(
        Datalog.Var("h"),
        Datalog.Evaluation(
          Seq(
            Datalog.Var("v1") -> Datalog.TScalaInt,
            Datalog.Var("v2") -> Datalog.TScalaInt,
          ),
          Datalog.TScalaInt,
          Scala(q"""((v1 : Int, v2 : Int) =>
            if (v1 > v2) v1 + 1 else v2 + 1)"""))
    )))

    // D(t, m) :- leaf(t), m <- 1
    val pat2Body1 : Datalog.Body = Datalog.Body(Seq(
      Datalog.ExtensionalCall("leaf", Seq(Datalog.Var("t"))),
      Datalog.UpdateMono(Datalog.MonoVar("m"),
        Datalog.Constant(Datalog.IntLiteral(1)))
    ))

    // D(t, m) :- btree(t, l, r), height(l, v1),
    //            height(r, v2), k = v1 + v2 + 1, m <- k
    val pat2Body2 : Datalog.Body = Datalog.Body(Seq(
      Datalog.ExtensionalCall("btree", Seq(
        Datalog.Var("t"),
        Datalog.Var("l"),
        Datalog.Var("r")
      )),
      Datalog.Call("height", Seq(
        Datalog.Var("l"),
        Datalog.Var("v1")
      )),
      Datalog.Call("height", Seq(
        Datalog.Var("r"),
        Datalog.Var("v2")
      )),
      Datalog.Computed(
        Datalog.Var("h"),
        Datalog.Evaluation(
          Seq(
            Datalog.Var("v1") -> Datalog.TScalaInt,
            Datalog.Var("v2") -> Datalog.TScalaInt,
          ),
          Datalog.TScalaInt,
          Scala(q"((v1 : Int, v2 : Int) => v1 + v2))")
    ))))

    // main(t, v) :- MkMono(m, MaxMono), D(t, m), m -> v
    val pat3Body : Datalog.Body = Datalog.Body(Seq(
      Datalog.MkMono(Datalog.MonoVar("m"), "MaxMono"),
      Datalog.Call("D", Seq(
        Datalog.Var("t"),
        Datalog.MonoVar("m")
      )),
      Datalog.ReadMono(
        Datalog.MonoVar("m"),
        Datalog.Var("v")
      )
    ))

    lazy val pat1 : Datalog.Pattern = Datalog.Pattern(
      None, "height", Seq(
        Datalog.Param("t", Datalog.TScalaString),
        Datalog.Param("v", Datalog.TScalaInt)
      ),
      Seq(pat1Body1, pat1Body2)
    )

    lazy val pat2 : Datalog.Pattern = Datalog.Pattern(
      None, "D", Seq(
        Datalog.Param("t", Datalog.TScalaString),
        Datalog.Param("m", Datalog.TScala(Scala(t"Mono[Int, Int]")))
      ), Seq(pat2Body1, pat2Body2)
    )

    lazy val pat3 : Datalog.Pattern = Datalog.Pattern(
      None, "main", Seq(
        Datalog.Param("t", Datalog.TScalaString),
        Datalog.Param("v", Datalog.TScalaInt)
      ), Seq(pat3Body)
    )

    Datalog.Module("Diameter", Seq(), Seq(pat1, pat2, pat3), Seq(Scala(maxMono)))
  }

}
