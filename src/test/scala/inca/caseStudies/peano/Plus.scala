package inca.caseStudies.peano

import inca.backend.ir.GP._
import inca.compiler.{Compiler, Options}
import inca.runtime.EnginePool
import inca.runtime.context.{LanguageMetaInfo, QueryScope}
import inca.util.Meta.Scala
import org.eclipse.viatra.query.runtime.rete.matcher.TimelyReteBackendFactory

import scala.meta.XtensionQuasiquoteTerm

object Plus extends App {

//  sealed trait Nat
//  case class Zero() extends Nat
//  case class Succ(pred: Nat) extends Nat

  // Zero_f(out) :- ...
  val Zero_f = Pattern(None, "Zero_f", Seq(Param("out", TScalaString)), Seq(Body(Seq(
//    Computed(Var("out"), Evaluation(Seq(), Nat, Scala(q"() => new truechange.JVMURI()")))
    Eq(Var("out"), Constant(StringLiteral("Zero()")))
  ))))

  // unZero_b(n) :- input_unZero_b(n), ... .
  val unZero_b = Pattern(None, "unZero_b", Seq(Param("n", TScalaString)), Seq(Body(Seq(
    Call("Zero_f", Seq(Var("n")))
  ))))

  // input_Succ_bf(x0) :- Zero_f(x0).
  // input_Succ_bf(x1) :- Zero_f(x0), Succ_bf(x0, x1).
  // input_Succ_bf(out1) :- input_plus_bbf(m, n), unSucc_bf(m, pred), plus_bbf(pred, n, out1).
  val input_Succ_bf = Pattern(None, "input_Succ_bf", Seq(Param("out", TScalaString)), Seq(
    Body(Seq(
      Call("Zero_f", Seq(Var("out")))
    )),
    Body(Seq(
      Call("Zero_f", Seq(Var("x0"))),
      Call("Succ_bf", Seq(Var("x0"), Var("out")))
    )),
    Body(Seq(
      Call("input_plus_bbf", Seq(Var("m"), Var("n"))),
      Call("unSucc_bf", Seq(Var("m"), Var("pred"))),
      Call("plus_bbf", Seq(Var("pred"), Var("n"), Var("out")))
    ))
  ))

  val Nat = Pattern(None, "Nat", Seq(Param("out", TScalaString)), Seq(
    Body(Seq(
      Call("Zero_f", Seq(Var("out")))
    )),
    Body(Seq(
      Call("Succ_bf", Seq(Var("pred"), Var("out")))
    )),
    Body(Seq(
      HasType(Var("out"), TNode("Nat"))
    ))
  ))

  // (0_a,1_b), (0_c,1_d), (1_b,2_e)
  val Succ_bf = Pattern(None, "Succ_bf", Seq(Param("pred", TScalaString), Param("out", TScalaString)), Seq(
    Body(Seq(
      Call("input_Succ_bf", Seq(Var("pred"))),
      Computed(Var("out"), Evaluation(Seq(Var("pred") -> TScalaString), TScalaString, Scala(q"""(pred: String) => "Succ(" + pred + ")" """)))
    )),
    Body(Seq(
      HasType(Var("out"), TNode("Succ")),
      Path(Var("out"), TNode("Succ"), NamedLink(TNode("Succ"), "pred"), Var("pred"), TNode("Nat"))
    ))
  ))

  // unSucc_bf(n, pred) :- input_unSucc_bf(n), ... .
  val unSucc_bf = Pattern(None, "unSucc_bf", Seq(Param("n", TScalaString), Param("pred", TScalaString)), Seq(Body(Seq(
    Call("Succ_bf", Seq(Var("pred"), Var("n")))
  ))))

  // input_plus_bbf(x2, y1) :- Zero_f(x0), Succ_bf(x0, x1), Succ_bf(x1, x2), Zero_f(y0), Succ_bf(y0, y1).
  // input_plus_bbf(pred, n) :- input_plus_bbf(m, n), unSucc_bf(m, pred).
  val input_plus_bbf = Pattern(None, "input_plus_bbf", Seq(Param("m", TScalaString), Param("n", TScalaString)), Seq(
    Body(Seq(
      Call("Zero_f", Seq(Var("x0"))),
      Call("Succ_bf", Seq(Var("x0"), Var("x1"))),
      Call("Succ_bf", Seq(Var("x1"), Var("m"))),
      Call("Zero_f", Seq(Var("y0"))),
      Call("Succ_bf", Seq(Var("y0"), Var("n")))
    )),
    Body(Seq(
      Call("input_plus_bbf", Seq(Var("k"), Var("n"))),
      Call("unSucc_bf", Seq(Var("k"), Var("m")))
    ))
  ))

  /*
  plus_bbf(m, n, out) :-
    input_plus_bbf(m, n),
    unZero_b(m),
    out == n.
  plus_bbf(m, n, out) :-
    input_plus_bbf(m, n),
    unSucc_bf(m, pred),
    plus_bbf(pred, n, out1),
    Succ_bf(out1, out2),
    out == out2.
   */
  val plus_bbf = Pattern(None, "plus_bbf", Seq(Param("m", TScalaString), Param("n", TScalaString), Param("out", TScalaString)), Seq(
    Body(Seq(
      Call("input_plus_bbf", Seq(Var("m"), Var("n"))),
      Call("unZero_b", Seq(Var("m"))),
      Eq(Var("out"), Var("n"))
    )),
    Body(Seq(
      Call("input_plus_bbf", Seq(Var("m"), Var("n"))),
      Call("unSucc_bf", Seq(Var("m"), Var("pred"))),
      Call("plus_bbf", Seq(Var("pred"), Var("n"), Var("out1"))),
      Call("Succ_bf", Seq(Var("out1"), Var("out")))
    ))
  ))

  val main = Pattern(None, "main", Seq(Param("out", TScalaString)), Seq(Body(Seq(
    Call("Zero_f", Seq(Var("x0"))),
    Call("Succ_bf", Seq(Var("x0"), Var("x1"))),
    Call("Succ_bf", Seq(Var("x1"), Var("x2"))),
    Call("Zero_f", Seq(Var("y0"))),
    Call("Succ_bf", Seq(Var("y0"), Var("y1"))),
    Call("plus_bbf", Seq(Var("x2"), Var("y1"), Var("out")))
  ))))

  val module = Module("Main", Seq(), Seq(), Seq(
    Zero_f,
    unZero_b,
    Succ_bf, input_Succ_bf,
    unSucc_bf,
    Nat,
    plus_bbf, input_plus_bbf,
    main
  ), Seq())

  val compiled = Compiler.compileGP(module, Options(new LanguageMetaInfo()))
  println(compiled.optimized)
  val scope = new QueryScope(new LanguageMetaInfo())
  val (engine, feed) = EnginePool.loadEngineAndDatabase(scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)

  def printMatches(name: String): Unit = {
    val matcher = EnginePool.loadQuery(compiled.psystemModule.patterns(name)(), scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)
    println(s"matches of $name:   ${matcher.getAllMatches}")
  }

  compiled.psystemModule.patterns.keys.foreach(printMatches)
}
