package inca.backend.transform

import inca.backend.ir.GP
import inca.backend.transform.magic.{Adornment, AdornmentTag, Free, MagicSetTransformation}
import inca.frontend.examples.AST
import inca.frontend.lowering.GenerateDatalog
import inca.util.Meta.Scala
import org.scalatest.funsuite.AnyFunSuite

import scala.meta.quasiquotes._

class MagicSetTransformationTest extends AnyFunSuite {

  def gpmodule(content: GP.Pattern*): GP.Module =
    GP.Module("Main", Seq(), content, Seq())

  val incFunGP = GP.Pattern(None, "inc_bf", Seq(GP.Param("n", GP.TScalaInt), GP.Param("out", GP.TScalaInt)), Seq(GP.Body(Seq(
    GP.Call("input_inc_bf", Seq(GP.Var("n")), transitive = false, neg = false),
    GP.Computed(GP.Var("lit"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 1"))),
    GP.Computed(GP.Var("eval"), GP.Evaluation(Seq(GP.Var("n") -> GP.TScalaInt, GP.Var("lit") -> GP.TScalaInt),
    GP.TScalaInt, Scala(q"(left: Int, right: Int) => left + right"))),
    GP.Eq(GP.Var("out"), GP.Var("eval"))
  ))))
  val incMainGP = GP.Pattern(None, "main_f", Seq(GP.Param("out", GP.TScalaInt)), Seq(GP.Body(Seq(
    GP.Computed(GP.Var("lit"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 0"))),
    GP.Call("inc_bf", Seq(GP.Var("lit"), GP.Var("out_0")), transitive = false, neg = false),
    GP.Eq(GP.Var("out"), GP.Var("out_0"))
  ))))
  val magicIncFunGP = GP.Pattern(None, "input_inc_bf", Seq(GP.Param("n", GP.TScalaInt)), Seq(GP.Body(Seq(
    GP.Computed(GP.Var("lit"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 0"))),
    GP.Eq(GP.Var("lit"), GP.Var("n"))
  ))))
  val incModuleGP = gpmodule(incFunGP, incMainGP, magicIncFunGP)



  val factFunGP = GP.Pattern(None, "fact_bf", Seq(GP.Param("n", GP.TScalaInt), GP.Param("out", GP.TScalaInt)), Seq(GP.Body(Seq(
    GP.Call("input_fact_bf", Seq(GP.Var("n")), transitive = false, neg = false),
    GP.Computed(GP.Var("lit"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 1"))),
    GP.Computed(GP.Var("eval"), GP.Evaluation(Seq(GP.Var("n") -> GP.TScalaInt, GP.Var("lit") -> GP.TScalaInt),
      GP.TScalaBoolean, Scala(q"(left: Int, right: Int) => left == right"))),
    GP.Eq(GP.Var("eval"), GP.True),
    GP.Computed(GP.Var("lit_0"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 1"))),
    GP.Eq(GP.Var("out"), GP.Var("lit_0"))
  )), GP.Body(Seq(
    GP.Call("input_fact_bf", Seq(GP.Var("n")), transitive = false, neg = false),
    GP.Computed(GP.Var("lit"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 1"))),
    GP.Computed(GP.Var("eval"), GP.Evaluation(Seq(GP.Var("n") -> GP.TScalaInt, GP.Var("lit") -> GP.TScalaInt),
      GP.TScalaBoolean, Scala(q"(left: Int, right: Int) => left == right"))),
    GP.Eq(GP.Var("eval"), GP.False),
    GP.Computed(GP.Var("lit_1"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 1"))),
    GP.Computed(GP.Var("eval_0"), GP.Evaluation(Seq(GP.Var("n") -> GP.TScalaInt, GP.Var("lit_1") -> GP.TScalaInt),
      GP.TScalaInt, Scala(q"(left: Int, right: Int) => left - right"))),
    GP.Call("fact_bf", Seq(GP.Var("eval_0"), GP.Var("out_0")), transitive = false, neg = false),
    GP.Computed(GP.Var("eval_1"), GP.Evaluation(Seq(GP.Var("n") -> GP.TScalaInt, GP.Var("out_0") -> GP.TScalaInt),
      GP.TScalaInt, Scala(q"(left: Int, right: Int) => left * right"))),
    GP.Eq(GP.Var("out"), GP.Var("eval_1"))
  ))))
  val factMainGP = GP.Pattern(None, "main_f", Seq(GP.Param("out", GP.TScalaInt)), Seq(GP.Body(Seq(
    GP.Computed(GP.Var("lit"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 3"))),
    GP.Call("fact_bf", Seq(GP.Var("lit"), GP.Var("out_0")), transitive = false, neg = false),
    GP.Eq(GP.Var("out"), GP.Var("out_0"))
  ))))
  val magicFactFunGP = GP.Pattern(None, "input_fact_bf", Seq(GP.Param("n_0", GP.TScalaInt)), Seq(GP.Body(Seq(
    GP.Computed(GP.Var("lit"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 3"))),
    GP.Eq(GP.Var("lit"), GP.Var("n_0"))
  )), GP.Body(Seq(
    GP.Call("input_fact_bf", Seq(GP.Var("n")), transitive = false, neg = false),
    GP.Computed(GP.Var("lit"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 1"))),
    GP.Computed(GP.Var("eval"), GP.Evaluation(Seq(GP.Var("n") -> GP.TScalaInt, GP.Var("lit") -> GP.TScalaInt),
      GP.TScalaBoolean, Scala(q"(left: Int, right: Int) => left == right"))),
    GP.Eq(GP.Var("eval"), GP.False),
    GP.Computed(GP.Var("lit_1"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 1"))),
    GP.Computed(GP.Var("eval_0"), GP.Evaluation(Seq(GP.Var("n") -> GP.TScalaInt, GP.Var("lit_1") -> GP.TScalaInt),
      GP.TScalaInt, Scala(q"(left: Int, right: Int) => left - right"))),
    GP.Eq(GP.Var("eval_0"), GP.Var("n_0"))
  ))))
  val factModuleGP = gpmodule(factFunGP, factMainGP, magicFactFunGP)



  def moduleEqual(m1: GP.Module, m2: GP.Module): Boolean =
    m1.pats.size == m2.pats.size && m1.pats.forall(m2.pats.contains)

  test("magic sets of flat function") {
    val trans = new Adornment {
      override def query: GP.Call = GP.Call("main", Seq(GP.Var("out")), transitive = false, neg = false)
      override def adornmentTags: Seq[AdornmentTag] = Seq(Free)
    }

    val gpModule = GenerateDatalog.transformModule(AST.incModule)
    val adorned = trans.transformer.transformModule(gpModule)
    val magicSet = new MagicSetTransformation {}.transformer.transformModule(adorned)
    assert(moduleEqual(magicSet, incModuleGP))
  }

  test("magic sets of recursive function") {
    val trans = new Adornment {
      override def query: GP.Call = GP.Call("main", Seq(GP.Var("out")), transitive = false, neg = false)
      override def adornmentTags: Seq[AdornmentTag] = Seq(Free)
    }

    val gpModule = GenerateDatalog.transformModule(AST.factModule)
    val adorned = trans.transformer.transformModule(gpModule)
    val magicSet = new MagicSetTransformation {}.transformer.transformModule(adorned)
    assert(moduleEqual(magicSet, factModuleGP))
  }


  // trait Env
  // object Env {
  //   case class Empty() extends Env
  //   case class Bind(x: String, v: Val, rest: Env) extends Env
  // }
  // trait Exp
  // object Exp {
  //   case class Unit() extends Exp
  //   case class App(e1: Exp, e2: Exp) extends Exp
  //   case class Lam(name: String, e: Exp) extends Exp
  //   case class Var(name: String) extends Exp
  // }
  // trait Val
  // object Val {
  //   case class VUnit() extends Val
  //   case class VClosure(name: String, exp: Exp, env: Env) extends Val
  // }

  // val tyEnv = GP.TScala(Scala(t"inca.caseStudies.functional.FunctionalExamples.Env"))
  // val tyExp = GP.TScala(Scala(t"inca.caseStudies.functional.FunctionalExamples.Exp"))
  // val tyVal = GP.TScala(Scala(t"inca.caseStudies.functional.FunctionalExamples.Val"))
  // val interp = GP.Module("LambdaCalcInterp", Seq(),
  //   Seq(
  //     GP.Pattern(
  //       None,
  //       "interpDefault",
  //       Seq(GP.Param("e", tyExp), GP.Param("out", tyVal)),
  //       Seq(
  //         GP.Body(Seq(
  //           GP.Computed(GP.Var("env"), GP.Evaluation(Seq(), tyEnv, Scala(q"() => Env.Empty()"))),
  //           GP.Call("interp", Seq(GP.Var("env"), GP.Var("e"), GP.Var("out")), transitive = false, neg = false)
  //         ))
  //       )
  //     ),
  //     GP.Pattern(
  //       None,
  //       "interp",
  //       Seq(GP.Param("env", tyEnv), GP.Param("e", tyExp), GP.Param("out", tyVal)),
  //       Seq(
  //         GP.Body(Seq(
  //           GP.Computed(GP.Var("isUnit"), GP.Evaluation(Seq((GP.Var("e"), tyExp)), GP.TScalaBoolean, Scala(q"(e: Exp) => e match { case Exp.Unit() => true; case _ => false }"))),
  //           Compare(EqComparator, Var("isUnit"), Constant(BooleanLiteral(true))),
  //           Computed(Var("out"), Evaluation(Seq(), tyVal, Scala(q"() => Val.VUnit()")))
  //         )),
  //         Body(Seq(
  //           Computed(Var("isUnit"), Evaluation(Seq((Var("e"), tyExp)), TScalaBoolean, Scala(q"(e: Exp) => e match { case Exp.Unit() => true; case _ => false }"))),
  //           Compare(EqComparator, Var("isUnit"), Constant(BooleanLiteral(false))),
  //           Computed(Var("isApp"), Evaluation(Seq((Var("e"), tyExp)), TScalaBoolean, Scala(q"(e: Exp) => e match { case Exp.App(_, _) => true; case _ => false }"))),
  //           Compare(EqComparator, Var("isApp"), Constant(BooleanLiteral(true))),
  //           Computed(Var("e1"), Evaluation(Seq((Var("e"), tyExp)), tyExp, Scala(q"(e: Exp) => e match { case Exp.App(e1, _) => e1; case _ => null }"))),
  //           Call("interp", Seq(Var("env"), Var("e1"), Var("out1")), transitive = false, neg = false),
  //           Computed(Var("isVClosure"), Evaluation(Seq((Var("out1"), tyVal)), TScalaBoolean, Scala(q"(v: Val) => v match { case Val.VClosure(_, _, _) => true; case _ => false }"))),
  //           Compare(EqComparator, Var("isVClosure"), Constant(BooleanLiteral(true))),
  //           Computed(Var("e2"), Evaluation(Seq((Var("e"), tyExp)), tyExp, Scala(q"(e: Exp) => e match { case Exp.App(_, e2) => e2; case _ => null }"))),
  //           Call("interp", Seq(Var("env"), Var("e2"), Var("out2")), transitive = false, neg = false),
  //           Computed(Var("name"), Evaluation(Seq((Var("out1"), tyVal)), TScalaString, Scala(q"(v: Val) => v match { case Val.VClosure(name, _, _) => name; case _ => null }"))),
  //           Computed(Var("body"), Evaluation(Seq((Var("out1"), tyVal)), tyExp, Scala(q"(v: Val) => v match { case Val.VClosure(_, body, _) => body; case _ => null }"))),
  //           Computed(Var("fenv"), Evaluation(Seq((Var("out1"), tyVal)), tyEnv, Scala(q"(v: Val) => v match { case Val.VClosure(_, _, env) => env; case _ => null }"))),
  //           Computed(Var("extEnv"), Evaluation(Seq((Var("name"), TScalaString), (Var("out2"), tyVal), (Var("fenv"), tyEnv)), tyEnv, Scala(q"(name: String, v: Val, env: Env) => Env.Bind(name, v, env)"))),
  //           Call("interp", Seq(Var("extEnv"), Var("body"), Var("out")), transitive = false, neg = false),
  //         )),
  //         Body(Seq(
  //           Computed(Var("isUnit"), Evaluation(Seq((Var("e"), tyExp)), TScalaBoolean, Scala(q"(e: Exp) => e match { case Exp.Unit() => true; case _ => false }"))),
  //           Compare(EqComparator, Var("isUnit"), Constant(BooleanLiteral(false))),
  //           Computed(Var("isApp"), Evaluation(Seq((Var("e"), tyExp)), TScalaBoolean, Scala(q"(e: Exp) => e match { case Exp.App(_, _) => true; case _ => false }"))),
  //           Compare(EqComparator, Var("isApp"), Constant(BooleanLiteral(false))),
  //           Computed(Var("isLam"), Evaluation(Seq((Var("e"), tyExp)), TScalaBoolean, Scala(q"(e: Exp) => e match { case Exp.Lam(_, _) => true; case _ => false }"))),
  //           Compare(EqComparator, Var("isLam"), Constant(BooleanLiteral(true))),
  //           Computed(Var("name"), Evaluation(Seq((Var("e"), tyExp)), TScalaString, Scala(q"(e: Exp) => e match { case Exp.Lam(name, _) => name; case _ => null }"))),
  //           Computed(Var("body"), Evaluation(Seq((Var("e"), tyExp)), tyExp, Scala(q"(e: Exp) => e match { case Exp.Lam(_, body) => body; case _ => null }"))),
  //           Computed(Var("out"), Evaluation(Seq((Var("name"), TScalaString), (Var("body"), tyExp), (Var("env"), tyEnv)), tyVal, Scala(q"(name: String, exp: Exp, env: Env) => Val.VClosure(name, exp, env)"))),
  //         )),
  //         Body(Seq(
  //           Computed(Var("isUnit"), Evaluation(Seq((Var("e"), tyExp)), TScalaBoolean, Scala(q"(e: Exp) => e match { case Exp.Unit() => true; case _ => false }"))),
  //           Compare(EqComparator, Var("isUnit"), Constant(BooleanLiteral(false))),
  //           Computed(Var("isApp"), Evaluation(Seq((Var("e"), tyExp)), TScalaBoolean, Scala(q"(e: Exp) => e match { case Exp.App(_, _) => true; case _ => false }"))),
  //           Compare(EqComparator, Var("isApp"), Constant(BooleanLiteral(false))),
  //           Computed(Var("isLam"), Evaluation(Seq((Var("e"), tyExp)), TScalaBoolean, Scala(q"(e: Exp) => e match { case Exp.Lam(_, _) => true; case _ => false }"))),
  //           Compare(EqComparator, Var("isLam"), Constant(BooleanLiteral(false))),
  //           Computed(Var("isVar"), Evaluation(Seq((Var("e"), tyExp)), TScalaBoolean, Scala(q"(e: Exp) => e match { case Exp.Var(_) => true; case _ => false }"))),
  //           Compare(EqComparator, Var("isVar"), Constant(BooleanLiteral(true))),
  //           Computed(Var("name"), Evaluation(Seq((Var("e"), tyExp)), TScalaString, Scala(q"(exp: Exp) => exp match { case Exp.Var(name) => name; case _ => null }"))),
  //           Call("lookup", Seq(Var("env"), Var("name"), Var("out")), transitive = false, neg = false),
  //         ))
  //       )
  //     ),
  //     Pattern(
  //       None,
  //       "lookup",
  //       Seq(Param("env", tyEnv), Param("name", TScalaString), Param("v", tyVal)),
  //       Seq(
  //         Body(Seq(
  //           Computed(Var("name1"), Evaluation(Seq((Var("env"), tyEnv)), TScalaString, Scala(q"(env: Env) => env match { case Env.Empty() => null; case Env.Bind(x, v, rest) => x }"))),
  //           Computed(Var("v1"), Evaluation(Seq((Var("env"), tyEnv)), tyVal, Scala(q"(env: Env) => env match { case Env.Empty() => null; case Env.Bind(x, v, rest) => v }"))),
  //           Computed(Var("rest"), Evaluation(Seq((Var("env"), tyEnv)), tyEnv, Scala(q"(env: Env) => env match { case Env.Empty() => null; case Env.Bind(_, _, rest) => rest }"))),
  //           Compare(EqComparator, Var("name"), Var("name1")),
  //           Compare(EqComparator, Var("v"), Var("v1")),
  //         )),
  //         Body(Seq(
  //           Computed(Var("name1"), Evaluation(Seq((Var("env"), tyEnv)), TScalaString, Scala(q"(env: Env) => env match { case Env.Empty() => null; case Env.Bind(x, v, rest) => x }"))),
  //           Computed(Var("v1"), Evaluation(Seq((Var("env"), tyEnv)), tyVal, Scala(q"(env: Env) => env match { case Env.Empty() => null; case Env.Bind(x, v, rest) => v }"))),
  //           Computed(Var("rest"), Evaluation(Seq((Var("env"), tyEnv)), tyEnv, Scala(q"(env: Env) => env match { case Env.Empty() => null; case Env.Bind(_, _, rest) => rest }"))),
  //           Compare(NeqComparator, Var("name"), Var("name1")),
  //           Call("lookup", Seq(Var("rest"), Var("name"), Var("v")), transitive = false, neg = false),
  //         )),
  //       )
  //     ),
  //     Pattern(
  //       None,
  //       "main",
  //       Seq(Param("out", tyVal)),
  //       Seq(
  //         Body(Seq(
  //           Computed(Var("e"), Evaluation(Seq(), tyExp, Scala(q"""() => Exp.App(Exp.Lam("x", Exp.Var("x")), Exp.Unit())"""))),
  //           Call("interpDefault", Seq(Var("e"), Var("out")), transitive = false, neg = false)
  //         ))
  //       )
  //     )
  //   ),
  //   Seq(
  //     Scala(q"import inca.caseStudies.functional.FunctionalExamples.Env"),
  //     Scala(q"import inca.caseStudies.functional.FunctionalExamples.Exp"),
  //     Scala(q"import inca.caseStudies.functional.FunctionalExamples.Val"))
  // )

  // test("magic sets of lambda interpreter") {
  //   val trans = new Adornment {
  //     override def query: GP.Call = GP.Call("main", Seq(GP.Var("out")), transitive = false, neg = false)
  //     override def adornmentTags: Seq[AdornmentTag] = Seq(Free)
  //   }

  //   // val gpModule = GenerateDatalog.transformModule(AST.factModule)
  //   val adorned = trans.transformer.transformModule(interp)
  //   val magicSet = new MagicSetTransformation {}.transformer.transformModule(adorned)
  //   // assert(moduleEqual(adorned, factModuleGP))
  //   println(magicSet)
  // }

  val tLink = GP.TNode("Link")
  val fromTLink = GP.NamedLink(tLink, "from")
  val toTLink = GP.NamedLink(tLink, "to")
  val tNode = GP.TNode("Node")

  val notreachableModuleGP = GP.Module("Main", Seq(),
    Seq(
      GP.Pattern(None, "reachable", Seq(GP.Param("x", tNode), GP.Param("y", tNode)),
        Seq(
          GP.Body(Seq(
            GP.HasType(GP.Var("link"), tLink),
            GP.Path(GP.Var("link"), tNode, fromTLink, GP.Var("x"), tNode),
            GP.Path(GP.Var("link"), tNode, toTLink, GP.Var("y"), tNode)
          )),
          GP.Body(Seq(
            GP.HasType(GP.Var("link"), tLink),
            GP.Path(GP.Var("link"), tNode, fromTLink, GP.Var("x"), tNode),
            GP.Path(GP.Var("link"), tNode, toTLink, GP.Var("z"), tNode),
            GP.Call("reachable", Seq(GP.Var("z"), GP.Var("y")), transitive = false, neg = false)
          )),
        )
      ),
      GP.Pattern(None, "node", Seq(GP.Param("x", tNode)),
        Seq(
          GP.Body(Seq(
            GP.HasType(GP.Var("link"), tLink),
            GP.Path(GP.Var("link"), tNode, fromTLink, GP.Var("x"), tNode),
          )),
          GP.Body(Seq(
            GP.HasType(GP.Var("link"), tLink),
            GP.Path(GP.Var("link"), tNode, toTLink, GP.Var("x"), tNode),
          )),
        )
      ),
      GP.Pattern(None, "unreachable", Seq(GP.Param("x", tNode), GP.Param("y", tNode)),
        Seq(
          GP.Body(Seq(
            GP.Call("node", Seq(GP.Var("x")), transitive = false, neg = false),
            GP.Call("node", Seq(GP.Var("y")), transitive = false, neg = false),
            GP.Call("reachable", Seq(GP.Var("x"), GP.Var("y")), transitive = false, neg = true),
          ))
        )
      ),
      GP.Pattern(None, "main", Seq(GP.Param("x", tNode), GP.Param("y", tNode)),
        Seq(
          GP.Body(Seq(
            GP.HasType(GP.Var("y"), tNode),
            GP.Eq(GP.Var("y"), GP.Constant(GP.IntLiteral(0))), // this is only temporary
            GP.Call("unreachable", Seq(GP.Var("x"), GP.Var("y")), transitive = false, neg = false),
          ))
        )
      ),
    ),
    Seq())

  val adornedNotreachableModuleGP = GP.Module("Main", Seq(),
    Seq(
      GP.Pattern(None, "reachable_bb", Seq(GP.Param("x", tNode), GP.Param("y", tNode)),
        Seq(
          GP.Body(Seq(
            GP.HasType(GP.Var("link"), tLink),
            GP.Path(GP.Var("link"), tNode, fromTLink, GP.Var("x"), tNode),
            GP.Path(GP.Var("link"), tNode, toTLink, GP.Var("y"), tNode)
          )),
          GP.Body(Seq(
            GP.HasType(GP.Var("link"), tLink),
            GP.Path(GP.Var("link"), tNode, fromTLink, GP.Var("x"), tNode),
            GP.Path(GP.Var("link"), tNode, toTLink, GP.Var("z"), tNode),
            GP.Call("reachable_bb", Seq(GP.Var("z"), GP.Var("y")), transitive = false, neg = false)
          )),
        )
      ),
      GP.Pattern(None, "node_b", Seq(GP.Param("x", tNode)),
        Seq(
          GP.Body(Seq(
            GP.HasType(GP.Var("link"), tLink),
            GP.Path(GP.Var("link"), tNode, fromTLink, GP.Var("x"), tNode),
          )),
          GP.Body(Seq(
            GP.HasType(GP.Var("link"), tLink),
            GP.Path(GP.Var("link"), tNode, toTLink, GP.Var("x"), tNode),
          )),
        )
      ),
      GP.Pattern(None, "node_f", Seq(GP.Param("x", tNode)),
        Seq(
          GP.Body(Seq(
            GP.HasType(GP.Var("link"), tLink),
            GP.Path(GP.Var("link"), tNode, fromTLink, GP.Var("x"), tNode),
          )),
          GP.Body(Seq(
            GP.HasType(GP.Var("link"), tLink),
            GP.Path(GP.Var("link"), tNode, toTLink, GP.Var("x"), tNode),
          )),
        )
      ),
      GP.Pattern(None, "unreachable_fb", Seq(GP.Param("x", tNode), GP.Param("y", tNode)),
        Seq(
          GP.Body(Seq(
            GP.Call("node_f", Seq(GP.Var("x")), transitive = false, neg = false),
            GP.Call("node_b", Seq(GP.Var("y")), transitive = false, neg = false),
            GP.Call("reachable_bb", Seq(GP.Var("x"), GP.Var("y")), transitive = false, neg = true),
          ))
        )
      ),
      GP.Pattern(None, "main_ff", Seq(GP.Param("x", tNode), GP.Param("y", tNode)),
        Seq(
          GP.Body(Seq(
            GP.HasType(GP.Var("y"), tNode),
            GP.Eq(GP.Var("y"), GP.Constant(GP.IntLiteral(0))), // this is only temporary
            GP.Call("unreachable_fb", Seq(GP.Var("x"), GP.Var("y")), transitive = false, neg = false),
          ))
        )
      ),
    ),
    Seq())

  val magicNotreachableModuleGP = GP.Module("Main", Seq(),
    Seq(
      GP.Pattern(None, "reachable_bb", Seq(GP.Param("x", tNode), GP.Param("y", tNode)),
        Seq(
          GP.Body(Seq(
            GP.Call("input_reachable_bb", Seq(GP.Var("x"), GP.Var("y")), transitive = false, neg = false),
            GP.HasType(GP.Var("link"), tLink),
            GP.Path(GP.Var("link"), tNode, fromTLink, GP.Var("x"), tNode),
            GP.Path(GP.Var("link"), tNode, toTLink, GP.Var("y"), tNode)
          )),
          GP.Body(Seq(
            GP.Call("input_reachable_bb", Seq(GP.Var("x"), GP.Var("y")), transitive = false, neg = false),
            GP.HasType(GP.Var("link"), tLink),
            GP.Path(GP.Var("link"), tNode, fromTLink, GP.Var("x"), tNode),
            GP.Path(GP.Var("link"), tNode, toTLink, GP.Var("z"), tNode),
            GP.Call("reachable_bb", Seq(GP.Var("z"), GP.Var("y")), transitive = false, neg = false)
          )),
        )
      ),
      GP.Pattern(None, "node_b", Seq(GP.Param("x", tNode)),
        Seq(
          GP.Body(Seq(
            GP.Call("input_node_b", Seq(GP.Var("x")), transitive = false, neg = false),
            GP.HasType(GP.Var("link"), tLink),
            GP.Path(GP.Var("link"), tNode, fromTLink, GP.Var("x"), tNode),
          )),
          GP.Body(Seq(
            GP.Call("input_node_b", Seq(GP.Var("x")), transitive = false, neg = false),
            GP.HasType(GP.Var("link"), tLink),
            GP.Path(GP.Var("link"), tNode, toTLink, GP.Var("x"), tNode),
          )),
        )
      ),
      GP.Pattern(None, "node_f", Seq(GP.Param("x", tNode)),
        Seq(
          GP.Body(Seq(
            GP.HasType(GP.Var("link"), tLink),
            GP.Path(GP.Var("link"), tNode, fromTLink, GP.Var("x"), tNode),
          )),
          GP.Body(Seq(
            GP.HasType(GP.Var("link"), tLink),
            GP.Path(GP.Var("link"), tNode, toTLink, GP.Var("x"), tNode),
          )),
        )
      ),
      GP.Pattern(None, "unreachable_fb", Seq(GP.Param("x", tNode), GP.Param("y", tNode)),
        Seq(
          GP.Body(Seq(
            GP.Call("input_unreachable_fb", Seq(GP.Var("y")), transitive = false, neg = false),
            GP.Call("node_f", Seq(GP.Var("x")), transitive = false, neg = false),
            GP.Call("node_b", Seq(GP.Var("y")), transitive = false, neg = false),
            GP.Call("reachable_bb", Seq(GP.Var("x"), GP.Var("y")), transitive = false, neg = true),
          ))
        )
      ),
      GP.Pattern(None, "main_ff", Seq(GP.Param("x", tNode), GP.Param("y", tNode)),
        Seq(
          GP.Body(Seq(
            GP.HasType(GP.Var("y"), tNode),
            GP.Eq(GP.Var("y"), GP.Constant(GP.IntLiteral(0))), // this is only temporary
            GP.Call("unreachable_fb", Seq(GP.Var("x"), GP.Var("y")), transitive = false, neg = false),
          ))
        )
      ),
      GP.Pattern(None, "input_unreachable_fb", Seq(GP.Param("y_0", tNode)),
        Seq(
          GP.Body(Seq(
            GP.HasType(GP.Var("y"), tNode),
            GP.Eq(GP.Var("y"), GP.Constant(GP.IntLiteral(0))), // this is only temporary
            GP.Eq(GP.Var("y"), GP.Var("y_0"))
          ))
        )
      ),
      GP.Pattern(None, "input_reachable_bb", Seq(GP.Param("x_0", tNode), GP.Param("y_0", tNode)),
        Seq(
          GP.Body(Seq(
            GP.Call("input_unreachable_fb", Seq(GP.Var("y")), transitive = false, neg = false),
            GP.Call("node_f", Seq(GP.Var("x")), transitive = false, neg = false),
            GP.Call("node_b", Seq(GP.Var("y")), transitive = false, neg = false),
            GP.Eq(GP.Var("x"), GP.Var("x_0")),
            GP.Eq(GP.Var("y"), GP.Var("y_0"))
          )),
          GP.Body(Seq(
            GP.Call("input_reachable_bb", Seq(GP.Var("x"), GP.Var("y")), transitive = false, neg = false),
            GP.HasType(GP.Var("link"), tLink),
            GP.Path(GP.Var("link"), tNode, fromTLink, GP.Var("x"), tNode),
            GP.Path(GP.Var("link"), tNode, toTLink, GP.Var("z"), tNode),
            GP.Eq(GP.Var("z"), GP.Var("x_0")),
            GP.Eq(GP.Var("y"), GP.Var("y_0"))
          ))
        )
      ),
      GP.Pattern(None, "input_node_b", Seq(GP.Param("x_0", tNode)),
        Seq(
          GP.Body(Seq(
            GP.Call("input_unreachable_fb", Seq(GP.Var("y")), transitive = false, neg = false),
            GP.Call("node_f", Seq(GP.Var("x")), transitive = false, neg = false),
            GP.Eq(GP.Var("y"), GP.Var("x_0"))
          ))
        )
      ),
    ),
    Seq())

  test("magic sets of not reachable") {
    val trans = new Adornment {
      override def query: GP.Call = GP.Call("main", Seq(GP.Var("x"), GP.Var("y")), transitive = false, neg = false)
      override def adornmentTags: Seq[AdornmentTag] = Seq(Free, Free)
    }
    val adorned = trans.transformer.transformModule(notreachableModuleGP)
    val magicSet = new MagicSetTransformation {}.transformer.transformModule(adorned)
    assert(moduleEqual(magicSet, magicNotreachableModuleGP))
  }
}
