package inca.backend.transform.magic

import inca.backend.ir.GP
import inca.backend.transform.magic.Examples._
import inca.frontend.examples.AST
import inca.frontend.lowering.GenerateDatalog
import org.scalatest.funsuite.AnyFunSuite

class MagicSetTransformationTest extends AnyFunSuite {

  def moduleEqual(m1: GP.Module, m2: GP.Module): Boolean =
    m1.pats.size == m2.pats.size && m1.pats.forall(m2.pats.contains)

  test("magic sets of flat function") {
    val magicSet = MagicSetTransformation.transformer.transformModule(adornedIncModuleGP)
    assert(moduleEqual(magicSet, magicIncModuleGP))
  }

  test("magic sets of recursive function") {
    val magicSet = MagicSetTransformation.transformer.transformModule(adornedFactModuleGP)
    assert(moduleEqual(magicSet, magicFactModuleGP))
  }

  test("magic sets of not reachable") {
    val magicSet = MagicSetTransformation.transformer.transformModule(adornedUnreachableModuleGP)
    assert(moduleEqual(magicSet, magicUnreachableModuleGP))
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
}
