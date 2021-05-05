package inca.caseStudies.functional

import inca.backend.ir.GP._
import inca.backend.ir.Printer
import inca.compiler.{Compiler, Options}
import inca.runtime.EnginePool
import inca.runtime.context.{DataModel, QueryScope}
import inca.util.Meta.Scala
import org.eclipse.viatra.query.runtime.rete.matcher.TimelyReteBackendFactory

import scala.meta.quasiquotes._


object FunctionalExamples extends App {
  val options = Options(new DataModel())

  val inc = Module("Inc", Seq(), Seq(),
    Seq(
      Pattern(
        None,
        "inc",
        Seq(Param("n", TScalaInt), Param("out", TScalaInt)),
        Seq(
          Body(Seq(
            Call("input_inc", Seq(Var("n")), transitive = false, neg = false),
            Computed(Var("out"), Evaluation(Seq((Var("n"), TScalaInt)), TScalaInt, Scala(q"(n: Int) => n + 1")))
          ))
        )
      ),
      Pattern(
        None,
        "input_inc",
        Seq(Param("n", TScalaInt)),
        Seq(
          Body(Seq(
            Compare(EqComparator, Var("n"), Constant(IntLiteral(0)))
          ))
        )
      ),
      Pattern(
        None,
        "main",
        Seq(Param("out", TScalaInt)),
        Seq(
          Body(Seq(
            Call("inc", Seq(Constant(IntLiteral(0)), Var("out")), transitive = false, neg = false)
          ))
        )
      )
    ),
    Seq()
  )

  val fact = Module("Fact", Seq(), Seq(),
    Seq(
      Pattern(
        None,
        "fact",
        Seq(Param("n", TScalaInt), Param("out", TScalaInt)),
        Seq(
          Body(Seq(
            Call("input_fact", Seq(Var("n")), transitive = false, neg = false),
            Compare(EqComparator, Var("n"), Constant(IntLiteral(1))),
            Compare(EqComparator, Var("out"), Constant(IntLiteral(1)))
            // Computed(Var("out"), Evaluation(Seq((Var("n"), TScalaInt)), TScalaInt, Scala(q"(n: Int) => n + 1")))
          )),
          Body(Seq(
            Call("input_fact", Seq(Var("n")), transitive = false, neg = false),
            Compare(NeqComparator, Var("n"), Constant(IntLiteral(1))),
            Computed(Var("subn1"), Evaluation(Seq((Var("n"), TScalaInt)), TScalaInt, Scala(q"(n: Int) => n - 1"))),
            Call("fact", Seq(Var("subn1"), Var("m")), transitive = false, neg = false),
            Computed(Var("out"), Evaluation(Seq((Var("n"), TScalaInt), (Var("m"), TScalaInt)), TScalaInt, Scala(q"(n: Int, m: Int) => n * m")))
          ))
        )
      ),
      Pattern(
        None,
        "input_fact",
        Seq(Param("n", TScalaInt)),
        Seq(
          Body(Seq(
            Compare(EqComparator, Var("n"), Constant(IntLiteral(3)))
          )),
          Body(Seq(
            Call("input_fact", Seq(Var("p")), transitive = false, neg = false),
            Compare(NeqComparator, Var("p"), Constant(IntLiteral(1))),
            Computed(Var("n"), Evaluation(Seq((Var("p"), TScalaInt)), TScalaInt, Scala(q"(p : Int) => p - 1"))),
          )),
        )
      ),
      Pattern(
        None,
        "main",
        Seq(Param("out", TScalaInt)),
        Seq(
          Body(Seq(
            Call("fact", Seq(Constant(IntLiteral(3)), Var("out")), transitive = false, neg = false)
          ))
        )
      )
    ),
    Seq()
  )

  trait Env
  object Env {
    case class Empty() extends Env
    case class Bind(x: String, v: Val, rest: Env) extends Env
  }
  trait Exp
  object Exp {
    case class Unit() extends Exp
    case class App(e1: Exp, e2: Exp) extends Exp
    case class Lam(name: String, e: Exp) extends Exp
    case class Var(name: String) extends Exp
  }
  trait Val
  object Val {
    case class VUnit() extends Val
    case class VClosure(name: String, exp: Exp, env: Env) extends Val
  }

  val tyEnv = TScala(Scala(t"inca.caseStudies.functional.FunctionalExamples.Env"))
  val tyExp = TScala(Scala(t"inca.caseStudies.functional.FunctionalExamples.Exp"))
  val tyVal = TScala(Scala(t"inca.caseStudies.functional.FunctionalExamples.Val"))
  val interp = Module("LambdaCalcInterp", Seq(), Seq(),
    Seq(
      Pattern(
        None,
        "interpDefault",
        Seq(Param("e", tyExp), Param("out", tyVal)),
        Seq(
          Body(Seq(
            Call("input_interpDefault", Seq(Var("e")), transitive = false, neg = false),
            Computed(Var("env"), Evaluation(Seq(), tyEnv, Scala(q"() => Env.Empty()"))),
            Call("interp", Seq(Var("env"), Var("e"), Var("out")), transitive = false, neg = false)
          ))
        )
      ),
      Pattern(
        None,
        "interp",
        Seq(Param("env", tyEnv), Param("e", tyExp), Param("out", tyVal)),
        Seq(
          Body(Seq(
            Call("input_interp", Seq(Var("env"), Var("e")), transitive = false, neg = false),
            Computed(Var("isUnit"), Evaluation(Seq((Var("e"), tyExp)), TScalaBoolean, Scala(q"(e: Exp) => e match { case Exp.Unit() => true; case _ => false }"))),
            Compare(EqComparator, Var("isUnit"), Constant(BooleanLiteral(true))),
            Computed(Var("out"), Evaluation(Seq(), tyVal, Scala(q"() => Val.VUnit()")))
          )),
          Body(Seq(
            Call("input_interp", Seq(Var("env"), Var("e")), transitive = false, neg = false),
            Computed(Var("isUnit"), Evaluation(Seq((Var("e"), tyExp)), TScalaBoolean, Scala(q"(e: Exp) => e match { case Exp.Unit() => true; case _ => false }"))),
            Compare(EqComparator, Var("isUnit"), Constant(BooleanLiteral(false))),
            Computed(Var("isApp"), Evaluation(Seq((Var("e"), tyExp)), TScalaBoolean, Scala(q"(e: Exp) => e match { case Exp.App(_, _) => true; case _ => false }"))),
            Compare(EqComparator, Var("isApp"), Constant(BooleanLiteral(true))),
            Computed(Var("e1"), Evaluation(Seq((Var("e"), tyExp)), tyExp, Scala(q"(e: Exp) => e match { case Exp.App(e1, _) => e1; case _ => null }"))),
            Call("interp", Seq(Var("env"), Var("e1"), Var("out1")), transitive = false, neg = false),
            Computed(Var("isVClosure"), Evaluation(Seq((Var("out1"), tyVal)), TScalaBoolean, Scala(q"(v: Val) => v match { case Val.VClosure(_, _, _) => true; case _ => false }"))),
            Compare(EqComparator, Var("isVClosure"), Constant(BooleanLiteral(true))),
            Computed(Var("e2"), Evaluation(Seq((Var("e"), tyExp)), tyExp, Scala(q"(e: Exp) => e match { case Exp.App(_, e2) => e2; case _ => null }"))),
            Call("interp", Seq(Var("env"), Var("e2"), Var("out2")), transitive = false, neg = false),
            Computed(Var("name"), Evaluation(Seq((Var("out1"), tyVal)), TScalaString, Scala(q"(v: Val) => v match { case Val.VClosure(name, _, _) => name; case _ => null }"))),
            Computed(Var("body"), Evaluation(Seq((Var("out1"), tyVal)), tyExp, Scala(q"(v: Val) => v match { case Val.VClosure(_, body, _) => body; case _ => null }"))),
            Computed(Var("fenv"), Evaluation(Seq((Var("out1"), tyVal)), tyEnv, Scala(q"(v: Val) => v match { case Val.VClosure(_, _, env) => env; case _ => null }"))),
            Computed(Var("extEnv"), Evaluation(Seq((Var("name"), TScalaString), (Var("out2"), tyVal), (Var("fenv"), tyEnv)), tyEnv, Scala(q"(name: String, v: Val, env: Env) => Env.Bind(name, v, env)"))),
            Call("interp", Seq(Var("extEnv"), Var("body"), Var("out")), transitive = false, neg = false),
          )),
          Body(Seq(
            Call("input_interp", Seq(Var("env"), Var("e")), transitive = false, neg = false),
            Computed(Var("isUnit"), Evaluation(Seq((Var("e"), tyExp)), TScalaBoolean, Scala(q"(e: Exp) => e match { case Exp.Unit() => true; case _ => false }"))),
            Compare(EqComparator, Var("isUnit"), Constant(BooleanLiteral(false))),
            Computed(Var("isApp"), Evaluation(Seq((Var("e"), tyExp)), TScalaBoolean, Scala(q"(e: Exp) => e match { case Exp.App(_, _) => true; case _ => false }"))),
            Compare(EqComparator, Var("isApp"), Constant(BooleanLiteral(false))),
            Computed(Var("isLam"), Evaluation(Seq((Var("e"), tyExp)), TScalaBoolean, Scala(q"(e: Exp) => e match { case Exp.Lam(_, _) => true; case _ => false }"))),
            Compare(EqComparator, Var("isLam"), Constant(BooleanLiteral(true))),
            Computed(Var("name"), Evaluation(Seq((Var("e"), tyExp)), TScalaString, Scala(q"(e: Exp) => e match { case Exp.Lam(name, _) => name; case _ => null }"))),
            Computed(Var("body"), Evaluation(Seq((Var("e"), tyExp)), tyExp, Scala(q"(e: Exp) => e match { case Exp.Lam(_, body) => body; case _ => null }"))),
            Computed(Var("out"), Evaluation(Seq((Var("name"), TScalaString), (Var("body"), tyExp), (Var("env"), tyEnv)), tyVal, Scala(q"(name: String, exp: Exp, env: Env) => Val.VClosure(name, exp, env)"))),
          )),
          Body(Seq(
            Call("input_interp", Seq(Var("env"), Var("e")), transitive = false, neg = false),
            Computed(Var("isUnit"), Evaluation(Seq((Var("e"), tyExp)), TScalaBoolean, Scala(q"(e: Exp) => e match { case Exp.Unit() => true; case _ => false }"))),
            Compare(EqComparator, Var("isUnit"), Constant(BooleanLiteral(false))),
            Computed(Var("isApp"), Evaluation(Seq((Var("e"), tyExp)), TScalaBoolean, Scala(q"(e: Exp) => e match { case Exp.App(_, _) => true; case _ => false }"))),
            Compare(EqComparator, Var("isApp"), Constant(BooleanLiteral(false))),
            Computed(Var("isLam"), Evaluation(Seq((Var("e"), tyExp)), TScalaBoolean, Scala(q"(e: Exp) => e match { case Exp.Lam(_, _) => true; case _ => false }"))),
            Compare(EqComparator, Var("isLam"), Constant(BooleanLiteral(false))),
            Computed(Var("isVar"), Evaluation(Seq((Var("e"), tyExp)), TScalaBoolean, Scala(q"(e: Exp) => e match { case Exp.Var(_) => true; case _ => false }"))),
            Compare(EqComparator, Var("isVar"), Constant(BooleanLiteral(true))),
            Computed(Var("name"), Evaluation(Seq((Var("e"), tyExp)), TScalaString, Scala(q"(exp: Exp) => exp match { case Exp.Var(name) => name; case _ => null }"))),
            Call("lookup", Seq(Var("env"), Var("name"), Var("out")), transitive = false, neg = false),
          ))
        )
      ),
      Pattern(
        None,
        "lookup",
        Seq(Param("env", tyEnv), Param("name", TScalaString), Param("v", tyVal)),
        Seq(
          Body(Seq(
            Call("input_lookup", Seq(Var("env"), Var("name")), transitive = false, neg = false),
            Computed(Var("name1"), Evaluation(Seq((Var("env"), tyEnv)), TScalaString, Scala(q"(env: Env) => env match { case Env.Empty() => null; case Env.Bind(x, v, rest) => x }"))),
            Computed(Var("v1"), Evaluation(Seq((Var("env"), tyEnv)), tyVal, Scala(q"(env: Env) => env match { case Env.Empty() => null; case Env.Bind(x, v, rest) => v }"))),
            Computed(Var("rest"), Evaluation(Seq((Var("env"), tyEnv)), tyEnv, Scala(q"(env: Env) => env match { case Env.Empty() => null; case Env.Bind(_, _, rest) => rest }"))),
            Compare(EqComparator, Var("name"), Var("name1")),
            Compare(EqComparator, Var("v"), Var("v1")),
          )),
          Body(Seq(
            Call("input_lookup", Seq(Var("env"), Var("name")), transitive = false, neg = false),
            Computed(Var("name1"), Evaluation(Seq((Var("env"), tyEnv)), TScalaString, Scala(q"(env: Env) => env match { case Env.Empty() => null; case Env.Bind(x, v, rest) => x }"))),
            Computed(Var("v1"), Evaluation(Seq((Var("env"), tyEnv)), tyVal, Scala(q"(env: Env) => env match { case Env.Empty() => null; case Env.Bind(x, v, rest) => v }"))),
            Computed(Var("rest"), Evaluation(Seq((Var("env"), tyEnv)), tyEnv, Scala(q"(env: Env) => env match { case Env.Empty() => null; case Env.Bind(_, _, rest) => rest }"))),
            Compare(NeqComparator, Var("name"), Var("name1")),
            Call("lookup", Seq(Var("rest"), Var("name"), Var("v")), transitive = false, neg = false),
          )),
        )
      ),
      Pattern(
        None,
        "input_interpDefault",
        Seq(Param("e", tyExp)),
        Seq(
          Body(Seq(
            Computed(Var("e"), Evaluation(Seq(), tyExp, Scala(q"""() => Exp.App(Exp.Lam("x", Exp.Var("x")), Exp.Unit())"""))),
          )),
        )
      ),
      Pattern(
        None,
        "input_interp",
        Seq(Param("env", tyEnv), Param("e", tyExp)),
        Seq(
          Body(Seq(
            Call("input_interpDefault", Seq(Var("e")), transitive = false, neg = false),
            Computed(Var("env"), Evaluation(Seq(), tyEnv, Scala(q"() => Env.Empty()")))
          )),
          Body(Seq(
            Call("input_interp", Seq(Var("env"), Var("pe")), transitive = false, neg = false),
            Computed(Var("isUnit"), Evaluation(Seq((Var("pe"), tyExp)), TScalaBoolean, Scala(q"(e: Exp) => e match { case Exp.Unit() => true; case _ => false }"))),
            Compare(EqComparator, Var("isUnit"), Constant(BooleanLiteral(false))),
            Computed(Var("isApp"), Evaluation(Seq((Var("pe"), tyExp)), TScalaBoolean, Scala(q"(e: Exp) => e match { case Exp.App(_, _) => true; case _ => false }"))),
            Compare(EqComparator, Var("isApp"), Constant(BooleanLiteral(true))),
            Computed(Var("e"), Evaluation(Seq((Var("pe"), tyExp)), tyExp, Scala(q"(e: Exp) => e match { case Exp.App(e1, _) => e1; case _ => null }"))),
          )),
          Body(Seq(
            Call("input_interp", Seq(Var("env"), Var("pe")), transitive = false, neg = false),
            Computed(Var("isUnit"), Evaluation(Seq((Var("pe"), tyExp)), TScalaBoolean, Scala(q"(e: Exp) => e match { case Exp.Unit() => true; case _ => false }"))),
            Compare(EqComparator, Var("isUnit"), Constant(BooleanLiteral(false))),
            Computed(Var("isApp"), Evaluation(Seq((Var("pe"), tyExp)), TScalaBoolean, Scala(q"(e: Exp) => e match { case Exp.App(_, _) => true; case _ => false }"))),
            Compare(EqComparator, Var("isApp"), Constant(BooleanLiteral(true))),
            Computed(Var("e1"), Evaluation(Seq((Var("pe"), tyExp)), tyExp, Scala(q"(e: Exp) => e match { case Exp.App(e1, _) => e1; case _ => null }"))),
            Call("interp", Seq(Var("env"), Var("e1"), Var("out1")), transitive = false, neg = false),
            Computed(Var("isVClosure"), Evaluation(Seq((Var("out1"), tyVal)), TScalaBoolean, Scala(q"(v: Val) => v match { case Val.VClosure(_, _, _) => true; case _ => false }"))),
            Compare(EqComparator, Var("isVClosure"), Constant(BooleanLiteral(true))),
            Computed(Var("e"), Evaluation(Seq((Var("pe"), tyExp)), tyExp, Scala(q"(e: Exp) => e match { case Exp.App(_, e2) => e2; case _ => null }"))),
          )),
          Body(Seq(
            Call("input_interp", Seq(Var("penv"), Var("pe")), transitive = false, neg = false),
            Computed(Var("isUnit"), Evaluation(Seq((Var("pe"), tyExp)), TScalaBoolean, Scala(q"(e: Exp) => e match { case Exp.Unit() => true; case _ => false }"))),
            Compare(EqComparator, Var("isUnit"), Constant(BooleanLiteral(false))),
            Computed(Var("isApp"), Evaluation(Seq((Var("pe"), tyExp)), TScalaBoolean, Scala(q"(e: Exp) => e match { case Exp.App(_, _) => true; case _ => false }"))),
            Compare(EqComparator, Var("isApp"), Constant(BooleanLiteral(true))),
            Computed(Var("e1"), Evaluation(Seq((Var("pe"), tyExp)), tyExp, Scala(q"(e: Exp) => e match { case Exp.App(e1, _) => e1; case _ => null }"))),
            Call("interp", Seq(Var("penv"), Var("e1"), Var("out1")), transitive = false, neg = false),
            Computed(Var("isVClosure"), Evaluation(Seq((Var("out1"), tyVal)), TScalaBoolean, Scala(q"(v: Val) => v match { case Val.VClosure(_, _, _) => true; case _ => false }"))),
            Compare(EqComparator, Var("isVClosure"), Constant(BooleanLiteral(true))),
            Computed(Var("e2"), Evaluation(Seq((Var("pe"), tyExp)), tyExp, Scala(q"(e: Exp) => e match { case Exp.App(_, e2) => e2; case _ => null }"))),
            Call("interp", Seq(Var("penv"), Var("e2"), Var("out2")), transitive = false, neg = false),
            Computed(Var("name"), Evaluation(Seq((Var("out1"), tyVal)), TScalaString, Scala(q"(v: Val) => v match { case Val.VClosure(name, _, _) => name; case _ => null }"))),
            Computed(Var("e"), Evaluation(Seq((Var("out1"), tyVal)), tyExp, Scala(q"(v: Val) => v match { case Val.VClosure(_, body, _) => body; case _ => null }"))),
            Computed(Var("fenv"), Evaluation(Seq((Var("out1"), tyVal)), tyEnv, Scala(q"(v: Val) => v match { case Val.VClosure(_, _, env) => env; case _ => null }"))),
            Computed(Var("env"), Evaluation(Seq((Var("name"), TScalaString), (Var("out2"), tyVal), (Var("fenv"), tyEnv)), tyEnv, Scala(q"(name: String, v: Val, env: Env) => Env.Bind(name, v, env)"))),
          )),
        )
      ),
      Pattern(
        None,
        "input_lookup",
        Seq(Param("env", tyEnv), Param("name", TScalaString)),
        Seq(
          Body(Seq(
            Call("input_interp", Seq(Var("env"), Var("e")), transitive = false, neg = false),
            Computed(Var("isUnit"), Evaluation(Seq((Var("e"), tyExp)), TScalaBoolean, Scala(q"(e: Exp) => e match { case Exp.Unit() => true; case _ => false }"))),
            Compare(EqComparator, Var("isUnit"), Constant(BooleanLiteral(false))),
            Computed(Var("isApp"), Evaluation(Seq((Var("e"), tyExp)), TScalaBoolean, Scala(q"(e: Exp) => e match { case Exp.App(_, _) => true; case _ => false }"))),
            Compare(EqComparator, Var("isApp"), Constant(BooleanLiteral(false))),
            Computed(Var("isLam"), Evaluation(Seq((Var("e"), tyExp)), TScalaBoolean, Scala(q"(e: Exp) => e match { case Exp.Lam(_, _) => true; case _ => false }"))),
            Compare(EqComparator, Var("isLam"), Constant(BooleanLiteral(false))),
            Computed(Var("isVar"), Evaluation(Seq((Var("e"), tyExp)), TScalaBoolean, Scala(q"(e: Exp) => e match { case Exp.Var(_) => true; case _ => false }"))),
            Compare(EqComparator, Var("isVar"), Constant(BooleanLiteral(true))),
            Computed(Var("name"), Evaluation(Seq((Var("e"), tyExp)), TScalaString, Scala(q"(exp: Exp) => exp match { case Exp.Var(name) => name; case _ => null }"))),
          )),
          Body(Seq(
            Call("input_lookup", Seq(Var("penv"), Var("name")), transitive = false, neg = false),
            Computed(Var("name1"), Evaluation(Seq((Var("penv"), tyEnv)), TScalaString, Scala(q"(env: Env) => env match { case Env.Empty() => null; case Env.Bind(x, v, rest) => x }"))),
            Computed(Var("v1"), Evaluation(Seq((Var("penv"), tyEnv)), tyVal, Scala(q"(env: Env) => env match { case Env.Empty() => null; case Env.Bind(x, v, rest) => v }"))),
            Computed(Var("env"), Evaluation(Seq((Var("penv"), tyEnv)), tyEnv, Scala(q"(env: Env) => env match { case Env.Empty() => null; case Env.Bind(_, _, rest) => rest }"))),
            Compare(NeqComparator, Var("name"), Var("name1")),
          )),
        )
      ),
      Pattern(
        None,
        "main",
        Seq(Param("out", tyVal)),
        Seq(
          Body(Seq(
            Computed(Var("e"), Evaluation(Seq(), tyExp, Scala(q"""() => Exp.App(Exp.Lam("x", Exp.Var("x")), Exp.Unit())"""))),
            Call("interpDefault", Seq(Var("e"), Var("out")), transitive = false, neg = false)
          ))
        )
      )
    ),
    Seq(
      Scala(q"import inca.caseStudies.functional.FunctionalExamples.Env"),
      Scala(q"import inca.caseStudies.functional.FunctionalExamples.Exp"),
      Scala(q"import inca.caseStudies.functional.FunctionalExamples.Val"))
  )
  println(Printer.prettyModule(interp))
  //  val compiled = Compiler.compileGP(inc, options)
  //  val compiled = Compiler.compileGP(fact, options)
  val compiled = Compiler.compileGP(interp, options)

  val scope = new QueryScope(options.languageMetaInfo)
  val (engine, feed) = EnginePool.loadEngineAndDatabase(scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)
  val matcher = EnginePool.loadQuery(compiled.psystemModule.patterns("main")(), scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)

  println("START")
  println(matcher.getAllMatches)
}
