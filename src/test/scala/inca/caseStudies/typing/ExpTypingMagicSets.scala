package inca.caseStudies.typing

import inca.compiler.{Compiler, Options}
import inca.runtime.EnginePool
import inca.runtime.context.QueryScope
import inca.util.Meta.Scala
import inca.backend.ir.GP._
import inca.backend.ir.Printer
import org.eclipse.viatra.query.runtime.rete.matcher.TimelyReteBackendFactory
import truediff.Diffable

import scala.meta.quasiquotes._


object ExpTypingMagicSets extends App {

  val options = Options(Prog.languageMetaInfo)

  val ExpT = Exp.expTag
  val IntLit = Exp.intLitTag
  val Add = Exp.addTag
  val VarT = Exp.varTag
  val Lam = Exp.lamTag
  val App = Exp.appTag
  val Let = Exp.letTag
  val ProgT = Prog.progTag
  val TypeExpT = TypeExp.typeExpTag
  val IntTypeExpT = TypeExp.intTag
  val FunTypeExpT = TypeExp.funTag

  val oldCode =
    s"""
       |module ExpTyping
       |
       |`import inca.caseStudies.typing.Type`
       |`import inca.caseStudies.typing.Context`
       |
       |def check(ctx: `Context`, e: $ExpT): `Type` = e match {
       |  case $IntLit() => yield `Type.Int`
       |  case $Add(e1, e2) =>
       |    if (check(ctx, e1) == `Type.Int` &&
       |        check(ctx, e2) == `Type.Int`)
       |      yield `Type.Int`
       |    else
       |      fail
       |  case v@$VarT() => yield lookup(ctx, v)
       |  case $Lam(name, ty, body) => {
       |    val bodyCtx = `Context.Bind`(name, ty, ctx)
       |    val bodyTy = check(bodyCtx, body)
       |    yield `Type.Fun`(ty, bodyTy)
       |  }
       |  case $App(e1, e2) => {
       |    val funTy = check(ctx, e1)
       |    val argTy = check(ctx, e2)
       |    funTy match {
       |      case `Type.Fun`(paramTy, bodyTy) =>
       |        if (paramTy == argTy)
       |          yield bodyTy
       |        else
       |          fail
       |      case _ => fail
       |    }
       |  }
       |  case $Let(name, bound, body) => {
       |    val boundTy = check(ctx, bound)
       |    val bodyCtx = `Context.Bind`(name, boundTy, ctx)
       |    yield check(bodyCtx, body)
       |  }
       |}
       |
       |def lookup(ctx: `Context`, v: $VarT): `Type` = ctx match {
       |  case `Context.Empty` => fail
       |  case `Context.Bind`(name, ty, rest) =>
       |    if (name == v.name)
       |      yield ty
       |    else
       |      yield lookup(rest, v)
       |}
       |""".stripMargin

  val tyContext = TScala(Scala(t"inca.caseStudies.typing.Context"))
  val tyType = TScala(Scala(t"inca.caseStudies.typing.Type"))

  val gp = Module("Typing", Seq(),
    Seq(
      Pattern(
        None,
        "input_ok_b",
        Seq(Param("p", TNode(ProgT))),
        Seq(Body(Seq(HasType(Var("p"), TNode(ProgT)))))
      ),
      Pattern(
        None,
        "input_typed_bbf",
        Seq(Param("ctx", tyContext), Param("e", TNode(ExpT))),
        Seq(
          Body(Seq(Call("sup_ok_b_0_2", Seq(Var("p"), Var("e"), Var("ctx")), transitive = false, neg = false))),
          Body(Seq(Call("sup_typed_bbf_2_1", Seq(Var("ctx"), Var("pe"), Var("e"), Var("e2")), transitive = false, neg = false))),
          Body(Seq(Call("sup_typed_bbf_2_3", Seq(Var("ctx"), Var("pe"), Var("e1"), Var("e"), Var("FunT"), Var("T1"), Var("T2")), transitive = false, neg = false))),
          Body(Seq(Call("sup_typed_bbf_3_2", Seq(Var("prevCtx"), Var("pe"), Var("x"), Var("T1"), Var("e"), Var("ctx")), transitive = false, neg = false))),
        )
      ),
      Pattern(
        None,
        "input_lookup_bbf",
        Seq(Param("ctx", tyContext), Param("x", TLiteral.String)),
        Seq(
          Body(Seq(Call("sup_typed_bbf_4_1", Seq(Var("ctx"), Var("e"), Var("x")), transitive = false, neg = false))),
          Body(Seq(Call("sup_lookup_bbf_6_1", Seq(Var("prevCtx"), Var("x"), Var("x1"), Var("T1"), Var("ctx")), transitive = false, neg = false))),
        )
      ),
      // supplementary for ok_b
      Pattern(
        None,
        "sup_ok_b_0_0",
        Seq(Param("p", TNode(ProgT))),
        Seq(
          Body(Seq(Call("input_ok_b", Seq(Var("p")), transitive = false, neg = false)))
        )
      ),
      Pattern(
        None,
        "sup_ok_b_0_1",
        Seq(Param("p", TNode(ProgT)), Param("e", TNode(ExpT))),
        Seq(
          Body(Seq(
            Call("sup_ok_b_0_0", Seq(Var("p")), transitive = false, neg = false),
            Path(Var("p"), TNode(ProgT), NamedLink(TNode(ProgT), "e"), Var("e"), TNode(ExpT))
          ))
        )
      ),
      Pattern(
        None,
        "sup_ok_b_0_2",
        Seq(Param("p", TNode(ProgT)), Param("e", TNode(ExpT)), Param("ctx", tyContext)),
        Seq(
          Body(Seq(
            Call("sup_ok_b_0_1", Seq(Var("p"), Var("e")), transitive = false, neg = false),
            Computed(Var("ctx"), Evaluation(Seq(), tyContext, Scala(q"() => Context.Empty")))
          ))
        )
      ),
      // supplementary for typed_bbf
      // first rule int
      Pattern(
        None,
        "sup_typed_bbf_1_0",
        Seq(Param("ctx", tyContext), Param("e", TNode(ExpT))),
        Seq(
          Body(Seq(
            Call("input_typed_bbf", Seq(Var("ctx"), Var("e")), transitive = false, neg = false)
          ))
        )
      ),
      Pattern(
        None,
        "sup_typed_bbf_1_1",
        Seq(Param("ctx", tyContext), Param("e", TNode(ExpT))),
        Seq(
          Body(Seq(
            Call("sup_typed_bbf_1_0", Seq(Var("ctx"), Var("e")), transitive = false, neg = false),
            // TODO is this correct?
            HasType(Var("e"), TNode(IntLit))
          ))
        )
      ),
      // second rule app
      Pattern(
        None,
        "sup_typed_bbf_2_0",
        Seq(Param("ctx", tyContext), Param("e", TNode(ExpT))),
        Seq(
          Body(Seq(
            Call("input_typed_bbf", Seq(Var("ctx"), Var("e")), transitive = false, neg = false)
          ))
        )
      ),
      Pattern(
        None,
        "sup_typed_bbf_2_1",
        Seq(Param("ctx", tyContext), Param("e", TNode(ExpT)), Param("e1", TNode(ExpT)), Param("e2", TNode(ExpT))),
        Seq(
          Body(Seq(
            Call("sup_typed_bbf_2_0", Seq(Var("ctx"), Var("e")), transitive = false, neg = false),
            Path(Var("e"), TNode(App), NamedLink(TNode(App), "e1"), Var("e1"), TNode(ExpT)),
            Path(Var("e"), TNode(App), NamedLink(TNode(App), "e2"), Var("e2"), TNode(ExpT))
          ))
        )
      ),
      Pattern(
        None,
        "sup_typed_bbf_2_2",
        Seq(Param("ctx", tyContext), Param("e", TNode(ExpT)), Param("e1", TNode(ExpT)), Param("e2", TNode(ExpT)), Param("FunT", tyType)),
        Seq(
          Body(Seq(
            Call("sup_typed_bbf_2_1", Seq(Var("ctx"), Var("e"), Var("e1"), Var("e2")), transitive = false, neg = false),
            Call("typed_bbf", Seq(Var("ctx"), Var("e1"), Var("FunT")), transitive = false, neg = false)
          ))
        )
      ),
      Pattern(
        None,
        "sup_typed_bbf_2_3",
        Seq(Param("ctx", tyContext), Param("e", TNode(ExpT)), Param("e1", TNode(ExpT)), Param("e2", TNode(ExpT)), Param("FunT", tyType), Param("T1", tyType), Param("T2", tyType)),
        Seq(
          Body(Seq(
            Call("sup_typed_bbf_2_2", Seq(Var("ctx"), Var("e"), Var("e1"), Var("e2"), Var("FunT")), transitive = false, neg = false),
            // TODO extract T1 and T2 correct?
            Computed(Var("T1"), Evaluation(Seq((Var("FunT"), tyType)), tyType, Scala(q"(funty: Type) => funty match { case Type.Fun(ty1, ty2) => ty1; case _ => null }"))),
            Computed(Var("T2"), Evaluation(Seq((Var("FunT"), tyType)), tyType, Scala(q"(funty: Type) => funty match { case Type.Fun(ty1, ty2) => ty2; case _ => null }")))
          ))
        )
      ),
      // third rule lam
      Pattern(
        None,
        "sup_typed_bbf_3_0",
        Seq(Param("ctx", tyContext), Param("e", TNode(ExpT))),
        Seq(
          Body(Seq(
            Call("input_typed_bbf", Seq(Var("ctx"), Var("e")), transitive = false, neg = false)
          ))
        )
      ),
      Pattern(
        None,
        "sup_typed_bbf_3_1",
        Seq(Param("ctx", tyContext), Param("e", TNode(ExpT)), Param("x", TLiteral.String), Param("T1", TNode(TypeExpT)), Param("b", TNode(ExpT))),
        Seq(
          Body(Seq(
            Call("sup_typed_bbf_3_0", Seq(Var("ctx"), Var("e")), transitive = false, neg = false),
            Path(Var("e"), TNode(Lam), NamedLink(TNode(Lam), "name"), Var("x"), TLiteral.String),
            Path(Var("e"), TNode(Lam), NamedLink(TNode(Lam), "ty"), Var("T1"), TNode(TypeExpT)),
            Path(Var("e"), TNode(Lam), NamedLink(TNode(Lam), "body"), Var("b"), TNode(ExpT)),
          ))
        )
      ),
      Pattern(
        None,
        "sup_typed_bbf_3_2",
        Seq(Param("ctx", tyContext), Param("e", TNode(ExpT)), Param("x", TLiteral.String), Param("T1", TNode(TypeExpT)), Param("b", TNode(ExpT)), Param("extCtx", tyContext)),
        Seq(
          Body(Seq(
            Call("sup_typed_bbf_3_1", Seq(Var("ctx"), Var("e"), Var("x"), Var("T1"), Var("b")), transitive = false, neg = false),
            Call("conv_typeExp", Seq(Var("T1"), Var("convT1")), transitive = false, neg = false),
            Computed(Var("extCtx"), Evaluation(Seq((Var("x"), TLiteral.String), (Var("convT1"), tyType), (Var("ctx"), tyContext)), tyContext, Scala(q"(x: String, ty: Type, ctx: Context) => Context.Bind(x, ty, ctx)")))
          ))
        )
      ),
      Pattern(
        None,
        "sup_typed_bbf_3_3",
        Seq(Param("ctx", tyContext), Param("e", TNode(ExpT)), Param("x", TLiteral.String), Param("T1", TNode(TypeExpT)), Param("b", TNode(ExpT)), Param("extCtx", tyContext), Param("T2", tyType)),
        Seq(
          Body(Seq(
            Call("sup_typed_bbf_3_2", Seq(Var("ctx"), Var("e"), Var("x"), Var("T1"), Var("b"), Var("extCtx")), transitive = false, neg = false),
            Call("typed_bbf", Seq(Var("extCtx"), Var("b"), Var("T2")), transitive = false, neg = false),
          ))
        )
      ),
      // fourth rule var
      Pattern(
        None,
        "sup_typed_bbf_4_0",
        Seq(Param("ctx", tyContext), Param("e", TNode(ExpT))),
        Seq(
          Body(Seq(
            Call("input_typed_bbf", Seq(Var("ctx"), Var("e")), transitive = false, neg = false)
          ))
        )
      ),
      Pattern(
        None,
        "sup_typed_bbf_4_1",
        Seq(Param("ctx", tyContext), Param("e", TNode(ExpT)), Param("x", TLiteral.String)),
        Seq(
          Body(Seq(
            Call("input_typed_bbf", Seq(Var("ctx"), Var("e")), transitive = false, neg = false),
            Path(Var("e"), TNode(VarT), NamedLink(TNode(VarT), "name"), Var("x"), TLiteral.String),
          ))
        )
      ),
      // lookup_bbf first rule
      Pattern(
        None,
        "sup_lookup_bbf_5_0",
        Seq(Param("ctx", tyContext), Param("x", TLiteral.String)),
        Seq(
          Body(Seq(
            Call("input_lookup_bbf", Seq(Var("ctx"), Var("x")), transitive = false, neg = false)
          ))
        )
      ),
      Pattern(
        None,
        "sup_lookup_bbf_5_1",
        Seq(Param("ctx", tyContext), Param("x", TLiteral.String), Param("x1", TLiteral.String), Param("T1", tyType), Param("rest", tyContext)),
        Seq(
          Body(Seq(
            Call("sup_lookup_bbf_5_0", Seq(Var("ctx"), Var("x")), transitive = false, neg = false),
            Computed(Var("x1"), Evaluation(Seq((Var("ctx"), tyContext)), TLiteral.String, Scala(q"(ctx: Context) => ctx match { case Context.Empty => null; case Context.Bind(x, ty, rest) => x }"))),
            Computed(Var("T1"), Evaluation(Seq((Var("ctx"), tyContext)), tyType, Scala(q"(ctx: Context) => ctx match { case Context.Empty => null; case Context.Bind(x, ty, rest) => ty }"))),
            Computed(Var("rest"), Evaluation(Seq((Var("ctx"), tyContext)), tyContext, Scala(q"(ctx: Context) => ctx match { case Context.Empty => null; case Context.Bind(x, ty, rest) => rest }"))),
          ))
        )
      ),
      // lookup_bbf second rule
      Pattern(
        None,
        "sup_lookup_bbf_6_0",
        Seq(Param("ctx", tyContext), Param("x", TLiteral.String)),
        Seq(
          Body(Seq(
            Call("input_lookup_bbf", Seq(Var("ctx"), Var("x")), transitive = false, neg = false)
          ))
        )
      ),
      Pattern(
        None,
        "sup_lookup_bbf_6_1",
        Seq(Param("ctx", tyContext), Param("x", TLiteral.String), Param("x1", TLiteral.String), Param("T1", tyType), Param("rest", tyContext)),
        Seq(
          Body(Seq(
            Call("sup_lookup_bbf_6_0", Seq(Var("ctx"), Var("x")), transitive = false, neg = false),
            Computed(Var("x1"), Evaluation(Seq((Var("ctx"), tyContext)), TLiteral.String, Scala(q"(ctx: Context) => ctx match { case Context.Empty => null; case Context.Bind(x, ty, rest) => x }"))),
            Computed(Var("T1"), Evaluation(Seq((Var("ctx"), tyContext)), tyType, Scala(q"(ctx: Context) => ctx match { case Context.Empty => null; case Context.Bind(x, ty, rest) => ty }"))),
            Computed(Var("rest"), Evaluation(Seq((Var("ctx"), tyContext)), tyContext, Scala(q"(ctx: Context) => ctx match { case Context.Empty => null; case Context.Bind(x, ty, rest) => rest }"))),
          ))
        )
      ),
      // typed_bbf
      Pattern(
        None,
        "typed_bbf",
        Seq(Param("ctx", tyContext), Param("e", TNode(ExpT)), Param("T", tyType)),
        Seq(
          Body(Seq(
            Call("sup_typed_bbf_1_0", Seq(Var("ctx"), Var("e")), transitive = false, neg = false),
            Path(Var("e"), TNode(IntLit), NamedLink(TNode(IntLit), "value"), Var("value"), TLiteral.Int),
            Call("sup_typed_bbf_1_1", Seq(Var("ctx"), Var("e")), transitive = false, neg = false),
            Computed(Var("T"), Evaluation(Seq(), tyType, Scala(q"() => Type.Int")))
          )),
          Body(Seq(
            Call("sup_typed_bbf_2_0", Seq(Var("ctx"), Var("e")), transitive = false, neg = false),
            Path(Var("e"), TNode(App), NamedLink(TNode(App), "e1"), Var("e1"), TNode(ExpT)),
            Path(Var("e"), TNode(App), NamedLink(TNode(App), "e2"), Var("e2"), TNode(ExpT)),
            Call("sup_typed_bbf_2_1", Seq(Var("ctx"), Var("e"), Var("e1"), Var("e2")), transitive = false, neg = false),
            Call("typed_bbf", Seq(Var("ctx"), Var("e1"), Var("FunT")), transitive = false, neg = false),
            Computed(Var("T1"), Evaluation(Seq((Var("FunT"), tyType)), tyType, Scala(q"(funty: Type) => funty match { case Type.Fun(ty1, ty2) => ty1; case _ => null }"))),
            Computed(Var("T2"), Evaluation(Seq((Var("FunT"), tyType)), tyType, Scala(q"(funty: Type) => funty match { case Type.Fun(ty1, ty2) => ty2; case _ => null }"))),
            Call("typed_bbf", Seq(Var("ctx"), Var("e2"), Var("T1")), transitive = false, neg = false),
            Compare(EqComparator, Var("T1"), Var("T"))
          )),
          Body(Seq(
            Call("sup_typed_bbf_3_0", Seq(Var("ctx"), Var("e")), transitive = false, neg = false),
            Path(Var("e"), TNode(Lam), NamedLink(TNode(Lam), "name"), Var("x"), TLiteral.String),
            Path(Var("e"), TNode(Lam), NamedLink(TNode(Lam), "ty"), Var("T1"), TNode(TypeExpT)),
            Path(Var("e"), TNode(Lam), NamedLink(TNode(Lam), "body"), Var("b"), TNode(ExpT)),
            Call("sup_typed_bbf_3_1", Seq(Var("ctx"), Var("e"), Var("x"), Var("T1"), Var("b")), transitive = false, neg = false),
            Call("conv_typeExp", Seq(Var("T1"), Var("convT1")), transitive = false, neg = false),
            Computed(Var("extCtx"), Evaluation(Seq((Var("x"), TLiteral.String), (Var("convT1"), tyType), (Var("ctx"), tyContext)), tyContext, Scala(q"(x: String, ty: Type, ctx: Context) => Context.Bind(x, ty, ctx)"))),
            Call("sup_typed_bbf_3_2", Seq(Var("ctx"), Var("e"), Var("x"), Var("T1"), Var("b"), Var("extCtx")), transitive = false, neg = false),
            Call("typed_bbf", Seq(Var("extCtx"), Var("b"), Var("T2")), transitive = false, neg = false),
            Call("sup_typed_bbf_3_3", Seq(Var("ctx"), Var("e"), Var("x"), Var("T1"), Var("b"), Var("extCtx"), Var("T2")), transitive = false, neg = false),
            Computed(Var("T"), Evaluation(Seq((Var("convT1"), tyType), (Var("T2"), tyType)), tyType, Scala(q"(ty1: Type, ty2: Type) => Type.Fun(ty1, ty2)")))
          )),
          Body(Seq(
            Call("sup_typed_bbf_4_0", Seq(Var("ctx"), Var("e")), transitive = false, neg = false),
            Path(Var("e"), TNode(VarT), NamedLink(TNode(VarT), "name"), Var("x"), TLiteral.String),
            Call("sup_typed_bbf_4_1", Seq(Var("ctx"), Var("e"), Var("x")), transitive = false, neg = false),
            Call("lookup_bbf", Seq(Var("ctx"), Var("x"), Var("T")), transitive = false, neg = false)
          ))
        )
      ),
      // lookup_bbf
      Pattern(
        None,
        "lookup_bbf",
        Seq(Param("ctx", tyContext), Param("x", TLiteral.String), Param("T", tyType)),
        Seq(
          Body(Seq(
            Call("sup_lookup_bbf_5_0", Seq(Var("ctx"), Var("x")), transitive = false, neg = false),
            Computed(Var("x1"), Evaluation(Seq((Var("ctx"), tyContext)), TLiteral.String, Scala(q"(ctx: Context) => ctx match { case Context.Empty => null; case Context.Bind(x, ty, rest) => x }"))),
            Computed(Var("T1"), Evaluation(Seq((Var("ctx"), tyContext)), tyType, Scala(q"(ctx: Context) => ctx match { case Context.Empty => null; case Context.Bind(x, ty, rest) => ty }"))),
            Computed(Var("rest"), Evaluation(Seq((Var("ctx"), tyContext)), tyContext, Scala(q"(ctx: Context) => ctx match { case Context.Empty => null; case Context.Bind(x, ty, rest) => rest }"))),
            Call("sup_lookup_bbf_5_1", Seq(Var("ctx"), Var("x"), Var("x1"), Var("T1"), Var("rest")), transitive = false, neg = false),
            Compare(EqComparator, Var("x"), Var("x1")),
            Compare(EqComparator, Var("T"), Var("T1")),
          )),
          Body(Seq(
            Call("sup_lookup_bbf_6_0", Seq(Var("ctx"), Var("x")), transitive = false, neg = false),
            Computed(Var("x1"), Evaluation(Seq((Var("ctx"), tyContext)), TLiteral.String, Scala(q"(ctx: Context) => ctx match { case Context.Empty => null; case Context.Bind(x, ty, rest) => x }"))),
            Computed(Var("T1"), Evaluation(Seq((Var("ctx"), tyContext)), tyType, Scala(q"(ctx: Context) => ctx match { case Context.Empty => null; case Context.Bind(x, ty, rest) => ty }"))),
            Computed(Var("rest"), Evaluation(Seq((Var("ctx"), tyContext)), tyContext, Scala(q"(ctx: Context) => ctx match { case Context.Empty => null; case Context.Bind(x, ty, rest) => rest }"))),
            Call("sup_lookup_bbf_6_1", Seq(Var("ctx"), Var("x"), Var("x1"), Var("T1"), Var("rest")), transitive = false, neg = false),
            Compare(NeqComparator, Var("x"), Var("x1")),
            Call("lookup_bbf", Seq(Var("rest"), Var("x"), Var("T")), transitive = false, neg = false),
          )),
        )
      ),
      Pattern(
        None,
        "conv_typeExp",
        Seq(Param("tyExp", TNode(TypeExpT)), Param("ty", tyType)),
        Seq(
          Body(Seq(
            HasType(Var("tyExp"), TNode(IntTypeExpT)),
            Computed(Var("ty"), Evaluation(Seq(), tyType, Scala(q"() => Type.Int")))
          )),
          Body(Seq(
            Path(Var("tyExp"), TNode(FunTypeExpT), NamedLink(TNode(FunTypeExpT), "ty1"), Var("tyExp1"), TNode(TypeExpT)),
            Path(Var("tyExp"), TNode(FunTypeExpT), NamedLink(TNode(FunTypeExpT), "ty2"), Var("tyExp2"), TNode(TypeExpT)),
            Call("conv_typeExp", Seq(Var("tyExp1"), Var("ty1")), transitive = false, neg = false),
            Call("conv_typeExp", Seq(Var("tyExp2"), Var("ty2")), transitive = false, neg = false),
            Computed(Var("ty"), Evaluation(Seq((Var("ty1"), tyType), (Var("ty2"), tyType)), tyType, Scala(q"(ty1: Type, ty2: Type) => Type.Fun(ty1, ty2)")))
          ))
        )
      )
    ),
    Seq(
      Scala(q"import inca.caseStudies.typing.Context"),
      Scala(q"import inca.caseStudies.typing.Type")))

  val compiled = Compiler.compileGP(gp, options)

  val scope = new QueryScope(Prog.languageMetaInfo)
  val (engine, feed) = EnginePool.loadEngineAndDatabase(scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)
//  val matcher = EnginePool.loadQuery(compiled.psystemModule.patterns("sup_typed_bbf_1_0")(), scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)
//  val matcher = EnginePool.loadQuery(compiled.psystemModule.patterns("input_typed_bbf")(), scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)
//  val matcher = EnginePool.loadQuery(compiled.psystemModule.patterns("input_ok_b")(), scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)
//  val matcher = EnginePool.loadQuery(compiled.psystemModule.patterns("conv_typeExp")(), scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)
//  val matcher = EnginePool.loadQuery(compiled.psystemModule.patterns("lookup_bbf")(), scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)
  val matcher = EnginePool.loadQuery(compiled.psystemModule.patterns("typed_bbf")(), scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)

  val prog1 = Prog(Exp.Lam("x", TypeExp.Int(), Exp.Lam("y", TypeExp.Fun(TypeExp.Fun(TypeExp.Int(), TypeExp.Int()), TypeExp.Int()), Exp.IntLit(1))))
  val prog2 = Prog(Exp.Lam("x", TypeExp.Int(), Exp.IntLit(1)))
  val prog3 = Prog(Exp.IntLit(1))
  val prog5 = Prog(Exp.App(Exp.IntLit(1), Exp.IntLit(1)))
  val prog6 = Prog(Exp.Lam("x", TypeExp.Int(), Exp.Var("x")))
  val prog7 = Prog(Exp.App(Exp.Lam("x", TypeExp.Int(), Exp.Var("x")), Exp.IntLit(5)))
  val es = Diffable.load(prog7)
  println(Printer.prettyModule(gp))
  println("START")
  feed.processEditScript(es)
  println(matcher.getAllMatches)
}
