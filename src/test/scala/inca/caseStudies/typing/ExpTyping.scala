package inca.caseStudies.typing

import inca.compiler.{Compiler, Options}

object ExpTyping extends App {

  val options = Options(Exp.languageMetaInfo)

  val ExpT = Exp.expTag
  val Int = Exp.intTag
  val Add = Exp.addTag
  val Var = Exp.varTag
  val Lam = Exp.lamTag
  val App = Exp.appTag



  val code =
    s"""
       |module ExpTyping
       |
       |scala import inca.caseStudies.typing.Type
       |scala import inca.caseStudies.typing.Context
       |
       |def check(ctx: `Context`, e: $ExpT): `Type` = e match {
       |  case $Int() => yield `Type.Int`
       |  case $Add(e1, e2) =>
       |    if (check(ctx, e1) == `Type.Int` &&
       |        check(ctx, e2) == `Type.Int`)
       |      yield `Type.Int`
       |    else
       |      fail
       |  case v@$Var() => yield lookup(ctx, v)
       |  case $Lam(name, ty, body) => {
       |    val bodyCtx = `Context.Bind`(name, ty, ctx)
       |    val bodyTy = check(bodyCtx, body)
       |    yield `Type.Fun`(ty, bodyTy)
       |  }
       |  case $App(e1, e2) => {
       |    val funTy = check(ctx, e1)
       |    val argTy = check(ctx, e2)
       |    if (`funTy.isInstanceOf[Type.Fun]`) {
       |      val paramTy = `funTy.asInstanceOf[Type.Fun].t1`
       |      if (paramTy == argTy)
       |        yield `funTy.asInstanceOf[Type.Fun].t2`
       |      else
       |        fail
       |    } else {
       |      fail
       |    }
       |  }
       |}
       |
       |
       |def lookup(ctx: `Context`, v: $Var): `Type` =
       |  if (ctx == `Context.Empty`)
       |    fail
       |  else {
       |    val bind = `ctx.asInstanceOf[Context.Bind]`
       |    if (`bind.name` == v.name)
       |      yield `bind.ty`
       |    else
       |      yield lookup(`bind.rest`, v)
       |  }
       |""".stripMargin

  val compiled = Compiler.compileFun(code, options)
  println(compiled.typed)

}
