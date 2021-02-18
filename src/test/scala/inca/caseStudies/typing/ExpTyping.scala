package inca.caseStudies.typing

import inca.compiler.{Compiler, Options}

object ExpTyping extends App {

  val options = Options(Exp.languageMetaInfo)

  val ExpT = Exp.expTag
  val IntLit = Exp.intLitTag
  val Add = Exp.addTag
  val Var = Exp.varTag
  val Lam = Exp.lamTag
  val App = Exp.appTag
  val Let = Exp.letTag


  val code =
    s"""
       |module ExpTyping
       |
       |using GoLang
       |`import inca.caseStudies.typing.Type`
       |`import inca.caseStudies.typing.Context`
       |
       |def checkSimple(e: $ExpT): `Type` = e match {
       |  case $IntLit() => yield `Type.Int`
       |  case $Add(e1, e2) =>
       |    if (checkSimple(e1) == `Type.Int` &&
       |        checkSimple(e2) == `Type.Int`)
       |      yield `Type.Int`
       |    else
       |      fail
       |}
       |
       |def check(ctx: `Context`, e: $ExpT): `Type` = e match {
       |  case $IntLit() => yield `Type.Int`
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
       |def lookup(ctx: `Context`, v: $Var): `Type` = ctx match {
       |  case `Context.Empty` => fail
       |  case `Context.Bind`(name, ty, rest) =>
       |    if (name == v.name)
       |      yield ty
       |    else
       |      yield lookup(rest, v)
       |}
       |""".stripMargin

  val compiled = Compiler.compileFun(code, options)
  println(compiled.optimized)

}
