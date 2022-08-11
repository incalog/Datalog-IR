package inca.frontend.objectoriented.util

import inca.frontend.objectoriented.core._

trait Collect[R] {

  def apply(module: Module): Seq[R] = module.content.flatMap {
    case fun: FunctionDef => collectFunctionDef(fun)
    // TODO: collect class defs
  }

  def collectFunctionDef(fun: FunctionDef): Seq[R] =
    fun.annos.flatMap(collectAnnotation) ++ fun.params.flatMap(collectParam) ++ collectExpression(fun.body)

  def collectAnnotation(anno: Annotation): Seq[R] = Seq()

  def collectType(t: Type): Seq[R] = t match {
    case TAny => Seq()
    case TNothing => Seq()
    case TFun(from, to) => from.flatMap(collectType) ++ collectType(to)
    case TName(_) => Seq()
    // TODO: Figure out what to do here
    // case TClass(_) =>
    case TOption(ty) => collectType(ty)
  }

  def collectParam(par: Param): Seq[R] = collectType(par.typ)

  def collectExpression(exp: Expression): Seq[R] = exp match {
    case Var(name) => Seq()
    // TODO: Update this to collect other expressions
  }
}
