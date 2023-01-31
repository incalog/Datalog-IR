package inca.frontend.functional.itypes

trait Exp
case class Num(v: Int) extends Exp
case class Add(lhs: Exp, rhs: Exp) extends Exp
case class App(fun: Exp, arg: Exp) extends Exp
case class Lam(param: String, ty: Type, body: Exp) extends Exp {
  override def toString: String = s"""Lam(\"$param\",${ty.toString},${body.toString})"""
}
case class Var(name: String) extends Exp {
  override def toString: String = "Var(\"" + name + "\")"
}
case class LetStar(bindings: BindingList, body: Exp) extends Exp {
  override def toString: String = s"""LetStar(${bindings.toString},${body.toString})"""
}
case class Let(name: String, bound: Exp, body: Exp) extends Exp {
  override def toString: String = s"""Let(\"$name\",${bound.toString},${body.toString})"""
}

trait BindingList
case class Nil() extends BindingList {
  override def toString: String = "Nil()"
}
case class Cons(name: String, bound: Exp, rest: BindingList) extends BindingList {
  override def toString: String = s"""Cons(\"$name\",${bound.toString},${rest.toString})"""
}

trait Type
case class TInt() extends Type
case class TFun(arg: Type, res: Type) extends Type

object Exp {
  def fold[A](
      num: Int => A,
      add: (A, A) => A,
      app: (A, A) => A,
      lam: (String, Type, A) => A,
      vari: String => A,
      let: (String, A, A) => A,
      exp: Exp
    ): A = exp match {
    case Num(n) => num(n)
    case Add(l, r) =>
      add(fold(num, add, app, lam, vari, let, l), fold(num, add, app, lam, vari, let, r))
    case App(l, r) =>
      app(fold(num, add, app, lam, vari, let, l), fold(num, add, app, lam, vari, let, r))
    case Lam(n, ty, b) => lam(n, ty, fold(num, add, app, lam, vari, let, b))
    case Var(n) => vari(n)
    case Let(n, bound, body) =>
      let(n, fold(num, add, app, lam, vari, let, bound), fold(num, add, app, lam, vari, let, body))
  }

  def numOfExp(exp: Exp): Int = fold[Int](
    _ => 1,
    (l, r) => 1 + l + r,
    (l, r) => 1 + l + r,
    (_, _, b) => 1 + b,
    _ => 1,
    (_, b1, b2) => 1 + b1 + b2,
    exp
  )
}
