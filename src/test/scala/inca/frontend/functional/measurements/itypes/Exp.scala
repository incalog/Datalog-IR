package inca.frontend.functional.measurements.itypes

trait Exp
case class Num(v: Int) extends Exp
case class Add(lhs: Exp, rhs: Exp) extends Exp
case class App(fun: Exp, arg: Exp) extends Exp
case class Lam(param: String, ty: Type, body: Exp) extends Exp
case class Var(name: String) extends Exp
case class LetStar(bindings: Seq[(String, Exp)], body: Exp) extends Exp
case class Let(name: String, bound: Exp, body: Exp) extends Exp


trait Type
case object TInt extends Type
case class TFun(arg: Type, res: Type) extends Type
