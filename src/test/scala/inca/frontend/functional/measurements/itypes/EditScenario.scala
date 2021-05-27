package inca.frontend.functional.measurements.itypes

trait EditScenario {
  def edit(exp: Exp): Exp = {
    traverse(exp, 0)._1
  }

  def shouldChange(count: Int): Boolean

  def traverse(exp: Exp, count: Int): (Exp, Int) = exp match {
    case Num(n) => (exp, count)
    case Var(n) => (exp, count)
    case Add(lhs, rhs) =>
      val (lexp, lcount) = traverse(lhs, count)
      val (rexp, rcount) = traverse(rhs, lcount)
      (Add(lexp, rexp), rcount)
    case App(lhs, rhs) =>
      val (lexp, lcount) = traverse(lhs, count)
      val (rexp, rcount) = traverse(rhs, lcount)
      (App(lexp, rexp), rcount)
    case Lam(p, ty, b) =>
      val (bexp, bcount) = traverse(b, count)
      (Lam(p, ty, bexp), bcount)
    case Let(n, bound, body) =>
      val (boundexp, boundcount) = traverse(bound, count)
      val (bodyexp, bodycount) = traverse(body, boundcount)
      (Let(n, boundexp, bodyexp), bodycount)
    case LetStar(bindings, body) =>
      var bindingsCount = count
      val newBindings = mapBindingList(bindings) { case (n, e) =>
        val (newe, c) = traverse(e, bindingsCount)
        bindingsCount = c
        (n, newe)
      }
      val (bodyexp, bodycount) = traverse(body, bindingsCount)
      (LetStar(newBindings, bodyexp), bodycount)
  }

  def mapBindingList(bindings: BindingList)(f: (String, Exp) => (String, Exp)): BindingList = bindings match {
    case Nil() => Nil()
    case Cons(name, bound, rest) =>
      val (newName, newExp) = f(name, bound)
      Cons(newName, newExp, mapBindingList(rest)(f))
  }
}



object NumEditScenario extends EditScenario {

  override def shouldChange(count: Int): Boolean = count == 0

  override def traverse(exp: Exp, count: Int): (Exp, Int) = exp match {
    case Num(n) =>
      if (shouldChange(count)) (Num(n + 1), count + 1)
      else (exp, count + 1)
    case _ => super.traverse(exp, count)
  }
}

object RefEditScenario extends EditScenario {

  override def shouldChange(count: Int): Boolean = count == 0

  override def traverse(exp: Exp, count: Int): (Exp, Int) = exp match {
    case Var(n) =>
      if (shouldChange(count)) (Var(n + 1), count + 1)
      else (exp, count + 1)
    case _ => super.traverse(exp, count)
  }
}

object ParamEditScenario extends EditScenario {

  override def shouldChange(count: Int): Boolean = count == 0

  override def traverse(exp: Exp, count: Int): (Exp, Int) = exp match {
    case Lam(p, t, b) =>
      val newParam = if (shouldChange(count)) p + 1 else p
      val (bexp, bcount) = traverse(b, count)
      (Lam(newParam, t, bexp), bcount + 1)
    case _ => super.traverse(exp, count)
  }
}

object AnnoEditScenario extends EditScenario {

  override def shouldChange(count: Int): Boolean = count == 0

  override def traverse(exp: Exp, count: Int): (Exp, Int) = exp match {
    case Lam(p, t, b) =>
      val newType = if (shouldChange(count)) TFun(TInt(), TInt()) else t
      val (bexp, bcount) = traverse(b, count)
      (Lam(p, newType, bexp), bcount + 1)
    case _ => super.traverse(exp, count)
  }
}

object LambdaEditScenario extends EditScenario {

  override def shouldChange(count: Int): Boolean = count == 0

  override def traverse(exp: Exp, count: Int): (Exp, Int) = exp match {
    case Lam(p, t, b) =>
      val (bexp, bcount) = traverse(b, count + 1)
      if (shouldChange(count))
        (Lam(p, t, Lam("y", TInt(), bexp)), bcount)
      else
        (Lam(p, t, bexp), bcount)
    case _ => super.traverse(exp, count)
  }
}

object AddAppEditScenario extends EditScenario {

  override def shouldChange(count: Int): Boolean = count == 0

  override def traverse(exp: Exp, count: Int): (Exp, Int) = exp match {
    case Add(l, r) =>
      val (lexp, lcount) = traverse(l, count)
      val (rexp, rcount) = traverse(r, lcount)
      val newExp = if (shouldChange(count)) App(lexp, rexp) else Add(lexp, rexp)
      (newExp, rcount)
    case _ => super.traverse(exp, count)
  }
}
