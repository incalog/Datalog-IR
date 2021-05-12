package inca.frontend.functional.measurements.itypes

trait GenerateProg {
  def generate(depth: Int): Exp
}
object GenerateStarDependencyProg extends GenerateProg {
  def generate(depth: Int): Exp = {
    def newName(i: Int): String = s"f$i"

    val f0Name = newName(0)
    def genExp(): Exp = {
      Lam("x", TInt, Add(Num(1), App(Var(f0Name), Var("x"))))
    }

    val bindings = (1 to depth).map {i => (newName(i), genExp()) }

    Let(f0Name, Lam("x", TInt, Add(Num(1), Var("x"))),
      LetStar(bindings, Add(Num(1), App(Var(f0Name), Num(1))))
    )
  }
}
object GenerateChainDependencyProg extends GenerateProg {
  def generate(depth: Int): Exp = {
    def newName(i: Int): String = s"f$i"

    val f0Name = newName(0)
    def genExp(i: Int): Exp = {
      Lam("x", TInt, Add(Num(1), App(Var(newName(i)), Var("x"))))
    }

    val inner = (1 to depth).foldRight[Exp](Add(Num(1), App(Var(newName(depth)), Num(1)))) { case (ix, res) =>
      Let(newName(ix), genExp(ix - 1), res)
    }

    Let(f0Name, Lam("x", TInt, Add(Num(1), Var("x"))), inner)
  }
}