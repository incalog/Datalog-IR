package inca.frontend.objectoriented.asg

class Visitor {
  def visitProg(n: Prog): Unit = { }
  def visitDef(n: Def): Unit = { }
  def visitNum(n: Num): Unit = { }
  def visitVar(v: Var): Unit = { }
}

class Prog(val defs: List[Def]) {
  def accept(v: Visitor): Unit = {
    for (d <- this.defs)
      d.accept(v)
    v.visitProg(this)
  }
}

class Def(val name: String, val exp: Exp) {
  def accept(v: Visitor): Unit = {
    this.exp.accept(v)
    v.visitDef(this)
  }
}

abstract class Exp {
  def accept(v: Visitor): Unit
}
class Num(val value: Int) extends Exp {
  def accept(v: Visitor): Unit = {
    v.visitNum(this)
  }
}
class Var(val name: String) extends Exp {
  var target: Option[Def] = None
  def accept(v: Visitor): Unit = {
    v.visitVar(this)
  }
}

class ResolveVisitor(prog: Prog) extends Visitor {
  override def visitVar(v: Var): Unit = {
    for (d <- this.prog.defs.find(_.name == v.name))
      v.target = Some(d)
  }
}

class ReachVisitor extends Visitor {
  var reachable: Set[Def] = Set.empty

  override def visitDef(d: Def): Unit = {
    this.reachable = this.reachable + d
  }
  override def visitVar(v: Var): Unit = {
    for (d <- v.target)
      d.accept(this)
  }
}



object Main {
//  @main
  def main(args: Array[String]): Unit = {
    val d1 = new Def("a", new Num(1))
    val d2 = new Def("b", new Num(2))
    val d3 = new Def("c", new Var("a"))
    val d4 = new Def("d", new Var("b"))
    val main = new Def("main", new Var("c"))
    val prog = new Prog(List(d1, d2, d3, d4, main))

    val resolving = new ResolveVisitor(prog)
    prog.accept(resolving)
    // Var.target is now defined for all Var objects

    // this loops in Scala when prog is recursive, but always computes in Datalog
    val reaching = new ReachVisitor()
    main.accept(reaching)
    // reaching.reachable contains three definitions: d1, d3, main

    // these results remain the same in OODL even after changing d1
    //   val d1 = new Def("a", new Var("c"))
    // which makes prog cylcic: main -> d3 -> d1 -> d3 -> ...
  }
}