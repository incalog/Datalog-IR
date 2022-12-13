//package inca.frontend.objectoriented
//
//module WhileLang
//
//class Exp {
//  // abstract
//  def accept(v: Visitor): Unit = { }
//}
//
//class Var extends Exp {
//  val name: String
//  def this(name: String) = {
//      this.name = name
//  }
//
//  @override
//  def accept(v: Visitor): Unit = {
//      v.visitVar(this)
//  }
//}
//
//class Num extends Exp {
//  val num: Int
//  def this(num: Int) = {
//      this.num = num
//  }
//
//  @override
//  def accept(v: Visitor): Unit = {
//      v.visitNum(this)
//  }
//}
//
//class BinOp extends Exp {
//  val left: Exp
//  val right: Exp
//  def this(left: Exp, right: Exp) = {
//      this.left = left
//      this.right = right
//  }
//
//  @override
//  def accept(v: Visitor): Unit = {
//    this.left.accept(v)
//    this.right.accept(v)
//    this.binOpAccept()
//  }
//  // abstract
//  def binOpAccept(v: Visitor): Unit = { }
//}
//class Add extends BinOp {
//  def this(left: Exp, right: Exp) = {
//      super(left, right)
//  }
//  @override
//  def binOpAccept(v: Visitor): Unit = {
//      v.visitAdd(this)
//  }
//}
//class Sub extends BinOp {
//  def this(left: Exp, right: Exp) = {
//      super(left, right)
//  }
//  @override
//  def binOpAccept(v: Visitor): Unit = {
//      v.visitSub(this)
//  }
//}
//class Mul extends BinOp {
//  def this(left: Exp, right: Exp) = {
//      super(left, right)
//  }
//  @override
//  def binOpAccept(v: Visitor): Unit = {
//      v.visitMul(this)
//  }
//}
//class GT extends BinOp {
//  def this(left: Exp, right: Exp) = {
//      super(left, right)
//  }
//  @override
//  def binOpAccept(v: Visitor): Unit = {
//      v.visitGT(this)
//  }
//}
//
//class Stm {
//  // abstract
//  def accept(v: Visitor): Unit = {
//  }
//}
//class Assign extends Stm {
//  val lhs: String
//  val rhs: Exp
//  def this(lhs: String, rhs: Exp): Unit = {
//    this.lhs = lhs
//    this.rhs = rhs
//  }
//  @override
//  def accept(v: Visitor): Unit = {
//    this.rhs.accept(v)
//    v.visitAssign(this)
//  }
//}
//class Skip extends Stm {
//  def this(): Unit = {
//  }
//  @override
//  def accept(v: Visitor): Unit = {
//    v.visitSkip(this)
//  }
//}
//class Sequence extends Stm {
//  val s1: Stm
//  val s2: Stm
//  def this(s1: Stm, s2: Stm): Unit = {
//    this.s1 = s1
//    this.s2 = s2
//  }
//  @override
//  def accept(v: Visitor): Unit = {
//    this.s1.accept(v)
//    this.s2.accept(v)
//    v.visitSequence(this)
//  }
//}
//class If extends Stm {
//  val cnd: Exp
//  val thn: Stm
//  val els: Stm
//  def this(cnd: Exp, thn: Stm, els: Stm): Unit = {
//    this.cnd = cnd
//    this.thn = thn
//    this.els = els
//  }
//  @override
//  def accept(v: Visitor): Unit = {
//    this.cnd.accept(v)
//    this.thn.accept(v)
//    this.els.accept(v)
//    v.visitIf(this)
//  }
//}
//class While extends Stm {
//  val cnd: Exp
//  val body: Stm
//  def this(cnd: Exp, body: Stm): Unit = {
//    this.cnd = cnd
//    this.body = body
//  }
//  @override
//  def accept(v: Visitor): Unit = {
//    this.cnd.accept(v)
//    this.body.accept(v)
//    v.visitWhile(this)
//  }
//}
//class ValIn extends Stm {
//  val name: String
//  val rhs: Exp
//  val body: Stm
//  def this(name: String, rhs: Exp, body: Stm): Unit = {
//    this.name = name
//    this.rhs = rhs
//    this.body = body
//  }
//  @ override
//  def accept(v: Visitor): Unit = {
//    this.rhs.accept(v)
//    this.body.accept(v)
//    v.visitValIn(this)
//  }
//}
//class VarIn extends Stm {
//  val name: String
//  val rhs: Exp
//  val body: Stm
//  def this(name: String, rhs: Exp, body: Stm): Unit = {
//    this.name = name
//    this.rhs = rhs
//    this.body = body
//  }
//  @ override
//  def accept(v: Visitor): Unit = {
//    this.rhs.accept(v)
//    this.body.accept(v)
//    v.visitVarIn(this)
//  }
//}
//
//
//class Visitor {
//  // Exp
//  def visitVar(e: Var): Unit = { }
//  def visitNum(e: Num): Unit = { }
//  def visitAdd(e: Add): Unit = { }
//  def visitSub(e: Sub): Unit = { }
//  def visitMul(e: Mul): Unit = {}
//  def visitGT(e: GT): Unit = {}
//
//  // Stm
//  def visitAssign(s: Assign): Unit = { }
//  def visitSkip(s: Skip): Unit = { }
//  def visitSequence(s: Sequence): Unit = { }
//  def visitIf(s: If): Unit = { }
//  def visitWhile(s: While): Unit = { }
//  def visitValIn(s: ValIn): Unit = { }
//  def visitVarIn(s: VarIn): Unit = { }
//}
//
//class VarCountVisitor extends Visitor {
//    vag count: Int = 0 with VarCountVisitor.sum
//
//    @static
//    def sum(n1: Int, n2: Int): Int = {
//        n1 + n2
//    }
//
//    @override
//    def visitVar(e: Var): Unit = {
//        this.count #= 1
//    }
//}
//
//class SumNumVisitor extends Visitor {
//    vag sum: Int = 0 with SumNumVisitor.sum
//
//    @static
//    def sum(n1: Int, n2: Int): Int = {
//        n1 + n2
//    }
//
//    @override
//    def visitNum(e: Num): Unit = {
//        this.sum #= e.num
//    }
//}
//
//class Examples {
//  val factorial: Stm =
//    new VarIn("n", new Num(5),
//      new VarIn("acc", new Num(1),
//        new While(
//          // 2 > n  iff n <= 1
//          new GT(new Num(2), new Var("n")),
//          new Sequence(
//            new Assign("acc", new Mul(new Var("acc"), new Var("n"))),
//            new Assign("n", new Sub(new Var("n"), new Num(1)))
//          )
//        )
//      )
//    )
//}
//
//class Main {
////    @main
////    def countVar(): Int = {
//        //        val tree1: Exp = new Add(new Var("x"), new Var("y"))
//        //        val tree1Count: VarCountVisitor = new VarCountVisitor()
//        //        tree1.accept(tree1Count)
//        //        tree1Count.count
////        1
////    }
//
//  val examples: Examples = new Examples()
//
//  @main
//  def sumNum(): Int = {
//    val treeSum: SumNumVisitor = new SumNumVisitor()
//    val tree1: Exp = new Add(new Var("x"), new Num(7))
//    val tree2: Exp = new Add(new Num(1), new Add(new Num(2), new Num(3)))
//    tree1.accept(treeSum)
//    tree2.accept(treeSum)
//    examples.factorial.accept(treeSum)
//    treeSum.sum
//  }
//
//}
//
//
//
//
