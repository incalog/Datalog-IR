package inca.embedded

import inca.util.{Gensym, Scala, TupleOps}

trait Functional {
  type Mod
  type Fun
  type Exp
  type Typ

  def module(name: String, funs: List[Fun]): Mod
  def function(name: String, params: List[(String, Typ)], outType: Typ, body: => Exp): Fun

  // types
  def tany: Typ
  def tbool: Typ
  def tint: Typ
  def tdouble: Typ
  def tstring: Typ

  // expressions
  def bool(b: Boolean): Exp
  def int(v: Int): Exp
  def double(v: Double): Exp
  def string(s: String): Exp
  def va(name: String): Exp
  def let(names: List[String], bound: Exp, body: Exp): Exp
  def ifTrue(cnd: Exp, thn: => Exp, els: => Exp): Exp
  def call(fun: String, args: List[Exp]): Exp
  def op(lhs: Exp, op: String, rhs: Exp): Exp
}

trait FunctionalTypeAST extends Functional {
  sealed trait Type
  case object TAny extends Type
  case object TBool extends Type
  case object TInt extends Type
  case object TDouble extends Type
  case object TString extends Type

  override type Typ = Type
  override def tany: Typ = TAny
  override def tbool: Typ = TBool
  override def tint: Typ = TInt
  override def tdouble: Typ = TDouble
  override def tstring: Typ = TString
}

object FunctionalEval extends Functional with FunctionalTypeAST {
  sealed trait Value
  case class VBoolean(b: Boolean) extends Value
  case class VInt(i: Int) extends Value
  case class VDouble(d: Double) extends Value
  case class VString(s: String) extends Value

  /** runs the main function */
  override type Mod = String => List[Value] => Value
  /** runs the function */
  override type Fun = (String, List[Value] => Value)
  /** yields the value of the expression */
  override type Exp = Value

  private val scalaCompiler = new Scala.ScalaCompiler

  // runtime environment
  private var functions: Map[String, List[Value] => Value] = Map()

  private var env: Map[String, Value] = Map()
  private def locally[A](newenv: Map[String, Value])(f: => A): A = {
    val old = env
    env = newenv
    try f
    finally env = old
  }

  // evaluation
  override def module(name: String, funs: List[Fun]): Mod = name => mainArgs => {
    functions = funs.toMap
    functions(name)(mainArgs)
  }

  override def function(name: String, params: List[(String, FunctionalEval.Type)], outType: FunctionalEval.Typ, body: => Value): Fun = (name, args =>
    locally(Map() ++ params.view.map(_._1).zip(args))(body))

  // expressions
  override def bool(b: Boolean): Exp = VBoolean(b)
  override def int(v: Int): Exp = VInt(v)
  override def double(v: Double): Exp = VDouble(v)
  override def string(s: String): Exp = VString(s)
  override def va(name: String): Value = env.getOrElse(name, throw new IllegalArgumentException(s"Unbound variable $name"))
  override def let(names: List[String], bound: Value, body: Value): Value = {
    if (names.size != 1)
      throw new UnsupportedOperationException(s"Cannot handle tuples yet")
    locally(env + (names.head -> bound))(body)
  }
  override def ifTrue(cnd: Value, thn: => Value, els: => Value): Value = cnd match {
    case VBoolean(b) => if (b) thn else els
    case _ => throw new IllegalArgumentException(s"Type error: Expected boolean, but got $cnd")
  }

  override def call(fun: String, args: List[Value]): Value = functions.get(fun) match {
    case Some(f) => f(args)
    case None => throw new IllegalArgumentException(s"Unknown function $fun")
  }

  override def op(lhs: Value, op: String, rhs: Value): Value = (lhs, rhs) match {
    case (VBoolean(l), VBoolean(r)) => op match {
      case "==" => VBoolean(l == r)
      case "!=" => VBoolean(l != r)
      case "&&" => VBoolean(l && r)
      case "||" => VBoolean(l || r)
      case _ => throw new IllegalArgumentException(s"Type error: Expected boolean, but got $lhs and $rhs")
    }
    case (VInt(l), VInt(r)) => op match {
      case "==" => VBoolean(l == r)
      case "!=" => VBoolean(l != r)
      case "<" => VBoolean(l < r)
      case "<=" => VBoolean(l <= r)
      case ">" => VBoolean(l > r)
      case ">=" => VBoolean(l >= r)
      case "+" => VInt(l + r)
      case "-" => VInt(l - r)
      case "*" => VInt(l * r)
      case "/" => VInt(l / r)
      case _ => throw new IllegalArgumentException(s"Unknown operator $op")
    }
    case (VDouble(l), VDouble(r)) => op match {
      case "==" => VBoolean(l == r)
      case "!=" => VBoolean(l != r)
      case "<" => VBoolean(l < r)
      case "<=" => VBoolean(l <= r)
      case ">" => VBoolean(l > r)
      case ">=" => VBoolean(l >= r)
      case "+" => VDouble(l + r)
      case "-" => VDouble(l - r)
      case "*" => VDouble(l * r)
      case "/" => VDouble(l / r)
      case _ => throw new IllegalArgumentException(s"Unknown operator $op")
    }
    case (VString(l), VString(r)) => op match {
      case "==" => VBoolean(l == r)
      case "!=" => VBoolean(l != r)
      case "+" => VString(l + r)
      case _ => throw new IllegalArgumentException(s"Unknown operator $op")
    }
    case _ => throw new IllegalArgumentException(s"Type error: Expected numeric or string, but got $lhs and $rhs")
  }
}

trait FunctionalDatalog extends Functional {
  val datalog: Datalog

  private val gensym = new Gensym(Set())

  type Mod = datalog.Mod
  type Fun = datalog.Pat
  type Tuple = List[datalog.Trm]
  type Exp = List[(Tuple, List[datalog.Ato])]
  type Typ = datalog.Typ

  override def module(name: String, funs: List[Fun]): Mod = datalog.module(name, funs)
  override def function(name: String, params: List[(String, Typ)], outType: Typ, body: => Exp): Fun = {
    val outParams = Seq((gensym.fresh("out"), outType))
    val bodies = for ((terms, cons) <- body)
      yield datalog.body(cons ++ outParams.zip(terms).map(pt => datalog.eq(datalog.va(pt._1._1), pt._2)))
    datalog.pattern(name, params ++ outParams, bodies)
  }

  // types
  override def tany: datalog.Typ = datalog.tany
  override def tbool: datalog.Typ = datalog.tbool
  override def tint: datalog.Typ = datalog.tint
  override def tdouble: datalog.Typ = datalog.tdouble
  override def tstring: datalog.Typ = datalog.tstring

  // expressions
  override def bool(b: Boolean): List[(Tuple, List[datalog.Ato])] = List((List(datalog.bool(b)), List()))
  override def int(v: Int): List[(Tuple, List[datalog.Ato])] = List((List(datalog.int(v)), List()))
  override def double(v: Double): List[(Tuple, List[datalog.Ato])] = List((List(datalog.double(v)), List()))
  override def string(s: String): List[(Tuple, List[datalog.Ato])] = List((List(datalog.string(s)), List()))
  override def va(name: String): List[(Tuple, List[datalog.Ato])] = List((List(datalog.va(name)), List()))

  override def let(names: List[String], bound: List[(Tuple, List[datalog.Ato])], body: List[(Tuple, List[datalog.Ato])]): List[(Tuple, List[datalog.Ato])] = {
    if (names.size != 1)
      throw new UnsupportedOperationException(s"Cannot handle tuples yet")
    val vars = names.map(datalog.va)
    for ((boundTerms, boundCons) <- bound;
         (bodyTerm, bodyCons) <- body)
    yield {
      val eqs = vars.zip(boundTerms).map(vt => datalog.eq(vt._1, vt._2))
      (bodyTerm, boundCons ++ eqs ++ bodyCons)
    }
  }

  override def ifTrue(cnd: List[(Tuple, List[datalog.Ato])], thn: => List[(Tuple, List[datalog.Ato])], els: => List[(Tuple, List[datalog.Ato])]): List[(Tuple, List[datalog.Ato])] = {
    val thnRes =
      for ((Seq(cndTerm), cndCons) <- cnd;
           (thnTerm, thnCons) <- thn)
      yield (thnTerm, cndCons ++ Seq(datalog.eq(cndTerm, datalog.bool(true))) ++ thnCons)
    val elsRes =
      for ((Seq(cndTerm), cndCons) <- cnd;
           (elsTerm, elsCons) <- els)
      yield (elsTerm, cndCons ++ Seq(datalog.eq(cndTerm, datalog.bool(false))) ++ elsCons)
    thnRes ++ elsRes
  }

  override def call(fun: String, args: List[List[(Tuple, List[datalog.Ato])]]): List[(Tuple, List[datalog.Ato])] = {
    val outvars = List(datalog.va(gensym.fresh("out")))

    // create single call constraint when no arguments passed
    if (args.isEmpty)
      return List((outvars, List(datalog.query(fun, outvars))))

    for (tups <- TupleOps.cartesianProductList(args)) yield {
      val (argTerms, argCons) = tups.unzip
      (outvars, argCons.flatten :+ datalog.query(fun, argTerms.flatten ++ outvars))
    }
  }

  override def op(lhs: List[(Tuple, List[datalog.Ato])], op: String, rhs: List[(Tuple, List[datalog.Ato])]): List[(Tuple, List[datalog.Ato])] = {
    val opOut = datalog.va(gensym.fresh("op"))
    for ((List(leftTerm), leftCons) <- lhs;
         (List(rightTerm), rightCons) <- rhs) yield {
      val opAtom = datalog.op(opOut, leftTerm, op, rightTerm)
      (List(opOut), leftCons ++ rightCons :+ opAtom)
    }
  }
}

object FunctionalTest extends App {
  def factorial(fun: Functional): fun.Mod = {
    import fun._
    module("Factorial", List(
      function("fact", List("n" -> tint), tint,
        ifTrue(op(va("n"), "<", int(1)),
          int(1),
          op(
            va("n"),
            "*",
            call("fact", List(
              op(va("n"), "-", int(1))
            ))
          )
        )
      )
    ))
  }
  def fibonacci(fun: Functional): fun.Mod = {
    import fun._
    module("Fibonacci", List(
      function("fib", List("n" -> tint), tint,
        ifTrue(op(va("n"), "<=", int(2)),
          int(1),
          op(
            call("fib", List(
              op(va("n"), "-", int(2))
            )),
            "+",
            call("fib", List(
              op(va("n"), "-", int(1))
            ))
          )
        )
      )
    ))
  }

  private val factEval = factorial(FunctionalEval)("fact")
  private val fibEval = fibonacci(FunctionalEval)("fib")
  println("fact(5) = " + factEval(List(FunctionalEval.VInt(5))))
  println("fact(10) = " + factEval(List(FunctionalEval.VInt(10))))
  println("fib(5) = " + fibEval(List(FunctionalEval.VInt(5))))
  println("fib(10) = " + fibEval(List(FunctionalEval.VInt(10))))

  val functionalDatalogAST = new FunctionalDatalog {
    override val datalog: Datalog = DatalogModuleAST
  }
  private val factDatalog = factorial(functionalDatalogAST)
  println(factDatalog)
//  factDatalog(Map("input$fact" -> Set(Seq(5))))

}