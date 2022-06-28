package inca.embedded

import inca.backend.hints.MagicSetHints.FixedAdornment
import inca.backend.transform.magic.demand.DemandTransformation
import inca.util.{Gensym, Scala, TupleOps}

trait Functional {
  type Mod
  type Fun
  type Exp
  type Typ

  def module(name: String, funs: List[Fun]): Mod
  def function(name: String, params: List[(String, Typ)], outType: Typ, body: Exp): Fun

//  val datalog: Datalog
//  def embedDatalog(p: datalog.Mod): Exp
//  def mergeDatalog(p1: Exp, p2: Exp): Exp
//  def solveDatalog(problem: Exp): Exp

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
  def ifTrue(cnd: Exp, thn: Exp, els: Exp): Exp
  def call(fun: String, args: List[Exp]): Exp
  def op(lhs: Exp, op: String, rhs: Exp): Exp
}

trait FunctionalTypeChecked extends Functional {

  case class Context(functions: Map[String, (List[Typ], Typ)], env: Map[String, Typ])

  override type Mod = Map[String, (List[Typ], Typ)]
  override type Fun = ((String, (List[Typ], Typ)), Context => Typ)
  override type Exp = Context => Typ

  abstract override def module(name: String, funs: List[Fun]): Mod = {
    val sigs = funs.map(_._1).toMap
    val ctx = Context(sigs, Map())
    val actualResults = funs.map(_._2(ctx))
    sigs
  }

  override def function(name: String, params: List[(String, Typ)], outType: Typ, body: Exp): Fun = {
    val sig = (name, (params.map(_._2), outType))
    (sig, ctx => body(ctx.copy(env = ctx.env ++ params)))
  }

  override def bool(b: Boolean): Context => Typ = _ => tbool
  override def int(v: Int): Context => Typ = _ => tint
  override def double(v: Double): Context => Typ = _ => tdouble
  override def string(s: String): Context => Typ = _ => tstring
  override def va(name: String): Context => Typ = ctx => ctx.env.getOrElse(name, throw new IllegalArgumentException(s"Unbound variable $name"))

  override def let(names: List[String], bound: Exp, body: Exp): Context => Typ = ctx => {
    if (names.size != 1)
      throw new UnsupportedOperationException(s"Cannot handle tuples yet")
    val binding = names.head -> bound(ctx)
    body(ctx.copy(env = ctx.env + binding))
  }

  override def ifTrue(cnd: Exp, thn: Exp, els: Exp): Context => Typ = ctx => {
    val cndTy = cnd(ctx)
    if (cndTy != tbool)
      throw new IllegalArgumentException(s"Condition must be bool but was $cndTy")
    val thnTy = thn(ctx)
    val elsTy = els(ctx)
    if (thnTy != elsTy)
      throw new IllegalArgumentException(s"Branches must have same type, but was $thnTy and $elsTy")
    thnTy
  }

  override def call(fun: String, args: List[Exp]): Context => Typ = ctx => ctx.functions.get(fun) match {
    case Some((params, res)) =>
      val argTys = args.map(_.apply(ctx))
      if (params != argTys)
        throw new IllegalArgumentException(s"Arguments of type $args do not match function parameters $params")
      res
    case None => throw new IllegalArgumentException(s"Unknown function $fun")
  }
  override def op(lhs: Exp, op: String, rhs: Exp): Context => Typ = ctx => {
    val lhsTy = lhs(ctx)
    val rhsTy = rhs(ctx)
    if (lhsTy == tbool && rhsTy == tbool) op match {
      case "==" | "!=" | "&&" | "||" => tbool
      case _ => throw new IllegalArgumentException(s"Unknown operator $op")
    } else if (lhsTy == tint && rhsTy == tint) op match {
      case "==" | "!=" | "<" | "<=" | ">" | ">=" => tbool
      case "+" | "-" | "*" | "/" => tint
      case _ => throw new IllegalArgumentException(s"Unknown operator $op")
    } else if (lhsTy == tdouble && rhsTy == tdouble) op match {
      case "==" | "!=" | "<" | "<=" | ">" | ">=" => tbool
      case "+" | "-" | "*" | "/" => tdouble
      case _ => throw new IllegalArgumentException(s"Unknown operator $op")
    } else if (lhsTy == tstring && rhsTy == tstring) op match {
      case "==" | "!=" => tbool
      case "+" => tstring
      case _ => throw new IllegalArgumentException(s"Unknown operator $op")
    } else {
      throw new IllegalArgumentException(s"No operator $op for values of type $lhsTy and $rhsTy")
    }
  }
}

trait FunctionalEval extends Functional {
  sealed trait Value
  case class VBoolean(b: Boolean) extends Value
  case class VInt(i: Int) extends Value
  case class VDouble(d: Double) extends Value
  case class VString(s: String) extends Value

  override type Typ = Unit
  override def tany: Typ = ()
  override def tbool: Typ = ()
  override def tint: Typ = ()
  override def tdouble: Typ = ()
  override def tstring: Typ = ()

  case class Context(functions: Map[String, (Context, List[Value]) => Value], env: Map[String, Value])

  /** runs the main function */
  override type Mod = String => List[Value] => Value
  /** runs the function */
  override type Fun = (String, (Context, List[Value]) => Value)
  /** yields the value of the expression */
  override type Exp = Context => Value

  private val scalaCompiler = new Scala.ScalaCompiler

  // evaluation
  override def module(name: String, funs: List[Fun]): Mod = name => mainArgs => {
    val functions = funs.toMap
    val ctx = Context(functions, Map())
    functions(name)(ctx, mainArgs)
  }

  override def function(name: String, params: List[(String, Typ)], outType: Typ, body: Exp): Fun = (name, (ctx, args) =>
    body(ctx.copy(env = params.view.map(_._1).zip(args).toMap))
  )

  // expressions
  override def bool(b: Boolean): Context => Value = _ => VBoolean(b)
  override def int(v: Int): Context => Value = _ => VInt(v)
  override def double(v: Double): Context => Value = _ => VDouble(v)
  override def string(s: String): Context => Value = _ => VString(s)
  override def va(name: String): Context => Value = ctx => ctx.env.getOrElse(name, throw new IllegalArgumentException(s"Unbound variable $name"))
  override def let(names: List[String], bound: Exp, body: Exp): Context => Value = ctx => {
    if (names.size != 1)
      throw new UnsupportedOperationException(s"Cannot handle tuples yet")
    val binding = names.head -> bound(ctx)
    body(ctx.copy(env = ctx.env + binding))
  }
  override def ifTrue(cnd: Exp, thn: Exp, els: Exp): Context => Value = ctx => cnd(ctx) match {
    case VBoolean(b) => if (b) thn(ctx) else els(ctx)
    case _ => throw new IllegalArgumentException(s"Type error: Expected boolean, but got $cnd")
  }

  override def call(fun: String, args: List[Exp]): Context => Value = ctx => ctx.functions.get(fun) match {
    case Some(f) => f(ctx, args.map(_.apply(ctx)))
    case None => throw new IllegalArgumentException(s"Unknown function $fun")
  }

  override def op(lhs: Exp, op: String, rhs: Exp): Context => Value = ctx => (lhs(ctx), rhs(ctx)) match {
    case (VBoolean(l), VBoolean(r)) => op match {
      case "==" => VBoolean(l == r)
      case "!=" => VBoolean(l != r)
      case "&&" => VBoolean(l && r)
      case "||" => VBoolean(l || r)
      case _ => throw new IllegalArgumentException(s"Unknown operator $op")
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
    case _ => throw new IllegalArgumentException(s"No operator $op for values $lhs and $rhs")
  }
}

trait FunctionalDatalog extends Functional {
  val datalog: Datalog with DatalogOperatorType

  private val gensym = new Gensym(Set())

  case class Context(funRes: Map[String, datalog.Typ], env: Map[String, datalog.Typ])

  type Mod = datalog.Mod
  type Fun = ((String, datalog.Typ), Context => datalog.Pat)
  type Tuple = List[datalog.Trm]
  type Exp = Context => (List[(Tuple, List[datalog.Ato])], datalog.Typ)
  type Typ = datalog.Typ

  override def module(name: String, funs: List[Fun]): Mod = {
    val ctx = Context(funs.map(_._1).toMap, Map())
    datalog.module(name, funs.map(_._2(ctx)))
  }

  override def function(name: String, params: List[(String, Typ)], outType: Typ, body: Exp): Fun = {
    val outParams = Seq((gensym.fresh("out"), outType))
    ((name, outType), ctx => {
      val bodies = for ((terms, cons) <- body(ctx.copy(env = params.toMap))._1)
        yield datalog.body(cons ++ outParams.zip(terms).map(pt => datalog.eq(datalog.va(pt._1._1), pt._2)))
      val adorn = FixedAdornment(params.map(_ => true) ++ outParams.map(_ => false))
      datalog.pattern(name, params ++ outParams, bodies, Set(adorn))
    })
  }

  // types
  override def tany: datalog.Typ = datalog.tany
  override def tbool: datalog.Typ = datalog.tbool
  override def tint: datalog.Typ = datalog.tint
  override def tdouble: datalog.Typ = datalog.tdouble
  override def tstring: datalog.Typ = datalog.tstring

  // expressions
  private def const(trm: datalog.Trm, ty: datalog.Typ): Exp = _ => (List((List(trm), List())), ty)
  override def bool(b: Boolean): Exp = const(datalog.bool(b), tbool)
  override def int(v: Int): Exp = const(datalog.int(v), tint)
  override def double(v: Double): Exp = const(datalog.double(v), tdouble)
  override def string(s: String): Exp = const(datalog.string(s), tstring)
  override def va(name: String): Exp = ctx => const(datalog.va(name), ctx.env(name))(ctx)

  override def let(names: List[String], bound: Exp, body: Exp): Exp = ctx => {
    if (names.size != 1)
      throw new UnsupportedOperationException(s"Cannot handle tuples yet")
    val vars = names.map(datalog.va)
    val boundCompiled = bound(ctx)
    val binding = names.head -> boundCompiled._2
    val bodyCompiled = body(ctx.copy(env = ctx.env + binding))
    val res = for ((boundTerms, boundCons) <- boundCompiled._1;
         (bodyTerm, bodyCons) <- bodyCompiled._1) yield {
      val eqs = vars.zip(boundTerms).map(vt => datalog.eq(vt._1, vt._2))
      (bodyTerm, boundCons ++ eqs ++ bodyCons)
    }
    (res, bodyCompiled._2)
  }

  override def ifTrue(cnd: Exp, thn: Exp, els: Exp): Exp = ctx => {
    val cndCompiled = cnd(ctx)
    val thnCompiled = thn(ctx)
    val elsCompiled = els(ctx)

    val thnRes =
      for ((Seq(cndTerm), cndCons) <- cndCompiled._1;
           (thnTerm, thnCons) <- thnCompiled._1)
      yield (thnTerm, cndCons ++ Seq(datalog.eq(cndTerm, datalog.bool(true))) ++ thnCons)
    val elsRes =
      for ((Seq(cndTerm), cndCons) <- cndCompiled._1;
           (elsTerm, elsCons) <- elsCompiled._1)
      yield (elsTerm, cndCons ++ Seq(datalog.eq(cndTerm, datalog.bool(false))) ++ elsCons)
    (thnRes ++ elsRes, thnCompiled._2)
  }

  override def call(fun: String, args: List[Exp]): Exp = ctx => {
    val outvars = List(datalog.va(gensym.fresh("out")))
    val outtype = ctx.funRes(fun)

    // create single call constraint when no arguments passed
    if (args.isEmpty) {
      (List((outvars, List(datalog.query(fun, outvars)))), outtype)
    } else {
      val res = for (tups <- TupleOps.cartesianProductList(args.map(_.apply(ctx)._1))) yield {
        val (argTerms, argCons) = tups.unzip
        (outvars, argCons.flatten :+ datalog.query(fun, argTerms.flatten ++ outvars))
      }
      (res, outtype)
    }
  }

  override def op(lhs: Exp, op: String, rhs: Exp): Exp = ctx => {
    val opOut = datalog.va(gensym.fresh("op"))
    val lhsCompiled = lhs(ctx)
    val rhsCompiled = rhs(ctx)
    val res = for ((List(leftTerm), leftCons) <- lhsCompiled._1;
         (List(rightTerm), rightCons) <- rhsCompiled._1) yield {
      val opAtom = datalog.op(opOut, leftTerm, lhsCompiled._2, op, rightTerm, rhsCompiled._2)
      (List(opOut), leftCons ++ rightCons :+ opAtom)
    }
    val ty = datalog.operatorType(op, lhsCompiled._2, rhsCompiled._2)
    (res, ty)
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

  val eval = new FunctionalEval {}
  private val factEval = factorial(eval)("fact")
  private val fibEval = fibonacci(eval)("fib")
  println("fact(5) = " + factEval(List(eval.VInt(5))))
  println("fact(10) = " + factEval(List(eval.VInt(10))))
  println("fib(5) = " + fibEval(List(eval.VInt(5))))
  println("fib(10) = " + fibEval(List(eval.VInt(10))))

  // compile functional to Datalog
  val functionalDatalogAST = new FunctionalDatalog {
    override val datalog = new DatalogDemandTransformed with DatalogOperatorType {
      override val target = new DatalogModuleAST with DatalogReplay {}
    }
  }
  val functionalDatalogEval = new FunctionalDatalog {
    override val datalog = new DatalogDemandTransformed with DatalogOperatorType {
      override val target = new DatalogEval with DatalogReplay {}
    }
  }
  lazy val functionalDatalogIncremental = new FunctionalDatalog {
    override val datalog = new DatalogDemandTransformed with DatalogOperatorType {
      override val target = new DatalogEvalIncremental with DatalogReplay {}
    }
  }
  private val factDatalogAST = factorial(functionalDatalogAST)(Set("fact"))
  println(factDatalogAST)

  private val factDatalogEval = factorial(functionalDatalogEval)(Set("fact"))
  val factIDB = factDatalogEval(Map(DemandTransformation.demandPatternExtensionalPrefix + "fact" -> Set(Seq(5))))
  println("fact IDB = " + factIDB("fact"))

  private val factDatalogIncremental = factorial(functionalDatalogIncremental)(Set("fact"))
  factDatalogIncremental.addObserver("fact", (tup, inserted) => println("fact " + (if (inserted) "insert " else "delete ") + tup))
  println("insert fact(3, ?)")
  factDatalogIncremental.modify(DemandTransformation.demandPatternExtensionalPrefix + "fact", Seq(3), true)
  println("insert fact(5, ?)")
  factDatalogIncremental.modify(DemandTransformation.demandPatternExtensionalPrefix + "fact", Seq(5), true)
  println("insert fact(5, ?)")
  factDatalogIncremental.modify(DemandTransformation.demandPatternExtensionalPrefix + "fact", Seq(5), true)
}
