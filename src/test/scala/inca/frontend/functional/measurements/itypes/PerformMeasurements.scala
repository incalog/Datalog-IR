package inca.frontend.functional.measurements.itypes

import inca.frontend.functional.executor.FunctionalExecutor
import inca.util.measurement.BenchmarkUtils.{Measurement, Timing, measurementsToCSV, ms, writeFile}
import inca.util.measurement.MemoryUtil

import scala.collection.mutable
import scala.meta.{XtensionParseInputLike, XtensionQuasiquoteTerm}

// set -Xss1G otherwise scalameta parse throws stackoverflow
object PerformMeasurements extends scala.App {
  // TODO how will the type checker for LetStar look like (needs support for lists or should we use ADT to encode lists?)?
  val code =
    s"""module TypeChecker
       |
       |data BindingList = Nil() | Cons(String, Exp, BindingList)
       |data Exp = Num(Int) | Var(String) | Add(Exp, Exp) | Lam(String, Type, Exp) | App(Exp, Exp) | Let(String, Exp, Exp) | LetStar(BindingList, Exp)
       |data Type = TInt() | TFun(Type, Type)
       |data Ctx = Empty() | Bind(String, Type, Ctx)
       |
       |@main def typeOf(ctx: Ctx, exp: Exp): Option[Type] = exp match {
       |  case Num(v) => Some(TInt())
       |  case Var(n) => lookup(ctx, n)
       |  case Add(l, r) => typeOf(ctx, l) match {
       |    case None => None
       |    case Some(lty) => lty match {
       |      case TInt() => typeOf(ctx, r) match {
       |        case None => None
       |        case Some(rty) => rty match {
       |          case TInt() => Some(TInt())
       |          case TFun(ty1, ty2) => None
       |        }
       |      }
       |      case TFun(ty1, ty2) => None
       |    }
       |  }
       |  case Lam(n, ty, b) =>
       |    typeOf(Bind(n, ty, ctx), b) match {
       |      case None => None
       |      case Some(ty2) => Some(TFun(ty, ty2))
       |    }
       |  case App(fun, arg) => typeOf(ctx, fun) match {
       |    case None => None
       |    case Some(funty) => funty match {
       |      case TInt() => None
       |      case TFun(ty1, ty2) =>
       |        typeOf(ctx, arg) match {
       |          case None => None
       |          case Some(argty) =>
       |            if (eqType(argty, ty1))
       |              Some(ty2)
       |            else
       |              None
       |        }
       |    }
       |  }
       |  case Let(n, bound, body) => typeOf(ctx, bound) match {
       |    case None => None
       |    case Some(boundty) => typeOf(Bind(n, boundty, ctx), body)
       |  }
       |  case LetStar(bindings, body) => extendCtx(ctx, bindings) match {
       |    case None => None
       |    case Some(extCtx) => typeOf(extCtx, body)
       |  }
       |}
       |
       |def extendCtx(ctx: Ctx, bindings: BindingList): Option[Ctx] = bindings match {
       |  case Nil() => Some(ctx)
       |  case Cons(name, bound, rest) => typeOf(ctx, bound) match {
       |    case None => None
       |    case Some(ty) => extendCtx(Bind(name, ty, ctx), rest)
       |  }
       |}
       |
       |def lookup(ctx: Ctx, n: String): Option[Type] = ctx match {
       |  case Empty() => None
       |  case Bind(n1, ty, rest) =>
       |    if (n1 == n) Some(ty)
       |    else lookup(rest, n)
       |}
       |
       |def eqType(ty1: Type, ty2: Type): Boolean = ty1 match {
       |  case TInt() => ty2 match {
       |    case TInt() => true
       |    case TFun(ofty1, ofty2) => false
       |  }
       |  case TFun(fty1, fty2) => ty2 match {
       |    case TInt() => false
       |    case TFun(ofty1, ofty2) =>
       |     eqType(fty1, ofty1) && eqType(fty2, ofty2)
       |  }
       |}
       |""".stripMargin

  // generate measurement configs
  val configs = MeasurementConfig.generate(200, 10, 40)

  val measurements = configs.flatMap { config =>
    println(config)
    // generate program and edit
    val prog = config.gen.generate(config.depth)
    val progEdit =  config.edit.edit(prog)

    val emptyCtx = q"Empty()"


    // load analysis
    val analysis = FunctionalExecutor.loadFunction(code)

    // collect garbage before running analysis
    MemoryUtil.collectGarbage()

    // initialize analysis
    val (initalLoadTime, initialQueryTime) = analysis.measure("typeOf", Seq(emptyCtx, toScalaMeta(prog)))

    val baseConfigName = config.gen.getClass.getSimpleName.replaceAllLiterally("$", "") + " " + config.edit.getClass.getSimpleName.replaceAllLiterally("$", "")
    println(s"$baseConfigName ${ms(initalLoadTime)}, ${ms(initialQueryTime)}")

    implicit val timing = Timing(config.warmupMeasurements, config.numMeasurements)
    val editTimes = mutable.ListBuffer[(Long, Long)]()
    val undoTimes = mutable.ListBuffer[(Long, Long)]()
    // do measurements
    (0 until config.warmupMeasurements + config.numMeasurements).foreach { ix =>
      editTimes += analysis.measure("typeOf", Seq(emptyCtx, toScalaMeta(progEdit)))
      undoTimes += analysis.measure("typeOf", Seq(emptyCtx, toScalaMeta(prog)))
    }

    Seq(
      Measurement(baseConfigName + " Edit", editTimes.map(_._1).toSeq),
      Measurement(baseConfigName + " Undo", undoTimes.map(_._2).toSeq))

  }
  println(measurementsToCSV(measurements))
  writeFile("benchmark/itypes/measurements.csv", measurementsToCSV(measurements))



  def toScalaMeta(exp: Exp): meta.Term = {
    exp.toString.parse[meta.Term].get
  }
}