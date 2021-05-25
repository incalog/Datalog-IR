package inca.frontend.functional.measurements.itypes

import inca.backend.ir.DatalogPrinter
import inca.frontend.functional.executor.FunctionalExecutor

import scala.meta.{XtensionParseInputLike, XtensionQuasiquoteTerm}

// set -Xss1G otherwise scalameta parse throws stackoverflow
object PerformMeasurements extends scala.App {
  // TODO how will the type checker for LetStar look like (needs support for lists or should we use ADT to encode lists?)?
  val code =
    s"""module TypeChecker
       |
       |// data BindingList = Nil() | Cons(String, Exp, BindingList)
       |data Exp = Num(Int) | Var(String) | Add(Exp, Exp) | Lam(String, Type, Exp) | App(Exp, Exp) | Let(String, Exp, Exp)
       |data Type = TInt() | TFun(Type, Type)
       |// data MbType = NoneType() | SomeType(Type)
       |data Ctx = Empty() | Bind(String, Type, Ctx)
       |
       |// @main def typeOf(ctx: Ctx, exp: Exp): MbType = exp match {
       |//   case Num(v) => SomeType(TInt())
       |//   case Var(n) => lookup(ctx, n)
       |//   case Add(l, r) => typeOf(ctx, l) match {
       |//     case NoneType() => NoneType()
       |//     case SomeType(lty) => lty match {
       |//       case TInt() => typeOf(ctx, r) match {
       |//         case NoneType() => NoneType()
       |//         case SomeType(rty) => rty match {
       |//           case TInt() => SomeType(TInt())
       |//           case TFun(ty1, ty2) => NoneType()
       |//         }
       |//       }
       |//       case TFun(ty1, ty2) => NoneType()
       |//     }
       |//   }
       |//   case Lam(n, ty, b) =>
       |//     typeOf(Bind(n, ty, ctx), b) match {
       |//       case NoneType() => NoneType()
       |//       case SomeType(ty2) => SomeType(TFun(ty, ty2))
       |//     }
       |//   case App(fun, arg) => typeOf(ctx, fun) match {
       |//     case NoneType() => NoneType()
       |//     case SomeType(funty) => funty match {
       |//       case TInt() => NoneType()
       |//       case TFun(ty1, ty2) =>
       |//         typeOf(ctx, arg) match {
       |//           case NoneType() => NoneType()
       |//           case SomeType(argty) =>
       |//             if (eqType(argty, ty1))
       |//               SomeType(ty2)
       |//             else
       |//               NoneType()
       |//         }
       |//     }
       |//   }
       |//   case Let(n, bound, body) => typeOf(ctx, bound) match {
       |//     case SomeType(boundty) => typeOf(Bind(n, boundty, ctx), body)
       |//     case NoneType() => NoneType()
       |//   }
       |// }
       |//
       |// def lookup(ctx: Ctx, n: String): MbType = ctx match {
       |//   case Empty() => NoneType()
       |//   case Bind(n1, ty, rest) =>
       |//     if (n1 == n) SomeType(ty)
       |//     else lookup(rest, n)
       |// }
       |//
       |// def eqType(ty1: Type, ty2: Type): Boolean = ty1 match {
       |//   case TInt() => ty2 match {
       |//     case TInt() => true
       |//     case TFun(ofty1, ofty2) => false
       |//   }
       |//   case TFun(fty1, fty2) => ty2 match {
       |//     case TInt() => false
       |//     case TFun(ofty1, ofty2) =>
       |//      eqType(fty1, ofty1) && eqType(fty2, ofty2)
       |//   }
       |// }
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
       |    case Some(boundty) => typeOf(Bind(n, boundty, ctx), body)
       |    case None => None
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
  val configs = MeasurementConfig.generate(1, 30, 40)
  configs.headOption.foreach { config =>
    val prog = config.gen.generate(config.depth)
    val progEdit =  config.edit.edit(prog)
    println(prog)
    println(Exp.numOfExp(prog))
//
    val analysis = FunctionalExecutor.loadFunction(code)
    println(DatalogPrinter.prettyModule(analysis.compiled.optimized)(false))

    val (loading, query) = analysis.measure("typeOf", Seq(q"Empty()", toScalaMeta(prog)))
//    println(s"Time to fill database: ${loading}ms")
//    println(s"Time to process query: ${query}ms")

//    println(t)
    // analysis.execute("typeOf", Seq(toScalaMeta(progEdit)), deleteInput = true)
    // analysis.execute("typeOf", Seq(toScalaMeta(prog)), deleteInput = true)

  }

  def toScalaMeta(exp: Exp): meta.Term = {
    exp.toString.parse[meta.Term].get
  }
}