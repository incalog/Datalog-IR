package inca.frontend.objectoriented.interpreter

import inca.frontend.objectoriented.core._
import inca.frontend.objectoriented.util.Collect

import scala.collection.{MapView, mutable}
import scala.reflect.runtime.{currentMirror, universe}
import scala.tools.reflect.ToolBox
import scala.meta.{XtensionQuasiquoteTermParam, XtensionQuasiquoteType}

trait CollectScalaCode extends Collect[(Expression, String)] {
  private def typeToScala(typ: Type): meta.Type = {
    typ match {
      case TScala(_) => typ.asScala
      case TClass(_) | TNull | TAny => t"Any"
      case TTuple(ts) => meta.Type.Tuple(ts.map(typeToScala).toList)
      case TSet(_) => t"Set[Any]"
    }
  }

  override def collectExpression(expr: Expression): Seq[(Expression, String)] = (expr match {
    case BaseLitExpr(code) =>
      Seq((expr, code.syntax))
    case BaseApplyExpr(fun, args) =>
      val paramsTyped = args.zipWithIndex.map { case (arg, ix) =>
        val argTyp = arg.typ.getOrElse(throw new IllegalStateException(s"Untyped expression $arg"))
        ("arg$" + ix, s"${typeToScala(argTyp)}")
      }.toList
      val scalaArgs = paramsTyped.map(_._1)
      val code = s"((${paramsTyped.map(p => s"${p._1}: ${p._2}").mkString(", ")}) => $fun(${scalaArgs.mkString(", ")}))"
      super.collectExpression(expr) ++ Seq((expr, code))
    case BaseApplyInfixExpr(left, op, right) =>
      val lhsTy = typeToScala(left.typ.getOrElse(throw new IllegalStateException(s"Untyped expression $left")))
      val rhsTy = typeToScala(right.typ.getOrElse(throw new IllegalStateException(s"Untyped expression $right")))
      val code = s"((left: $lhsTy, right: $rhsTy) => left $op right)"
      Seq((expr, code))
    case BaseApplyMethodExpr(recv, method, args) =>
      val paramsTyped = (recv +: args.getOrElse(Seq())).zipWithIndex.map { case (arg, ix) =>
        val argTyp = arg.typ.getOrElse(throw new IllegalStateException(s"Untyped expression $arg"))
        ("arg$" + ix, s"${typeToScala(argTyp)}")
      }.toList
      val scalaArgs = paramsTyped.map(_._1)
      val closureParams = s"(${paramsTyped.map(p => s"${p._1}: ${p._2}").mkString(", ")})"
      val closureBody = if (args.isDefined) {
        val argS = scalaArgs.tail.mkString("(", ", ", ")")
        s"${scalaArgs.head}.${method.raw}$argS"
      } else {
        s"${scalaArgs.head}.${method.raw}"
      }
      val code = s"($closureParams => $closureBody)"
      Seq((expr, code))
    case BaseApplyUnaryExpr(op, exp) =>
      val valueTy = typeToScala(exp.typ.getOrElse(throw new IllegalStateException(s"Untyped expression $exp")))
      val code = s"((value: $valueTy) => ${op.syntax} value)"
      Seq((expr, code))
    case _ =>
      Seq()
  }) ++ super.collectExpression(expr)
}

case class ScalaInterpreter(module: Module, precomputeScalaTerms: Boolean = true) {
  protected var imports: mutable.ListBuffer[meta.Import] = mutable.ListBuffer()

  private val collectScalaCode = new CollectScalaCode {}
  private val codeResultCache: MapView[Expression, Any] = {
    // MapView recomputes the values each time a key is accessed
    val lazyResults = collectScalaCode(module).toMap.view.mapValues(ScalaInterpreter.run)
    // Run all expressions by creating a map a second time
    if (precomputeScalaTerms)
      lazyResults.toMap.view
    else
      lazyResults
  }

  def eval(expr: Expression): Any = codeResultCache.get(expr) match {
    case Some(value) => value
    case None => throw new IllegalArgumentException(s"Uncached result for expression $expr")
  }

  def evalClosure(expr: Expression, args: Any*): Any = ScalaInterpreter.callClosure(eval(expr), args:_*)
}

object ScalaInterpreter {
  private lazy val toolbox: ToolBox[universe.type] = currentMirror.mkToolBox()

  def run(code: String): Any = {
    val tree = toolbox.parse(code)
    toolbox.eval(tree)
  }

  def callClosure(closure: Any, args: Any*): Any = closure match {
    case c: Function0[Any] => c()
    case c: Function1[Any, Any] => c(args(0))
    case c: Function2[Any, Any, Any] => c(args(0), args(1))
    case c: Function3[Any, Any, Any, Any] => c(args(0), args(1), args(2))
    case c: Function4[Any, Any, Any, Any, Any] => c(args(0), args(1), args(2), args(3))
    case c: Function5[Any, Any, Any, Any, Any, Any] => c(args(0), args(1), args(2), args(3), args(4))
    case c: Function6[Any, Any, Any, Any, Any, Any, Any] => c(args(0), args(1), args(2), args(3), args(4), args(5))
    case c: Function7[Any, Any, Any, Any, Any, Any, Any, Any] => c(args(0), args(1), args(2), args(3), args(4), args(5), args(6))
    case c: Function8[Any, Any, Any, Any, Any, Any, Any, Any, Any] => c(args(0), args(1), args(2), args(3), args(4), args(5), args(6), args(7))
    case c: Function9[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any] => c(args(0), args(1), args(2), args(3), args(4), args(5), args(6), args(7), args(8))
    case c: Function10[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any] => c(args(0), args(1), args(2), args(3), args(4), args(5), args(6), args(7), args(8), args(9))
    case c: Function11[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any] => c(args(0), args(1), args(2), args(3), args(4), args(5), args(6), args(7), args(8), args(9), args(10))
    case c: Function12[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any] => c(args(0), args(1), args(2), args(3), args(4), args(5), args(6), args(7), args(8), args(9), args(10), args(11))
    case c: Function13[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any] => c(args(0), args(1), args(2), args(3), args(4), args(5), args(6), args(7), args(8), args(9), args(10), args(11), args(12))
    case c: Function14[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any] => c(args(0), args(1), args(2), args(3), args(4), args(5), args(6), args(7), args(8), args(9), args(10), args(11), args(12), args(13))
    case c: Function15[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any] => c(args(0), args(1), args(2), args(3), args(4), args(5), args(6), args(7), args(8), args(9), args(10), args(11), args(12), args(13), args(14))
    case c: Function16[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any] => c(args(0), args(1), args(2), args(3), args(4), args(5), args(6), args(7), args(8), args(9), args(10), args(11), args(12), args(13), args(14), args(15))
    case c: Function17[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any] => c(args(0), args(1), args(2), args(3), args(4), args(5), args(6), args(7), args(8), args(9), args(10), args(11), args(12), args(13), args(14), args(15), args(16))
    case c: Function18[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any] => c(args(0), args(1), args(2), args(3), args(4), args(5), args(6), args(7), args(8), args(9), args(10), args(11), args(12), args(13), args(14), args(15), args(16), args(17))
    case c: Function19[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any] => c(args(0), args(1), args(2), args(3), args(4), args(5), args(6), args(7), args(8), args(9), args(10), args(11), args(12), args(13), args(14), args(15), args(16), args(17), args(18))
    case c: Function20[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any] => c(args(0), args(1), args(2), args(3), args(4), args(5), args(6), args(7), args(8), args(9), args(10), args(11), args(12), args(13), args(14), args(15), args(16), args(17), args(18), args(19))
    case c: Function21[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any] => c(args(0), args(1), args(2), args(3), args(4), args(5), args(6), args(7), args(8), args(9), args(10), args(11), args(12), args(13), args(14), args(15), args(16), args(17), args(18), args(19), args(20))
    case c: Function22[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any] => c(args(0), args(1), args(2), args(3), args(4), args(5), args(6), args(7), args(8), args(9), args(10), args(11), args(12), args(13), args(14), args(15), args(16), args(17), args(18), args(19), args(20), args(21))
    case _ => throw new IllegalArgumentException(s"Unsupported closure: $closure(..$args)")
  }
}
