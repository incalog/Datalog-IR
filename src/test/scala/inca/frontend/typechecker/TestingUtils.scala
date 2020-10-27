package inca.frontend.typechecker

import inca.frontend.core.Core._
import inca.frontend.util.Program
import org.scalatest.funsuite.AnyFunSuite

import scala.meta.XtensionQuasiquoteTerm

object TestingUtils {

  val evalIterable = Eval(Seq.empty, q"List(inca.analyzedData.Nat.Zero, inca.analyzedData.Nat.Succ(inca.analyzedData.Nat.Zero))")

  def buildFun(body: Body, expected: TypeAnno) = PatternFunction(None, Name("default"), Seq(Param(Name("x"), expected)), Seq(AnnoParam(None, expected)), Seq(body))

  def buildMod(body: Body, expected: TypeAnno) = Module(Name("default"), Seq.empty, Seq(buildFun(body, expected)))
  def getTypechecker(code: Body, expected: TypeAnno, extensions: Seq[TypecheckerExtension]) = new CoreTypechecker(null, Program(Seq(buildMod(code, expected))), extensions)

  def checkTypecheck(typechecker: CoreTypechecker, suite: AnyFunSuite): Unit = {
    val result = typechecker.typecheck()
    result match {
      case FailTypecheck(errors, warnings) =>
        println(errors)
        suite.fail()
      case SuccessTypecheck(warnings) =>
    }
  }
}
