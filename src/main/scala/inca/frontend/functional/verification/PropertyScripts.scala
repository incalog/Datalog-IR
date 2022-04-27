package inca.frontend.functional.verification

import smtlib.trees.Commands._
import smtlib.trees.Terms._
import scala.language.implicitConversions

object PropertyScripts {

  def commutativity(aggrName: String, paramTypeName: String): Script = Script(
    List(
      Push(1),
      Assert(Exists(SortedVar("x", Sort(paramTypeName)), Seq(SortedVar("y", Sort(paramTypeName))),
        FunctionApplication("not", Seq(FunctionApplication("=", Seq(
          FunctionApplication(aggrName, Seq("x", "y")),
          FunctionApplication(aggrName, Seq("y", "x")))))))),
      CheckSat(),
      Pop(1))
  )

  def associativity(aggrName: String, paramTypeName: String): Script = {
    val sort = Sort(paramTypeName)
    Script(
      List(
        Push(1),
        Assert(Exists(SortedVar("x", sort), Seq(SortedVar("y", sort), SortedVar("z", sort)),
          FunctionApplication("not", Seq(FunctionApplication("=", Seq(
            FunctionApplication(aggrName, Seq("x", FunctionApplication(aggrName, Seq("y", "z")))),
            FunctionApplication(aggrName, Seq(FunctionApplication(aggrName, Seq("x", "y")), "z")))))))),
        CheckSat(),
        Pop(1))
    )
  }

  def invertibility(aggrName: String, inverseName: String, paramTypeName: String): Script = {
    val sort = Sort(paramTypeName)
    Script(
      List(
        Push(1),
        Assert(Exists(SortedVar("x", sort), Seq(SortedVar("y", sort), SortedVar("z", sort)),
          FunctionApplication("and", Seq(
            FunctionApplication("=", Seq(FunctionApplication(aggrName, Seq("x", "y")), "z")),
            FunctionApplication("not", Seq(FunctionApplication("and", Seq(
              FunctionApplication("=", Seq(FunctionApplication(inverseName, Seq("z", "x")), "y")),
              FunctionApplication("=", Seq(FunctionApplication(inverseName, Seq("z", "y")), "x"))
            )))
          ))))),
        CheckSat(),
        Pop(1)
      )
    )
  }

  implicit def StringToSSymbol(s: String): SSymbol = {
    SSymbol(s)
  }
  implicit def StringToIdentifier(s: String): Identifier = {
    Identifier(SSymbol(s))
  }
  implicit def StringToQualifiedIdentifier(s: String): QualifiedIdentifier = {
    QualifiedIdentifier(Identifier(SSymbol(s)))
  }
}