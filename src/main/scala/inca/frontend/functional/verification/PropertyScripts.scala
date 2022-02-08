package inca.frontend.functional.verification

import smtlib.trees.Commands._
import smtlib.trees.Terms._

import scala.language.implicitConversions

object PropertyScripts {

  def commutativity(aggrName: String): Script = Script(
    List(
      Push(1),
      Assert(Exists(SortedVar("x", Sort("Int")), Seq(SortedVar("y", Sort("Int"))),
        FunctionApplication("not", Seq(FunctionApplication("=", Seq(
          FunctionApplication(aggrName, Seq("x", "y")),
          FunctionApplication(aggrName, Seq("y", "x")))))))),
      CheckSat(),
      Pop(1))
  )

  def associativity(aggrName: String): Script = Script(
    List(
      Push(1),
      Assert(Exists(SortedVar("x", Sort("Int")), Seq(SortedVar("y", Sort("Int")), SortedVar("z", Sort("Int"))),
        FunctionApplication("not", Seq(FunctionApplication("=", Seq(
          FunctionApplication(aggrName, Seq("x", FunctionApplication(aggrName, Seq("y", "z")))),
          FunctionApplication(aggrName, Seq(FunctionApplication(aggrName, Seq("x", "y")), "z")))))))),
      CheckSat(),
      Pop(1))
  )

  // TODO kann ich die hier benutzen?
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