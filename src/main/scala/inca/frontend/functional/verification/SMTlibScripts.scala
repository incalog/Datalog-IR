package inca.frontend.functional.verification

import smtlib.theories.Core
import smtlib.trees.Commands._
import smtlib.trees.Terms._

import scala.language.implicitConversions

object SMTlibScripts {

  def commutativity(aggrName: String, paramTypeName: String): Script = {
    val sort = Sort(paramTypeName)
    Script(
      List(
        Push(1),
        Assert(Exists(SortedVar("x", sort), Seq(SortedVar("y", sort)),
          FunctionApplication("not", Seq(FunctionApplication("=", Seq(
            FunctionApplication(aggrName, Seq("x", "y")),
            FunctionApplication(aggrName, Seq("y", "x")))))))),
        CheckSat(),
        Pop(1))
    )
  }

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

  def hasUnapply(aggrName: String, unapplyName: String, paramTypeName: String): Script = {
    val sort = Sort(paramTypeName)
    Script(
      List(
        Push(1),
        Assert(Exists(SortedVar("x", sort), Seq(SortedVar("y", sort), SortedVar("z", sort)),
          FunctionApplication("and", Seq(
            FunctionApplication("=", Seq(FunctionApplication(aggrName, Seq("x", "y")), "z")),
            FunctionApplication("not", Seq(FunctionApplication("and", Seq(
              FunctionApplication("=", Seq(FunctionApplication(unapplyName, Seq("z", "y")), "x"))
            )))
            ))))),
        CheckSat(),
        Pop(1)
      )
    )
  }

  def invariant(dataName: String, relName: String, freshVarName: String): Script = {
    val sort = Sort(dataName)
    Script(List(
      Assert(Forall(SortedVar(freshVarName, sort), Seq(),
        FunctionApplication("=", Seq(
          FunctionApplication(relName, Seq(
            freshVarName
          )),
          Core.BoolConst(true)
        )))
      )))
  }

  def reflexivity(relName: String, dataName: String): Script = {
    val sort = Sort(dataName)
    Script(
      List(
        Push(1),
        Assert(Exists(SortedVar("x", sort), Seq(),
          FunctionApplication("not", Seq(FunctionApplication("=", Seq(
            FunctionApplication(relName, Seq("x", "x")),
            Core.BoolConst(true))))))),
        CheckSat(),
        Pop(1))
    )
  }

  //TODO testen, ob es auch ohne ==true geht
  def transitivity(relName: String, dataName: String): Script = {
    val sort = Sort(dataName)
    Script(
      List(
        Push(1),
        Assert(Exists(SortedVar("x", sort), Seq(SortedVar("y", sort), SortedVar("z", sort)),
          FunctionApplication("and", Seq(
            FunctionApplication("and", Seq(
              FunctionApplication("=", Seq(
                FunctionApplication(relName, Seq("x", "y")),
                Core.BoolConst(true))),
              FunctionApplication("=", Seq(
                FunctionApplication(relName, Seq("y", "z")),
                Core.BoolConst(true)))
            )),
            FunctionApplication("not", Seq(
              FunctionApplication("=", Seq(
                FunctionApplication(relName, Seq("x", "z")),
                Core.BoolConst(true)))
            ))
          )))),
        CheckSat(),
        Pop(1))
    )
  }

  // für alle x, y: x rel y und y rel x => x = y
  // Existiert x, y: sodass x rel y und y rel x und nicht x = y
  def antisymmetry(relName: String, dataName: String): Script = {
    val sort = Sort(dataName)
    Script(
      List(
        Push(1),
        Assert(Exists(SortedVar("x", sort), Seq(SortedVar("y", sort)),
          FunctionApplication("and", Seq(
            FunctionApplication("and", Seq(
              FunctionApplication("=", Seq(
                FunctionApplication(relName, Seq("x", "y")),
                Core.BoolConst(true))),
              FunctionApplication("=", Seq(
                FunctionApplication(relName, Seq("y", "x")),
                Core.BoolConst(true)))
            )),
            FunctionApplication("not", Seq(
              FunctionApplication("=", Seq("x", "y"))
            ))
          )))),
        CheckSat(),
        Pop(1))
    )
  }

  // für alle x: abstract(beta(x)) > beta(concrete(x))
  // Existiert x: abstract(beta(x)) nicht > beta(concrete(x))
  def soundnessNAry(abstractAggrName: String, concreteAggrName: String, concreteParamTypeName: String,
                    paramBetaName: String, resultBetaName: String, poName: String, numParams: Int): Script = {
    val concreteParamSort = Sort(concreteParamTypeName)
    val vars = for (i <- 2 to numParams) yield SortedVar(s"x$i", concreteParamSort)
    val varNames = for (i <- 1 to numParams) yield StringToQualifiedIdentifier(s"x$i")
    val varFunImages = for (i <- 1 to numParams) yield FunctionApplication(paramBetaName, Seq(s"x$i"))
    Script(
      List(
        Push(1),
        Assert(Exists(SortedVar("x1", concreteParamSort), vars,
          FunctionApplication("not", Seq(
            FunctionApplication(poName, Seq(
              FunctionApplication(resultBetaName, Seq(FunctionApplication(concreteAggrName, varNames))),
              FunctionApplication(abstractAggrName, varFunImages)
            ))
          ))
        )),
        CheckSat(),
        Pop(1)
      )
    )
  }

  // für alle x, y: wenn x < y => f(x) < f(y)
  // existiert x, y: x < y und nicht f(x) < f(y)
  def monotonicityNAry(paramTypeName: String, funName: String, resultPoName: String,
                       paramPoName: String, numParams: Int): Script = {
    val sort = Sort(paramTypeName)
    val xVars = for (i <- 2 to numParams) yield SortedVar(s"x$i", sort)
    val yVars = for (i <- 1 to numParams) yield SortedVar(s"y$i", sort)
    val xVarNames = for (i <- 1 to numParams) yield StringToQualifiedIdentifier(s"x$i")
    val yVarNames = for (i <- 1 to numParams) yield StringToQualifiedIdentifier(s"y$i")
    val paramComparisons = for (i <- 1 to numParams) yield FunctionApplication(paramPoName, Seq(s"x$i", s"y$i"))

    Script(
      List(
        Push(1),
        Assert(Exists(SortedVar("x1", sort), xVars ++ yVars,
          FunctionApplication("and", paramComparisons ++ Seq(
            FunctionApplication("not", Seq(
              FunctionApplication(resultPoName, Seq(
                FunctionApplication(funName, xVarNames),
                FunctionApplication(funName, yVarNames)
              ))
            ))
          ))
        )),
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