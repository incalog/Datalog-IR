package inca.frontend.functional.verification

import inca.util.Gensym
import smtlib.theories.Core.{Equals, Implies, Not, True}
import smtlib.theories.Operations.OperationN
import smtlib.trees.Commands._
import smtlib.trees.Terms._

import scala.language.implicitConversions

object SMTlibScripts {

  // It would have probably been nicer to implement the scripts using the Operation trait
  // included in the scala-smtlib package

  object And extends OperationN {
    override val numRequired: Int = 2
    override val name = "and"
  }

  def generateInvariantAssertions(invariantNames: Seq[String], variableNames: Seq[String]): Seq[Term] = {
    variableNames.flatMap(vN => invariantNames.map(FunctionApplication(_, Seq(vN))))
  }

  def prove(sort: Sort, provable: Term, varNames: Seq[String], invariantNames: Seq[String]): Script = {
    val sortedVars = varNames.map(SortedVar(_, sort))
    val invariantAssertions = generateInvariantAssertions(invariantNames, varNames)
    Script(
      List(
        Push(1),
        Assert(Exists(sortedVars.head, sortedVars.tail,
          Not(if (invariantAssertions.isEmpty) {
            provable
          } else {
            if (invariantAssertions.size == 1) {
              Implies(invariantAssertions.head, provable)
            } else {
              Implies(And(invariantAssertions), provable)
            }
          }))),
        CheckSat(),
        Pop(1))
    )
  }

  def commutativity(aggrName: String, paramTypeName: String, invariantNames: Seq[String])(implicit gensym: Gensym): Script = {
    val varNames = for (_ <- 0 to 1) yield gensym.fresh("x")
    val provable: Term = Equals(
      FunctionApplication(aggrName, Seq(varNames.head, varNames(1))),
      FunctionApplication(aggrName, Seq(varNames(1), varNames.head)))
    prove(Sort(paramTypeName), provable, varNames, invariantNames)
  }

  def associativity(aggrName: String, paramTypeName: String, invariantNames: Seq[String])(implicit gensym: Gensym): Script = {
    val varNames = for (_ <- 0 to 2) yield gensym.fresh("x")
    val provable = Equals(
      FunctionApplication(aggrName, Seq(varNames.head, FunctionApplication(aggrName, Seq(varNames(1), varNames(2))))),
      FunctionApplication(aggrName, Seq(FunctionApplication(aggrName, Seq(varNames.head, varNames(1))), varNames(2))))
    prove(Sort(paramTypeName), provable, varNames, invariantNames)
  }

  def hasUnapply(aggrName: String, unapplyName: String, paramTypeName: String, invariantNames: Seq[String])(implicit gensym: Gensym): Script = {
    val varNames = for (_ <- 0 to 2) yield gensym.fresh("x")
    val provable = Implies(
      Equals(FunctionApplication(aggrName, Seq(varNames.head, varNames(1))), varNames(2)),
      Equals(FunctionApplication(unapplyName, Seq(varNames(2), varNames(1))), varNames.head)
    )
    prove(Sort(paramTypeName), provable, varNames, invariantNames)
  }

  def reflexivity(relName: String, dataName: String, invariantNames: Seq[String])(implicit gensym: Gensym): Script = {
    val varNames = Seq(gensym.fresh("x"))
    val provable = FunctionApplication(relName, Seq(varNames.head, varNames.head))
    prove(Sort(dataName), provable, varNames, invariantNames)
  }

  def transitivity(relName: String, dataName: String, invariantNames: Seq[String])(implicit gensym: Gensym): Script = {
    val varNames = for (_ <- 0 to 2) yield gensym.fresh("x")
    val provable = Implies(
      And(Seq(
        FunctionApplication(relName, Seq(varNames.head, varNames(1))),
        FunctionApplication(relName, Seq(varNames(1), varNames(2))))),
      FunctionApplication(relName, Seq(varNames.head, varNames(2)))
    )
    prove(Sort(dataName), provable, varNames, invariantNames)
  }

  def antisymmetry(relName: String, dataName: String, invariantNames: Seq[String])(implicit gensym: Gensym): Script = {
    val varNames = for (_ <- 0 to 1) yield gensym.fresh("x")
    val provable = Implies(
      And(Seq(
        FunctionApplication(relName, Seq(varNames.head, varNames(1))),
        FunctionApplication(relName, Seq(varNames(1), varNames.head))
      )),
      Equals(varNames.head, varNames(1))
    )
    prove(Sort(dataName), provable, varNames, invariantNames)
  }

  def soundness(abstractAggrName: String, concreteAggrName: String, concreteParamTypeName: String,
                paramBetaName: String, resultBetaName: String, poName: String, numParams: Int,
                invariantNames: Seq[String])(implicit gensym: Gensym): Script = {
    val varNames = for (_ <- 0 until numParams) yield gensym.fresh("x")
    val varFunImages = for (i <- 0 until numParams) yield FunctionApplication(paramBetaName, Seq(varNames(i)))
    val provable = FunctionApplication(poName, Seq(
      FunctionApplication(resultBetaName, Seq(FunctionApplication(concreteAggrName, varNames.map(StringToQualifiedIdentifier)))),
      FunctionApplication(abstractAggrName, varFunImages)
    ))
    prove(Sort(concreteParamTypeName), provable, varNames, invariantNames)
  }

  // CanDo: Allow different param types
  def monotonicity(paramTypeName: String, funName: String, paramPoName: String,
                   resultPoName: String, numParams: Int,
                   invariantNames: Seq[String])(implicit gensym: Gensym): Script = {
    val xVars = for (_ <- 0 until numParams) yield gensym.fresh("x")
    val yVars = for (_ <- 0 until numParams) yield gensym.fresh("y")
    val paramComparisons = for (i <- 0 until numParams) yield FunctionApplication(paramPoName, Seq(xVars(i), yVars(i)))
    val provable = Implies(
      if (paramComparisons.isEmpty) {
        True()
      } else {
        if (paramComparisons.size == 1) {
          paramComparisons.head
        } else {
          And(paramComparisons)
        }
      },
      FunctionApplication(resultPoName, Seq(
        FunctionApplication(funName, xVars.map(StringToQualifiedIdentifier)),
        FunctionApplication(funName, yVars.map(StringToQualifiedIdentifier))
      ))
    )
    prove(Sort(paramTypeName), provable, xVars ++ yVars, invariantNames)
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