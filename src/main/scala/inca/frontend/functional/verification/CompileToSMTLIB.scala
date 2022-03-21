package inca.frontend.functional.verification

import smtlib.theories.Core
import smtlib.trees.Terms.{Forall, FunctionApplication, Identifier, QualifiedIdentifier, SSymbol, Sort, SortedVar, Term}

object CompileToSMTLIB {

  def smtCall(funName: String, args: Seq[Term]): Term =
    FunctionApplication(QualifiedIdentifier(Identifier(SSymbol(funName))), args)

  def smtVarCall(varName: String): Term =
    QualifiedIdentifier(Identifier(SSymbol(varName)))

  def smtForall(sortedVars: Seq[SortedVar], body: Term): Term =
    Forall(sortedVars.head, sortedVars.tail, body)

  def smtSortedVar(name: String, sortName: String) =
    SortedVar(SSymbol(name), Sort(Identifier(SSymbol(sortName))))

  def smtTrue(): Term = Core.BoolConst(true)

  def smtFalse(): Term = Core.BoolConst(false)
}
