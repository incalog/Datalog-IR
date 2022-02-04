/*
package inca.frontend.functional.verification

import SMTLIB._

object PropertyScripts {

  def commutativity(aggrName: String): Script = Script(
    Seq(
      Push(),
      Assertion(Exists(Seq(SortedVariable("x", ZInt), SortedVariable("y", ZInt)),
        Call(Identifier("not"), Seq(Call(Identifier("="), Seq(
          Call(Identifier(aggrName), Seq(Identifier("x"), Identifier("y"))),
          Call(Identifier(aggrName), Seq(Identifier("y"), Identifier("x"))))))))),
      CheckSat(),
      Pop())
  )

  def associativity(aggrName: String): Script = Script(
    Seq(
      Push(),
      Assertion(Forall(Seq(SortedVariable("x", ZInt), SortedVariable("y", ZInt), SortedVariable("z", ZInt)),
        Call(Identifier("not"), Seq(Call(Identifier("="), Seq(
          Call(Identifier(aggrName), Seq(
            Identifier("x"), Call(Identifier(aggrName), Seq(Identifier("y"), Identifier("z"))))),
          Call(Identifier(aggrName), Seq(
            Call(Identifier(aggrName), Seq(Identifier("x"), Identifier("y"))), Identifier("z"))))))))),
      CheckSat(),
      Pop()
    )
  )

}
*/
