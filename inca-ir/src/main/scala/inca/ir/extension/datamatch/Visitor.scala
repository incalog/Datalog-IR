package inca.ir.extension.datamatch

import inca.ir.Hint.preserveHints
import inca.ir.visitors.BaseIRVisitor
import inca.ir.*
import inca.ir.extension.data

trait Visitor extends data.Visitor:
  override def visitAtom(atom: Atom): Seq[Atom] = preserveHints(atom)(atom match
    case Match(matchee, cases) =>
      for (t <- visitTerm(matchee)) yield
        val newCases = cases.map { case Case(name, vars, body) =>
          val newVars: Seq[Var] = vars.flatMap(visitTerm).map {
            case v@Var(_) => v
            case newTerm => throw new IllegalStateException(s"Unexpected term $newTerm for case $name")
          }
          Case(name, newVars, body.flatMap(visitAtom))
        }
        Match(t, newCases)
    case _ => super.visitAtom(atom))
