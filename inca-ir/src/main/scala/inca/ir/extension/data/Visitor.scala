package inca.ir.extension.data

import inca.ir.visitors.BaseIRVisitor
import inca.ir.{Atom, Term, Type, Var}

trait Visitor extends BaseIRVisitor:
  override def visitAtom(atom: Atom): Seq[Atom] = atom match
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
    case _ => super.visitAtom(atom)

  override def visitTerm(term: Term): Seq[Term] = term match
    case Construct(name, data) => Seq(Construct(name, data.flatMap(visitTerm)))
    case _ => super.visitTerm(term)

  override def visitType(ty: Type): Type = ty match
    case TData(name) => TData(name)
    case _ => super.visitType(ty)