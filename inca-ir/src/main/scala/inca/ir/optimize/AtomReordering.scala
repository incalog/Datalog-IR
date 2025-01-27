package inca.ir.optimize

import inca.ir.{Atom, Body, Eq, Var}
import inca.ir.Hint.preserveHints
import inca.ir.visitors.IRVisitor

class AtomOrdering(atoms: Seq[Atom]) extends Ordering[Atom]:
  override def compare(x: Atom, y: Atom): Int =
    val xUsesY = x.boundVars.exists(y.unboundVars.contains)
    val yUsesX = y.boundVars.exists(x.unboundVars.contains)
    if (xUsesY) {
      // y must occur before x
      1
    } else if (yUsesX) {
      // x must occur before y
      -1
    } else {
      // independent atoms, order by atom kind
      (x, y) match
        case (_: Eq, _) => -1
        case (_, _: Eq) => 1
        case _ => 0
    }

trait AtomReordering extends Optimizer, IRVisitor:
  override def name: String = s"Atom reordering"
  
  override def visitBody(body: Body): Seq[Body] = preserveHints(body) {
    val ordering = new AtomOrdering(body.atoms)
    val reordered = body.atoms.sorted(using ordering)
    println(s"was: ${body.atoms}\nnow: ${reordered}")
    Seq(Body(reordered))
  }
