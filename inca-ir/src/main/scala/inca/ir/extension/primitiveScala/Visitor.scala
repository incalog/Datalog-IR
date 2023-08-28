package inca.ir.extension.primitiveScala

import inca.ir
import inca.ir.*
import inca.ir.Hint.preserveHints
import inca.ir.extensions.*
import inca.ir.visitors.BaseIRVisitor

import scala.collection.immutable.Seq

trait Visitor extends BaseIRVisitor:
  override def visitAtom(atom: Atom): Seq[Atom] = preserveHints(atom) {
    atom match
      case Application(out, fun, args) =>
        // TODO: We might want to support tuples here
        //  We could automatically flatten scala tuples here
        val newArgs = args.flatMap(visitTerm)
        visitTerm(out).map { o =>
          Application(o, fun, newArgs)
        }
      case _ => super.visitAtom(atom)
  }

  override def visitTerm(term: Term): Seq[Term] = preserveHints(term) {
    term match
      case Constant(value) => Seq(Constant(value))
      case _ => super.visitTerm(term)
  }

  override def visitType(ty: Type): Type = preserveHints(ty) {
    ty match
      case TScala(sty) => TScala(sty)
      case _ => super.visitType(ty)
  }