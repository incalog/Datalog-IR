package inca.ir.extension.primitiveScala

import inca.Scala
import inca.ir.extension.primitiveScala.*
import inca.ir.typing.BaseIRTypechecker
import inca.ir.{Atom, TAny, Term, Type}

trait Typechecker extends BaseIRTypechecker:
  override def typecheck(atom: Atom): Unit = atom match
    case Application(out, fun, args) =>
      // TODO: We want to typecheck the Scala code
      typecheck(out)
      args.foreach(typecheck)
    case _ => super.typecheck(atom)

  override def typecheckInternal(term: Term, inferred: Option[Type]): Type = term match
    case Constant(value) =>
      // TODO: We want to typecheck the Scala code
      TScala(Scala.TypeName("Any"))
    case _ => super.typecheckInternal(term, inferred)
