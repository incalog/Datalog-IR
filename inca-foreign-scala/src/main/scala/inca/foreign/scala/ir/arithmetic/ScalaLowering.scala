package inca.foreign.scala.ir.arithmetic

import inca.ir.{Atom, BaseIR, Eq, ModuleEntry, Name, Relation, Term, TermType, Type, Var}
import inca.ir.lowering.BaseLowering
import inca.ir.extension.arithmetic
import inca.ir.extension.arithmetic.{BinCompare, BinOp, DoubleNum, IntNum, TDouble, TInt}
import inca.ir.extension.block
import inca.foreign.scala.ir.{BaseScalaLowering, primitive}
import inca.foreign.scala.ir.primitive.{ScalaInca, ScalaTerm, ScalaType}
import inca.foreign.scala.syntax.Scala
import inca.ir
import inca.ir.Hint.preserveHints

trait ScalaLowering extends BaseScalaLowering:
  override val loweredIRs: Set[BaseIR] = Set(arithmetic.IR)
  override val requiredIRs: Set[BaseIR] = super.requiredIRs ++ Set(block.IR)

  override def visitAtom(atom: Atom): Seq[Atom] = preserveHints(atom) {
    atom match
      case BinCompare(lhs, rhs, op) =>
        val constTrue = ScalaTerm(Scala.BoolLiteral(true), ScalaType.bool, Seq())

        typedParams(lhs).zip(typedParams(rhs)).map {
          case ((l, TInt), (r, TInt)) =>
            Eq(constTrue, createScalaBinOp(op, ScalaType.bool, l -> ScalaType.int, r -> ScalaType.int))
          case ((l, TDouble), (r, TDouble)) =>
            Eq(constTrue, createScalaBinOp(op, ScalaType.bool, l -> ScalaType.double, r -> ScalaType.double))
          case ((l, lty), (r, rty)) =>
            throw IllegalStateException(s"Can not compare types $lty and $rty")
        }
      case _ =>
        super.visitAtom(atom)
  }

  private def assignmentBlock(lhs: Term, rhs: Term) = block.Block(Eq(lhs, rhs), lhs)

  override def visitTerm(term: Term): Seq[Term] = preserveHints(term) {
    term match
      case IntNum(i) =>
        Seq(ScalaTerm(Scala.IntLiteral(i), ScalaType.int, Seq()))
      case DoubleNum(d) =>
        Seq(ScalaTerm(Scala.DoubleLiteral(d), ScalaType.double, Seq()))
      case BinOp(lhs, rhs, op) =>
        typedParams(lhs).zip(typedParams(rhs)).map {
          case ((l, TInt), (r, TInt)) =>
            assignmentBlock(
              Var(freshName()),
              createScalaBinOp(op, ScalaType.int, l -> ScalaType.int, r -> ScalaType.int)
            )
          case ((l, TDouble), (r, TDouble)) =>
            assignmentBlock(
              Var(freshName()),
              createScalaBinOp(op, ScalaType.double, l -> ScalaType.double, r -> ScalaType.double)
            )
          case _ =>
            throw IllegalStateException(s"Can not lower incompatible binary operation: $term")
        }
      case _ =>
        super.visitTerm(term)
  }

  override def visitType(ty: Type): Type = preserveHints(ty) {
    ty match
      case TInt | TDouble => ScalaInca.compileType(ty)
      case _ => super.visitType(ty)
  }
