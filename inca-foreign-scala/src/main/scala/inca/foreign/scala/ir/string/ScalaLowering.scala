package inca.foreign.scala.ir.string

import inca.foreign.scala.ir.{BaseScalaLowering, primitive}
import inca.foreign.scala.ir.primitive.{ScalaInca, ScalaTerm, ScalaType}
import inca.foreign.scala.syntax.Scala
import inca.ir
import inca.ir.Hint.preserveHints
import inca.ir.extension.string.*
import inca.ir.extension.{block, string}
import inca.ir.lowering.BaseLowering
import inca.ir.*

// TODO: Refactor this with numeric lowering
trait ScalaLowering extends BaseScalaLowering:
  override val loweredIRs: Set[BaseIR] = Set(string.IR)
  override val requiredIRs: Set[BaseIR] = super.requiredIRs

  override def visitTerm(term: Term): Seq[Term] = preserveHints(term) {
    term match
      case StringLit(value) =>
        Seq(ScalaTerm(Scala.StringLiteral(value), ScalaType.string, Seq()))
      case StringConcat(lhs, rhs) =>
        typedParams(lhs).zip(typedParams(rhs)).map {
          case ((l, TString), (r, TString)) =>
            createScalaBinOp("+", ScalaType.string, lhs -> ScalaType.string, rhs -> ScalaType.string)
          case ((l, lty), (r, rty)) =>
            throw IllegalStateException(s"Can not concat types $lty and $rty")
        }
      case _ =>
        super.visitTerm(term)
  }

  override def visitType(ty: Type): Type = preserveHints(ty) {
    ty match
      case TString => ScalaInca.compileType(ty)
      case _ => super.visitType(ty)
  }
