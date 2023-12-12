package inca.foreign.scala.ir.string

import inca.foreign.scala.ir.primitive
import inca.foreign.scala.ir.primitive.{ScalaLowering => BaseScalaLowering, ScalaConstantTerm, ScalaInca, ScalaTerm, ScalaType}
import inca.ir
import inca.ir.Hint.preserveHints
import inca.ir.extension.string.*
import inca.ir.extension.string
import inca.ir.*

trait ScalaLowering extends BaseScalaLowering:
  override val loweredIRs: Set[BaseIR] = Set(string.IR)
  override val requiredIRs: Set[BaseIR] = super.requiredIRs

  override def isTypeSupported(ty: Type): Boolean = ty match
    case TString => true
    case _ => super.isTypeSupported(ty)

  override def visitTerm(term: Term): Seq[Term] = preserveHints(term) {
    term match
      case StringLit(value) =>
        Seq(ScalaConstantTerm(s""""$value"""", ScalaType.string))
      case StringConcat(lhs, rhs) =>
        typedParams(lhs).zip(typedParams(rhs)).map {
          case ((l, TString), (r, TString)) =>
            createScalaBinOp("+", ScalaType.string, l -> ScalaType.string, r -> ScalaType.string)
          case ((l, lty), (r, rty)) =>
            throw IllegalStateException(s"Can not concat types $lty and $rty")
        }
      case ToString(term) =>
        visitTerm(term).map { t =>
          ScalaTerm(s"(s: Any) => s.toString", ScalaType.string, Seq(t))
        }
      case _ =>
        super.visitTerm(term)
  }
