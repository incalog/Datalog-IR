package inca.foreign.scala.ir.string

import inca.foreign.scala.ir.primitive
import inca.foreign.scala.ir.primitive.{ScalaConstantTerm, ScalaInca, ScalaTerm, ScalaType, ScalaLowering as BaseScalaLowering}
import inca.ir
import inca.ir.Hint.preserveHints
import inca.ir.extension.string.*
import inca.ir.extension.string
import inca.ir.*
import inca.ir.extension.arithmetic.TInt

trait ScalaLowering extends BaseScalaLowering:
  override def name: String = "StringScalaLowering"

  override def isTypeSupported(ty: Type): Boolean = ty match
    case TString => true
    case _ => super.isTypeSupported(ty)

  override def visitAtom(atom: Atom): Seq[Atom] = atom match
    case RegexMatch(t, pattern, neg) =>
      typedParams(t).zip(typedParams(pattern)).map {
        case ((l, TString), (r, TString)) =>
          val sty = compileType(TString)
          val flag = if (neg) ScalaConstantTerm.FALSE else ScalaConstantTerm.TRUE
          val lambdaCode = s"(str: ${sty.name}, pat: ${sty.name}) => pat.r.matches(str)"
          Eq(flag, ScalaTerm(lambdaCode, ScalaType.bool, Seq(l, r)))
        case ((l, lty), (r, rty)) =>
          throw IllegalStateException(s"Can not concat types $lty and $rty")
      }
    case _ =>
      super.visitAtom(atom)

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
      case Substring(t, index, length) =>
        val strParams = typedParams(t)
        val indexParams = typedParams(index)
        val lengthParams = typedParams(length)

        strParams.zip(indexParams).zip(lengthParams).map {
          case (((str, strTy@TString), (start, startTy)), (len, lenTy)) =>
            val sty = compileType(strTy)
            val startScalaTy = ScalaInca.compileType(startTy)
            val lenScalaTy = ScalaInca.compileType(lenTy)
            val lambdaCode =
              s"(str: ${sty.name}, start: ${startScalaTy.name}, len: ${lenScalaTy.name}) => str.substring(start, start + len)"
            ScalaTerm(lambdaCode, ScalaType.string, Seq(str, start, len))
          case (((_, strTy), (_, startTy)), (_, lenTy)) =>
            throw IllegalStateException(s"Unexpected types in Substring: $strTy, $startTy, $lenTy")
        }
      case StringLength(t) =>
        typedParams(t).map {
          case (t, ty@TString) =>
            val sty = compileType(ty)
            val lambdaCode = s"(arg: ${sty.name}) => arg.length"
            ScalaTerm(lambdaCode, ScalaType.int, Seq(t))
          case (_, strTy) =>
            throw IllegalStateException(s"Unexpected types in StringLength: $strTy")
        }
      case OrdinalNumber(t) =>
        typedParams(t).map {
          case (t, ty@TString) =>
            val sty = compileType(ty)
            val lambdaCode = s"(arg: ${sty.name}) => arg.hashCode"
            ScalaTerm(lambdaCode, ScalaType.int, Seq(t))
          case (_, strTy) =>
            throw IllegalStateException(s"Unexpected types in OrdinalNumber: $strTy")
        }
      case ToString(term) =>
        visitTerm(term).map { t =>
          ScalaTerm(s"(s: Any) => s.toString", ScalaType.string, Seq(t))
        }
      case _ =>
        super.visitTerm(term)
  }