package inca.foreign.scala.ir.tuple
import inca.foreign.scala.ir.primitive.{ScalaConstantTerm, ScalaInca, ScalaMonoAggregationOperator, ScalaTerm, ScalaType, IR as scalaIR, ScalaLowering as BaseScalaLowering}
import inca.ir.{BaseIR, Name, RefByName, Term, Type, Var}
import inca.ir.Hint.preserveHints
import inca.ir.extension.aggregate.AggregationOperator
import inca.ir.extension.foreign.ForeignTerm
import inca.ir.extension.tuple.{Project, TTuple, TupleLit, IR as tupleIR}
import inca.ir.extension.mono.{MonoAggregationOperator, NaiveSetMonoDefinition}

trait ScalaLowering extends BaseScalaLowering:
  override def isTypeSupported(ty: Type): Boolean = ty match
    case TTuple(tys) => true
    case _ => super.isTypeSupported(ty)

  override def visitTerm(term: Term): Seq[Term] = preserveHints(term) { term match
    case TupleLit(ts) =>
      val vts = ts.flatMap(visitTerm).zip(ts.map(t => t.typ.get.ty))
      Seq(ScalaTerm(
        s"${vts.zipWithIndex.map{ case ((_, ty), i) => s"v$i: ${ScalaInca.compileType(ty).name}"}.mkString("(", ", ", ")")} => " +
          s"${vts.indices.map(i => s"v$i").mkString("(", ",", ")")}",
        ScalaType(vts.map((_, ty) => ScalaInca.compileType(ty).name).mkString("(", ", ", ")")),
        vts.map((vt, _) => vt)
      ))
    case Project(t, idx) =>
      val ity = t.typ.get.ty.asInstanceOf[TTuple]
      require(0 <= idx && idx < ity.tys.size)
      val sty = visitType(t.typ.get.ty).asInstanceOf[ScalaType]
      Seq(ScalaTerm(
        s"(t: ${sty.name}) => t($idx)",
        ScalaInca.compileType(ity.tys(idx)),
        Seq(t)
      ))
    case _ => super.visitTerm(term)
  }

  private def createValidScalaName(s: String): String =
    Seq("(", ")", "[", "]").foldLeft(s)((t, s) => s.replace(t, "$"))