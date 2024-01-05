package inca.foreign.scala.ir.mono

import inca.foreign.scala.ir.primitive.{ScalaConstantTerm, ScalaInca, ScalaMonoAggregationOperator, ScalaType, ScalaLowering as BaseScalaLowering}
import inca.ir.*
import inca.ir.extension.aggregate.AggregationOperator
import inca.ir.extension.mono.{MapMonoDefinition, MonoAggregationOperator, NaiveSetMonoDefinition, SetMonoDefinition2}
import inca.ir.extension.{block, demand}


trait ScalaLowering extends BaseScalaLowering:

  /** Return the scala type name of input inca type */
  inline private def getSTName(ty: Type): String = visitType(ty).asInstanceOf[ScalaType].name

  override def visitAggregationOperator(op: AggregationOperator): AggregationOperator = op match
    case MonoAggregationOperator(SetMonoDefinition2(ty, rty)) =>
      val sty = getSTName(ty)
      ScalaMonoAggregationOperator(
        Name(s"ScalaSetMono$$${sty.replace("[", "$").replace("]", "$")}"),
        ScalaType(sty),
        ScalaType(s"Set[$sty]"),
        initCode = s"Set[$sty]()",
        addCode = s"(st: Set[$sty], a: $sty) => st + a"
      )
    case MonoAggregationOperator(NaiveSetMonoDefinition(ty)) =>
      val sty = getSTName(ty)
      ScalaMonoAggregationOperator(
        Name(s"ScalaNaiveSetMono$$${sty.replace("[", "$").replace("]", "$")}"),
        ScalaType(sty),
        ScalaType(s"Set[$sty]"),
        initCode = s"Set[$sty]()",
        addCode = s"(st: Set[$sty], a: $sty) => st + a"
      )
    case MonoAggregationOperator(MapMonoDefinition(keyTy, mono)) =>
      val kt = getSTName(keyTy)
      val inputVTy = getSTName(mono.typ.in)
      val stateVTy = getSTName(mono.typ.state)
      val outputVTy = getSTName(mono.typ.out)
      val valueAggOp = visitAggregationOperator(MonoAggregationOperator(mono)).asInstanceOf[ScalaMonoAggregationOperator]
      val initCode = valueAggOp.initCode
      val addCode = valueAggOp.addCode
      ScalaMonoAggregationOperator(
        name = s"ScalaMapMonoAggregation_${keyTy}_${mono.name}",
        inputTy = ScalaType(s"($kt, $inputVTy)"),
        stateTy = ScalaType(s"Map[$kt, $stateVTy]"),
        initCode = s"Map[$kt, $stateVTy]()",
        addCode =
          s"""(st: Map[$kt, $stateVTy], a: ($kt, $inputVTy)) =>
             | if st.contains(a._1) then st + (a._1 -> ($addCode (st(a._1), a._2)))
             | else st + (a._1 -> $addCode($initCode, a._2))
             |""".stripMargin
      )
    case _ => super.visitAggregationOperator(op)

def scalaSetMonoDefinition(ty: Type): SetMonoDefinition2 = {
  val scalaSetType = ScalaType(s"Set[${ScalaInca.compileType(ty).name}]")
  SetMonoDefinition2(ty, scalaSetType)
}

