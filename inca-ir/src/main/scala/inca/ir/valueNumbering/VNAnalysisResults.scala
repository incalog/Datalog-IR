package inca.ir.valueNumbering

import inca.ir.analysis.{AnalysisKey, AnalysisResult}
import inca.ir.valueNumbering.VNTables.VNTablesTerms
import inca.ir.{Name, Term}

import scala.collection.mutable


case object BodyVNKey extends AnalysisKey{
  override val key: String = "BodyVNResults"
  override type Result = BodyVNResults
}

case class BodyVNResults(vnTables: VNTablesTerms) extends AnalysisResult{
  override val akey: BodyVNKey.type = BodyVNKey
}


case object ParamVNKey extends AnalysisKey{
  override val key :String = "ParamVNResults"
  override type Result = ParamVNResults
}

type ParamName = Name
case class ParamVNResults(var paramLeaders: Map[ParamName, Term]) extends AnalysisResult{
  override val akey: ParamVNKey.type = ParamVNKey
}
