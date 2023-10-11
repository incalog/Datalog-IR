package inca.ir.analysis

trait AnalysisKey:
  val key: String
  type Result
trait AnalysisResult:
  val akey: AnalysisKey


trait Analyzable:
  var analysis: Map[String, Any] = Map()

  def storeAnalysisResult(res: AnalysisResult): Unit =
    analysis += res.akey.key -> res
  def getAnalysisResult(akey: AnalysisKey): Option[akey.Result] =
    analysis.get(akey.key).map(_.asInstanceOf[akey.Result])

  def analysisString: String =
    if (analysis.isEmpty)
      ""
    else
      analysis.mkString("{",", ","}")