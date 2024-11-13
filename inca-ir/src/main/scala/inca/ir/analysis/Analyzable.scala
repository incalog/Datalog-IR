package inca.ir.analysis

trait AnalysisKey:
  val key: String
  type Result

trait AnalysisResult:
  val akey: AnalysisKey


trait Analyzable:
  var analysis: Map[String, Set[Any]] = Map()

  def storeAnalysisResult(res: AnalysisResult): Unit =
    analysis += res.akey.key -> Set(res)

  def updateAnalysisResult(res: AnalysisResult): Unit =
    analysis += res.akey.key -> (analysis.getOrElse(res.akey.key, Set()) + res)

  def getAnalysisResult(akey: AnalysisKey): Set[akey.Result] =
    analysis.getOrElse(akey.key, Set()).map(_.asInstanceOf[akey.Result])

  def analysisString: String =
    if (analysis.isEmpty)
      ""
    else
      val ansiColorBlue = "\u001b[34m";
      val colorStop = "\u001b[m"
      ansiColorBlue + analysis.mkString("{", ", ", "}") + colorStop