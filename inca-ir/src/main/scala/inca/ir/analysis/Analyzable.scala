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
      analysis.foldLeft("") { case (acc, (key, value)) =>
        val valueS = value.size match
          case 0 => ""
          case 1 => value.head
          case _ => value.mkString("(", ", ", ")")
        val annotation = s" :: $valueS"
        s"$acc$annotation"
      }