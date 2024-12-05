package inca.ir.analysis

import inca.util.{Color, TextStyle}
import inca.util.{colorize, style}

trait AnalysisKey:
  val key: String
  val color: Color
  type Result

trait AnalysisResult:
  val akey: AnalysisKey

trait Analyzable:
  var analysis: Map[String, Set[Any]] = Map()
  private var colorMap: Map[String, Color] = Map()

  def storeAnalysisResult(res: AnalysisResult): Unit =
    analysis += res.akey.key -> Set(res)
    colorMap += res.akey.key -> res.akey.color

  def updateAnalysisResult(res: AnalysisResult): Unit =
    analysis += res.akey.key -> (analysis.getOrElse(res.akey.key, Set()) + res)
    colorMap += res.akey.key -> res.akey.color

  def getAnalysisResult(akey: AnalysisKey): Set[akey.Result] =
    analysis.getOrElse(akey.key, Set()).map(_.asInstanceOf[akey.Result])

  def analysisString: String =
    if (analysis.isEmpty)
      ""
    else
      analysis.foldLeft("") { case (acc, (key, value)) =>
        val annotation = s" :: $value".colorize(colorMap(key)).style(TextStyle.Bold)
        s"$acc$annotation"
      }