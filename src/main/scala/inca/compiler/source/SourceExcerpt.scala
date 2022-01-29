package inca.compiler.source

import scala.io.AnsiColor
import scala.jdk.StreamConverters._

sealed trait ExcerptConfig {
  def computeExcerpt(pre: String, core: String, post: String): (String, String, String)
}
case class ExcerptRelativeRegion(linesBefore: Int, linesAfter: Int) extends ExcerptConfig {
  override def computeExcerpt(pre: String, core: String, post: String): (String, String, String) = {
    val preLines = pre.reverse.lines().toScala(Iterator).take(linesBefore + 1).map(_.reverse).toArray.reverse
    val postLines = post.lines().toScala(Iterator).take(linesAfter + 1).toArray
    val preLine = preLines.mkString("\n")
    val postLine = postLines.mkString("\n")
    (preLine, core, postLine.stripTrailing())
  }
}
case class ExcerptAbsoluteRegion(startIndex: Int, endIndex: Int) extends ExcerptConfig {
  override def computeExcerpt(pre: String, core: String, post: String): (String, String, String) = {
    val preLine = pre.substring(startIndex)
    val postLine = post.substring(0, endIndex - pre.length - core.length)
    (preLine, core, postLine.stripTrailing())
  }
}
case class PaddedRegion(leftPad: String, config: ExcerptConfig, rightPad: String) extends ExcerptConfig {
  override def computeExcerpt(pre: String, core: String, post: String): (String, String, String) = {
    val (preS, coreS, postS) = config.computeExcerpt(pre, core, post)
    (leftPad + preS, coreS, postS + rightPad)
  }
}

class SourceExcerpt(loc: SourceLocation, config: ExcerptConfig) {
  if (loc.startIndex == SourceLocation.NoIndex || loc.endIndex == SourceLocation.NoIndex)
    throw new IllegalArgumentException

  val (preExcerpt, excerpt, postExcerpt): (String, String, String) = {
    val code = loc.source.code
    val (pre, corepost) = code.splitAt(loc.startIndex)
    val (core, post) = corepost.splitAt(loc.endIndex - pre.length)
    val strippedCore = core.stripTrailing()
    val coreStrip = core.substring(strippedCore.length)
    config.computeExcerpt(pre, strippedCore, coreStrip + post)
  }

  val colorStart: String = AnsiColor.YELLOW_B + AnsiColor.BLACK
  val colorEnd: String = AnsiColor.RESET
  def colored(s: String): String = {
    val sb = new StringBuilder
    s.lines().forEach { l =>
      sb ++= colorStart
      sb ++= l
      sb ++= colorEnd
      sb += '\n'
    }
    sb.deleteCharAt(sb.length() - 1)
    sb.toString()
  }

  def lines: String = preExcerpt + excerpt + postExcerpt
  def linesColored: String = preExcerpt + colored(excerpt) + postExcerpt
}
