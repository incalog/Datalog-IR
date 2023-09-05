package inca.compiler.source

import java.nio.file.Files
import java.nio.file.Path
import scala.jdk.StreamConverters._

/** A source */
trait Source {
  def code: String
  def lines: Iterator[String] = code.lines().toScala(Iterator)
}
case object NoSource extends Source {
  def code: String = throw new UnsupportedOperationException
}
case class SourceFile(f: Path) extends Source {
  lazy val code: String = Files.readString(f)
}
case class SourceString(code: String) extends Source
