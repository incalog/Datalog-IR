package inca.util

import java.io.{File, PrintWriter}

import inca.lang.gp.CompileToPSystem
import inca.lang.gp.GP._

import scala.meta.{Defn, Source}

object AnalysisWriter {
  def writeModule(module: Module): Unit = {
    val trans = new CompileToPSystem(Seq(module))
    module.pats.foreach { pat =>
      val source = trans.transGraphPattern(pat)
      val name = getAnalysisName(source)
      val file = new File(s"src/test/scala/inca/trans/generated/$name.scala")
      val writer = new PrintWriter(file)
      writer.write(source.syntax)
      writer.close()
    }
  }

  def getAnalysisName(source: Source): String =
    source.collect { case Defn.Class(_, name, _, _, _) => name.value }.head


}
