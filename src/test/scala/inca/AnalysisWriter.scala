package inca

import java.io.{File, PrintWriter}

import inca.lang.fun.{CompileToGP, Fun}
import inca.lang.gp.{CompileToPSystem, GP}

object AnalysisWriter {
  def writeModule(module: Fun.Module): Unit = {
    val gp = CompileToGP.transformModule(module)
    AnalysisWriter.writeModule(gp)
  }

  def writeModule(module: GP.Module): Unit = {
    val trans = new CompileToPSystem(Seq(module))
    module.pats.foreach { pat =>
      val (name,source) = trans.transGraphPattern(pat)
      val file = new File(s"src/test/scala/inca/trans/generated/$name.scala")
      val writer = new PrintWriter(file)
      writer.write(source.syntax)
      writer.close()
    }
  }


}
