package inca.frontend.functional.compile

import inca.frontend.functional.syntax.*
import inca.ir.extension.block
import inca.ir.typing.IRTypechecker
import inca.util.compileroptions.CompilerOptions
import org.scalatest.funsuite.AnyFunSuite

import java.io.IOException
import java.nio.file.*
import java.nio.file.attribute.BasicFileAttributes
import scala.io.Source
import math.Ordered.orderingToOrdered

class CompilerTest extends AnyFunSuite {
  val options = CompilerOptions.fromResource("functional/Options.ini")
  val uri = classOf[CompilerTest].getResource("/functional").toURI;

  def testCompile(code: String): Unit =
    val compiler = new GenerateIR
    val module = Parser.parseModule(code)
    val compiled = CompiledFunctionalModule(module, options)
    compiled.checked
    compiled.setPipeline(CompiledFunctionalModule.pipeline)
    compiled.lowered

  Files.walkFileTree(Paths.get(uri), new FileVisitor[Path] {
    override def preVisitDirectory(dir: Path, attrs: BasicFileAttributes): FileVisitResult =
      println(s"Entering ${dir.getFileName}")
      FileVisitResult.CONTINUE
    override def visitFile(p: Path, attrs: BasicFileAttributes): FileVisitResult =
      if (p.toString.endsWith(".finca")) {
        test(s"Compile functional IncA file ${p.getFileName}") {
          val file = Source.fromURI(p.toUri)
          val sourceCode = file.getLines().mkString("\n")
          file.close()
          testCompile(sourceCode)
        }
      }
      FileVisitResult.CONTINUE
    override def visitFileFailed(file: Path, exc: IOException): FileVisitResult =
      FileVisitResult.CONTINUE
    override def postVisitDirectory(dir: Path, exc: IOException): FileVisitResult =
      println(s"Leaving ${dir.getFileName}")
      FileVisitResult.CONTINUE
  })
}
