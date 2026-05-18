package inca.frontend.oodl.typechecker

import inca.frontend.oodl.syntax.*
import inca.ir.extension.block
import inca.ir.typing.IRTypechecker
import org.scalatest.funsuite.AnyFunSuite

import java.io.IOException
import java.nio.file.*
import java.nio.file.attribute.BasicFileAttributes
import scala.io.Source

class TypecheckerTest extends AnyFunSuite {

  val uri = classOf[TypecheckerTest].getResource("/objectoriented").toURI

  def testTypecheck(code: String): Unit =
    val checker = new Typechecker
    val module = Parser.parseModule(code)
    //println(module)
    checker.typecheck(module)
    //checker.printTypeIO()

    /*val ssa = new SSA
    val ssaModule = ssa.compileModule(module)
    println(ssaModule)
    checker.typecheck(ssaModule)
    checker.printTypeIO()*/

    assertResult(Nil)(checker.getErrors)

  Files.walkFileTree(Paths.get(uri), new FileVisitor[Path] {
    override def preVisitDirectory(dir: Path, attrs: BasicFileAttributes): FileVisitResult =
      //println(s"Entering ${dir.getFileName}")
      FileVisitResult.CONTINUE

    override def visitFile(p: Path, attrs: BasicFileAttributes): FileVisitResult =
      if (p.toString.endsWith(".oodl")) {
        test(s"Type check oodl file ${p.getFileName}") {
          val file = Source.fromURI(p.toUri)
          val sourceCode = file.getLines().mkString("\n")
          file.close()
          testTypecheck(sourceCode)
        }
      }
      FileVisitResult.CONTINUE

    override def visitFileFailed(file: Path, exc: IOException): FileVisitResult =
      FileVisitResult.CONTINUE

    override def postVisitDirectory(dir: Path, exc: IOException): FileVisitResult =
      //println(s"Leaving ${dir.getFileName}")
      FileVisitResult.CONTINUE
  })
}
