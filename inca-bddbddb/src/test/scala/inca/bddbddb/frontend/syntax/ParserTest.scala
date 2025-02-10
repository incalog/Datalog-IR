package inca.bddbddb.frontend.syntax

import cats.parse.{Parser as P, Parser0 as P0}
import inca.bddbddb.syntax.Parser
import inca.ir.Name
import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite

import java.io.IOException
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{FileVisitResult, FileVisitor, Files, Path, Paths}
import scala.io.Source
import scala.reflect.ClassTag

class ParserTest extends AnyFunSuite {

  val uri = classOf[ParserTest].getResource("/inca/bddbddb").toURI

  test("Parse all bddbddb Datalog files") {
    Files.walkFileTree(Paths.get(uri), new FileVisitor[Path] {
      override def preVisitDirectory(dir: Path, attrs: BasicFileAttributes): FileVisitResult =
        //ln(s"Entering ${dir.getFileName}")
        FileVisitResult.CONTINUE

      override def visitFile(p: Path, attrs: BasicFileAttributes): FileVisitResult =
        if (p.toString.endsWith(".datalog")) {
          //println(s"Parsing $p")
          val file = Source.fromURI(p.toUri)
          val sourceCode = file.getLines().mkString("\n")
          file.close()
          testSuccessAny(Parser.module, true)(sourceCode)
        }
        FileVisitResult.CONTINUE

      override def visitFileFailed(file: Path, exc: IOException): FileVisitResult =
        FileVisitResult.CONTINUE

      override def postVisitDirectory(dir: Path, exc: IOException): FileVisitResult =
        //println(s"Leaving ${dir.getFileName}")
        FileVisitResult.CONTINUE
    })
  }

  def explainParseError(e: P.Error): String =
    val input = e.input.getOrElse("")
    val startLineIndex = input.lastIndexOf('\n', e.failedAtOffset - 1).max(0)
    val endLineIndex = input.indexOf('\n', e.failedAtOffset + 1)
    val relevant = input.substring(startLineIndex, if (endLineIndex == -1) input.length else endLineIndex)
    s"""Parse error, expected ${e.expected.toList} in
       |$relevant
       |""".stripMargin


  private def testSuccessAny[T](parser: P0[Any], silent: Boolean = true): String => Assertion =
    (input: String) => {
      parser.parseAll(input) match
        case Left(value) => assert(false, explainParseError(value))
        case Right(value) =>
          if (!silent) println(value)
          assert(true)
    }
}
