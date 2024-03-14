package inca.bddbddb.frontend.compile

import cats.parse.{Parser as P, Parser0 as P0}
import inca.bddbddb.syntax.Parser
import inca.ir.typing.IRTypechecker
import inca.util.FileUtil
import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite

import java.io.IOException
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{FileVisitResult, FileVisitor, Files, Path, Paths}
import scala.io.Source

class CompilerTest extends AnyFunSuite:
  val uri = classOf[CompilerTest].getResource("/inca/bddbddb").toURI

  test("Parse and compile all bddbddb Datalog files") {
    Files.walkFileTree(Paths.get(uri), new FileVisitor[Path] {
      override def preVisitDirectory(dir: Path, attrs: BasicFileAttributes): FileVisitResult =
        println(s"Entering ${dir.getFileName}")
        FileVisitResult.CONTINUE

      override def visitFile(p: Path, attrs: BasicFileAttributes): FileVisitResult =
        // We don't support the mapping (=>) feature yet, since it's not clear what it should do
        if (p.toString.endsWith("testMap.datalog"))
          return FileVisitResult.CONTINUE

        if (p.toString.endsWith(".datalog")) {
          println(s"Checking $p")
          val file = Source.fromURI(p.toUri)
          val sourceCode = file.getLines().mkString("\n")
          file.close()
          Parser.module.parseAll(sourceCode) match
            case Left(value) => assert(false)
            case Right(prog) =>
              val generateIR = new GenerateIR
              val fName = p.getFileName.toString.split('.').head
              val mod = generateIR.compileProgram(prog, fName)

              // Only typecheck, since we need no lowering
              val typechecker = new IRTypechecker
              typechecker.checkProgram(Seq(mod))
              typechecker.printTypeIO()
        }
        FileVisitResult.CONTINUE

      override def visitFileFailed(file: Path, exc: IOException): FileVisitResult =
        FileVisitResult.CONTINUE

      override def postVisitDirectory(dir: Path, exc: IOException): FileVisitResult =
        println(s"Leaving ${dir.getFileName}")
        FileVisitResult.CONTINUE
    })
  }

  /*test("Can compile PointsTo") {
    val content = FileUtil.readFileFromResource("inca/bddbddb/pa.datalog")
    val prog = Parser.parseModule(content)
    val generateIR = new GenerateIR
    val mod = generateIR.compileProgram(prog, "PointsTo")
    println(mod)

    val typechecker = new IRTypechecker
    typechecker.checkProgram(Seq(mod))
    typechecker.printTypeIO()
  }*/
