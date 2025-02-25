package inca.ddlog.backend

import inca.ddlog.syntax.Identifier
import inca.ir.{CompiledUnit, Type}
import inca.ir.execution.{ExecutorEngine, IRExecutor, Relation, RelationUpdateListener, transformEDBInput}
import inca.ddlog.backend.GenerateDDLog.cleanName
import inca.util.FileUtil

import java.nio.file.{Files, Path, Paths}
import scala.sys.process.*

import cats.parse.{Numbers, Parser as P, Parser0 as P0}

private object DDLogOutputParser:
  type RelationName = String
  type ParamName = String
  type Value = Any
  type NamedTupleEntry = (ParamName, Value)
  type NamedTuple = List[NamedTupleEntry]

  val whitespace: P[Unit] = P.charIn(" \t\r\n").void
  val whitespaces0: P0[Unit] = whitespace.rep0.void
  def spaced[A](p: P[A]): P[A] = p <* whitespaces0
  def op(c: Char): P[Unit] = spaced(P.char(c))
  def inBraces[A](p: P0[A]): P[A] = op('{') *> p <* op('}')
  private val letterDigit: P[Unit] = P.charIn(('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9') ++ Some('_')).void

  val ucIdentifier: P[String] = (P.charIn('A' to 'Z').void ~ letterDigit.rep0).string
  val lcIdentifier: P[String] = (P.charIn(('a' to 'z') :+ '_').void ~ letterDigit.rep0).string

  val stringLit: P[String] = P.char('"') *> P.charsWhile0(_ != '\"') <* P.char('"')
  val intLit: P[Int] = Numbers.signedIntString.map(_.toInt)
  val doubleLit: P[Double] = (Numbers.signedIntString ~ (P.char('.') *> Numbers.nonNegativeIntString)).string.map(_.toDouble)
  def adtLit: P[Value] = (ucIdentifier.string ~ P.defer(inBraces(argList))).map { (n, args) =>
    s"$n${args.map(_._2).mkString("(", ",", ")")}"
  }
  def literal: P[Value] = stringLit | doubleLit.backtrack | intLit.backtrack | adtLit
  def arg: P[(String, Any)] = (spaced(P.char('.') *> lcIdentifier) <* op('=')) ~ spaced(literal)
  def argList: P0[NamedTuple] = arg.repSep0(op(','))

  def relationTuples: P[(RelationName, NamedTuple)] = ucIdentifier ~ inBraces(argList) <* (op(':') ~ P.charIn(Seq('+', '-')) *> Numbers.nonNegativeIntString)

  def relationEntry: P[(RelationName, List[NamedTuple])] =
    (spaced(ucIdentifier <* P.char(':')) ~ spaced(relationTuples.backtrack).rep0).map {
      case (relName, tups) =>
        val tupsRelName = tups.map(_._1)
        assert(tupsRelName.forall(_ == relName))
        (relName, tups.map(_._2))
    }

  def parse(res: String): Map[RelationName, List[NamedTuple]] =
    relationEntry.rep0
      .map(_.toMap)
      .parseAll(res) match
        case Left(error) => throw IllegalArgumentException(s"Could not parse output: $error")
        case Right(relEntries) => relEntries


// 1. Create Rust file
// 2. Compile Rust file -> executable
// 3. Execute the executable
// 4. Parse the output
object Executor:
  lazy val ddlogProjectDir: String = Files.createTempDirectory("ddlog_").toFile.getCanonicalPath

class Executor extends IRExecutor:
  override val name: String = "DDlog"

  class Engine(executableArgs: Seq[String],
               inputFile: Path,
               relParams: Map[String, Seq[(String, Type)]],
               relNames: Map[String, String] // clean relation name -> real relation name
              ) extends ExecutorEngine:
    var inputDirty = true
    var cachedResult: Option[Seq[Relation]] = None
    var edbEntries: Set[Relation] = Set()

    private def executable: ProcessBuilder =
      val commandArg = executableArgs.last
      val prefix = executableArgs.dropRight(1)
      Process(prefix :+ s"$commandArg < ${inputFile.toAbsolutePath}")

    private def transformString(s: String): Any =
      val current = StringBuilder()
      var count = 0
      s.foreach{c =>
        if (c == '\\')
          count += 1
          current.append(c)
        else if (c == 'n' || c == 't' || c == '"')
          if ((count != 0) && (count%2==0))
            current.append('\\')
          current.append(c)
          count = 0
        else
          if ((count != 0) && (count % 2 == 1))
            current.append('\\')
          current.append(c)
          count = 0
      }
      val fixed = current.toString()
      current.clear()
      s"\"$fixed\""

    private def ddlogifyInput(value: Any): Any =
      transformEDBInput(value)(transformString, value => s"$value", value => s"$value", transformADT)
    
    private def transformADT(dataName: String, caseName: String, args: Seq[Any]): Any =
      if (args.nonEmpty)
        s"""${GenerateDDLog.cleanName(caseName)}${args.mkString("{", ",", "}")}"""
      else
        s"""${GenerateDDLog.cleanName(caseName)}"""

    private def createInputFile(includeTimestamp: Boolean): Unit =
      val edbInserts =
        if (edbEntries.nonEmpty)
          edbEntries.flatMap { rel =>
            val relName = Identifier.rel(cleanName(rel.name))
            rel.entries.map { tup =>
              val tuples = rel.flattenEntry(tup)
                .map(ddlogifyInput)
                .mkString("(", ",", ")")
              s"insert $relName$tuples"
            }
          }.mkString("", ";\n", ";\n")
        else
          ""

      val content =
        s"""
           |start;
           |$edbInserts
           |commit dump_changes;
           |${if (includeTimestamp) "timestamp;" else ""}
           |exit;
           |""".stripMargin
      Files.deleteIfExists(inputFile)
      FileUtil.writeFile(inputFile.toFile, content)

    private def execute():List[String] =
      createInputFile(false)
      executable.lazyLines_!.toList

    override def measure(rel: Relation): Long =
      createInputFile(true)
      val ts = executable.lazyLines_!.toList.last.split(": ")
      ts.last.toLong

    def insert(edb: Relation): Unit =
      val newEntries = edbEntries + edb
      inputDirty = newEntries != edbEntries
      edbEntries = newEntries

    def remove(edb:inca.ir.execution.Relation): Unit =
      val newEntries = edbEntries - edb
      inputDirty = newEntries != edbEntries
      edbEntries = newEntries

    def addUpdateListener(up: RelationUpdateListener): Unit =
      throw IllegalStateException("Incremental updates are currently not supported by the DDLog backend")

    def removeUpdateListener(up: RelationUpdateListener): Unit =
      throw IllegalStateException("Incremental updates are currently not supported by the DDLog backend")

    def readAll(): Seq[Relation] =
      val res = execute().mkString("\n")
      val parsedRes = DDLogOutputParser.parse(res).toSeq
      parsedRes.map { case (name, tups) =>
        Relation.from(relNames(name), relParams(name).map(_._1), tups.map(_.map(_._2)))
      }

    def read(rel: Relation): Relation = {
      val facts = readAll().find(_.name == rel.name) match {
        case Some(r) if rel.isEmpty => r
        case Some(r) =>
          val matches = r.entries.flatMap { el =>
            val flatEl = r.flattenEntry(el)
            val matches = rel.entries.exists { query =>
              val flatQuery = rel.flattenEntry(query)
              flatEl.zipAll(flatQuery, null, null).forall {
                case (e, null) => true
                case (e, q) => e == q
              }
            }
            if (matches) Some(flatEl) else None
          }
          Relation.from(r.name, r.parameterNames, matches)
        case _ => throw IllegalStateException(s"No relation named ${rel.name} found")
      }
      facts
    }


  override def instantiate(m: CompiledUnit): Engine =
    val prefix = Executor.ddlogProjectDir
    val ddlogExecPath = "ddlog"

    val Seq(lowered) = m.compiled
    val generateDDLog = new GenerateDDLog
    val contents = generateDDLog.compileModule(lowered)

    // Use one shared project for our executor to speed up DDLog compilation
    val progName = "Prog" // m.name.name
    val stdInFile = Paths.get(s"$prefix/$progName.dat")
    val dlfilePath = Paths.get(s"$prefix/$progName.dl")
    Files.deleteIfExists(dlfilePath)

    FileUtil.writeFile(dlfilePath.toFile, contents.toString)

    val genProjectProcess = stringToProcess(s"$ddlogExecPath -i ${dlfilePath.toAbsolutePath}")
    genProjectProcess.! match
      case 0 =>
      case _ => throw IllegalStateException("Failed to generate DDlog project")

    val fileName = dlfilePath.getFileName.toString
    val dotIndex = fileName.lastIndexOf(".")
    val fileNameWithoutExtension = if (dotIndex > 0)
      fileName.substring(0, dotIndex)
    else
      fileName

    // Combine the parent directory and the file name without extension
    val pathWithoutExtension: Path = dlfilePath.getParent.resolve(fileNameWithoutExtension + "_ddlog")
    val cargoBuildProcess = stringToProcess(s"cargo build --manifest-path $pathWithoutExtension/Cargo.toml --release")
    cargoBuildProcess.! match
      case 0 =>
      case _ => throw IllegalStateException("cannot build project")

    val processArgs = Seq("/bin/bash", "-c", s"$pathWithoutExtension/target/release/${progName}_cli")

    val relParams = lowered.relations.map { (n, r) =>
      Identifier.rel(cleanName(n)).toString -> r.params.map(p => Identifier.arg(cleanName(p.name)).toString -> p.ty)
    }
    val relNames = lowered.relations.map { (n, _) =>
      Identifier.rel(cleanName(n)).toString -> n
    }
    new Engine(processArgs, stdInFile, relParams, relNames)
