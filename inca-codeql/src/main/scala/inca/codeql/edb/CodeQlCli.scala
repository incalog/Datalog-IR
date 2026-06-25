package inca.codeql.edb

import inca.codeql.compile.CompiledCodeQlUnit
import inca.codeql.syntax.{PredicateDecl, QlType}
import _root_.inca.ir.Name
import _root_.inca.ir.execution.{Relation, Relation as ExecutionRelation}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import scala.sys.process.{Process, ProcessLogger}

case class EdbExportQuery(relation: Name, qlSource: String)

trait CommandRunner:
  def run(command: Seq[String]): Unit

object CommandRunner:
  val system: CommandRunner = new CommandRunner:
    override def run(command: Seq[String]): Unit =
      val stdout = StringBuilder()
      val stderr = StringBuilder()
      val exitCode = Process(command).!(ProcessLogger(
        line => stdout.append(line).append('\n'),
        line => stderr.append(line).append('\n')
      ))
      if exitCode != 0 then
        throw IllegalStateException(
          s"Command failed with exit code $exitCode: ${command.mkString(" ")}\n${stderr.result()}${stdout.result()}"
        )

case class CodeQlCli(
  executable: Path = Paths.get(System.getProperty("user.home"), "CodeQL", "codeql", "codeql"),
  runner: CommandRunner = CommandRunner.system
):
  def createDatabase(
    sourceRoot: Path,
    database: Path,
    language: String,
    buildMode: String = "none",
    overwrite: Boolean = true
  ): Unit =
    val command = Seq(
      executable.toString,
      "database",
      "create",
      database.toString,
      s"--language=$language",
      s"--source-root=${sourceRoot.toAbsolutePath}",
      s"--build-mode=$buildMode"
    ) ++ (if overwrite then Seq("--overwrite") else Seq.empty)
    runner.run(command)

  def exportRelation(
    database: Path,
    workDirectory: Path,
    compiled: CompiledCodeQlUnit,
    exportQuery: EdbExportQuery
  ): Relation =
    val predicate = compiled.externalPredicate(exportQuery.relation).getOrElse {
      throw IllegalArgumentException(s"${exportQuery.relation} is not an external predicate in the compiled CodeQL program")
    }
    Files.createDirectories(workDirectory)
    val baseName = sanitize(exportQuery.relation.name)
    val queryFile = workDirectory.resolve(s"$baseName.ql")
    val bqrsFile = workDirectory.resolve(s"$baseName.bqrs")
    val csvFile = workDirectory.resolve(s"$baseName.csv")
    Files.writeString(queryFile, exportQuery.qlSource, StandardCharsets.UTF_8)

    runner.run(Seq(
      executable.toString,
      "query",
      "run",
      s"--database=${database.toAbsolutePath}",
      s"--output=${bqrsFile.toAbsolutePath}",
      queryFile.toAbsolutePath.toString
    ))
    runner.run(Seq(
      executable.toString,
      "bqrs",
      "decode",
      "--format=csv",
      "--no-titles",
      "--entities=id",
      s"--output=${csvFile.toAbsolutePath}",
      bqrsFile.toAbsolutePath.toString
    ))

    val rows = Csv.parse(Files.readString(csvFile, StandardCharsets.UTF_8))
    val types = predicate.outputTypes
    rows.foreach { row =>
      if row.size != types.size then
        throw IllegalArgumentException(
          s"EDB export ${exportQuery.relation} produced ${row.size} columns, expected ${types.size}: $row"
        )
    }
    val values = rows.map(row => row.zip(types).map(parseValue))
    ExecutionRelation.from(exportQuery.relation.name, parameterNames(predicate), values)

  private def parameterNames(predicate: PredicateDecl): Seq[String] =
    predicate.params.map(_.name.name) ++ predicate.resultType.toSeq.map(_ => "result")

  private def parseValue(valueAndType: (String, QlType)): Any = valueAndType match
    case (value, QlType.IntType) => value.trim.toInt
    case (value, QlType.FloatType) => value.trim.toDouble
    case (value, QlType.BooleanType) => if value.trim.toBoolean then 1 else 0
    case (value, _) => value

  private def sanitize(name: String): String = name.map {
    case c if c.isLetterOrDigit || c == '_' => c
    case _ => '_'
  }

object Csv:
  def parse(source: String): Seq[Seq[String]] =
    val rows = Seq.newBuilder[Seq[String]]
    val row = Seq.newBuilder[String]
    val field = StringBuilder()
    var index = 0
    var quoted = false

    def finishField(): Unit =
      row += field.result()
      field.clear()

    def finishRow(): Unit =
      finishField()
      rows += row.result()
      row.clear()

    while index < source.length do
      source(index) match
        case '"' if quoted && index + 1 < source.length && source(index + 1) == '"' =>
          field += '"'
          index += 1
        case '"' => quoted = !quoted
        case ',' if !quoted => finishField()
        case '\n' if !quoted => finishRow()
        case '\r' if !quoted =>
          if index + 1 >= source.length || source(index + 1) != '\n' then finishRow()
        case char => field += char
      index += 1

    if quoted then throw IllegalArgumentException("Unterminated quoted CSV field")
    if field.nonEmpty || source.nonEmpty && !source.endsWith("\n") && !source.endsWith("\r") then finishRow()
    rows.result().filterNot(row => row.size == 1 && row.head.isEmpty)
