package inca.codeql.executor

import java.io._
import java.nio.charset.StandardCharsets
import java.nio.file._
import scala.collection.mutable
import scala.jdk.CollectionConverters._
import scala.util.matching.Regex

object CodeQlJavaEdbDumper:

  final case class CmdResult(exitCode: Int, stdout: String, stderr: String)
  final case class Column(storageType: String, qlType: String)
  final case class RelationSchema(name: String, columns: Vector[Column])

  /** Runs CodeQL on the given Java source string and dumps the requested raw EDB relations as CSV.
   *
   * The source is written to `GeneratedClass.java`, so if it contains a public top-level class,
   * that class should be named `GeneratedClass`.
   */
  def dump(
            javaCode: String,
            relationNames: Seq[String],
            keepTempDirectory: Boolean = false
          ): Map[String, String] =
    val workDir = Files.createTempDirectory("codeql-java-edb-")
    try
      val sourceFile = workDir.resolve("GeneratedClass.java")
      val databaseDir = workDir.resolve("database-test")

      Files.writeString(sourceFile, javaCode, StandardCharsets.UTF_8)

      runOrFail(
        Seq(
          "codeql",
          "database",
          "create",
          databaseDir.toString,
          "--no-cleanup",
          "--language=java",
          "--overwrite",
          "--command",
          "javac GeneratedClass.java"
        ),
        cwd = Some(workDir)
      )

      val dbscheme = locateJavaDbscheme(databaseDir)
      val schemas = parseDbscheme(dbscheme)

      relationNames.map { relationName =>
        val schema =
          schemas.getOrElse(
            relationName,
            throw RuntimeException(
              s"Relation '$relationName' not found in dbscheme: $dbscheme"
            )
          )

        relationName -> dumpSingleRelation(
          databaseDir = databaseDir,
          workDir = workDir,
          schema = schema
        )
      }.toMap
    finally
      if !keepTempDirectory then
        deleteRecursively(workDir)

  def dumpAndPrint(
                    javaCode: String,
                    relationNames: Seq[String],
                    keepTempDirectory: Boolean = false
                  ): Map[String, String] =
    val result = dump(javaCode, relationNames, keepTempDirectory)

    result.foreach { case (name, csv) =>
      println()
      println(s"===== $name =====")
      print(csv)
      if !csv.endsWith("\n") then println()
    }

    result

  private def dumpSingleRelation(
                                  databaseDir: Path,
                                  workDir: Path,
                                  schema: RelationSchema
                                ): String =
    val queryFile = workDir.resolve(s"dump_${schema.name}.ql")
    val queryText = generateQuery(schema)

    System.err.println()
    System.err.println(s"===== generated query for ${schema.name} =====")
    System.err.println(queryText)
    System.err.println("============================================")
    System.err.println()

    Files.writeString(queryFile, queryText, StandardCharsets.UTF_8)

    val bqrsFile = workDir.resolve(s"${schema.name}.bqrs")
    val csvFile = workDir.resolve(s"${schema.name}.csv")

    Files.writeString(queryFile, generateQuery(schema), StandardCharsets.UTF_8)

    runOrFail(
      Seq(
        "codeql",
        "query",
        "run",
        queryFile.toString,
        "--database",
        databaseDir.toString,
        "--output",
        bqrsFile.toString
      ),
      cwd = Some(workDir)
    )

    runOrFail(
      Seq(
        "codeql",
        "bqrs",
        "decode",
        bqrsFile.toString,
        "--format=csv",
        "--entities=id",
        "--output",
        csvFile.toString
      ),
      cwd = Some(workDir)
    )

    Files.readString(csvFile, StandardCharsets.UTF_8)

  private def generateQuery(schema: RelationSchema): String =
    def isEntityType(tpe: String): Boolean =
      tpe.startsWith("@")

    def wrapperName(tpe: String): String =
      "Raw_" + tpe.stripPrefix("@").replaceAll("[^A-Za-z0-9_]", "_")

    val entityTypes =
      schema.columns
        .map(_.qlType)
        .filter(isEntityType)
        .distinct

    val wrapperClasses =
      entityTypes
        .map { tpe =>
          s"class ${wrapperName(tpe)} extends $tpe { string toString() { result = \"instance\" } }"
        }
        .mkString("\n")

    val declarations =
      schema.columns.zipWithIndex
        .map { case (column, index) =>
          val qlVariableType =
            if isEntityType(column.qlType) then wrapperName(column.qlType)
            else column.qlType

          s"$qlVariableType c$index"
        }
        .mkString(",\n  ")

    val args =
      schema.columns.indices.map(i => s"c$i").mkString(", ")

    val selected =
      schema.columns.indices.map(i => s"c$i").mkString(", ")

    val prefix =
      if wrapperClasses.nonEmpty then wrapperClasses + "\n\n"
      else ""

    s"""|${prefix}from
        |  $declarations
        |where
        |  ${schema.name}($args)
        |select
        |  $selected
        |""".stripMargin


  private def parseDbscheme(dbscheme: Path): Map[String, RelationSchema] =
    val raw = Files.readString(dbscheme, StandardCharsets.UTF_8)

    val withoutBlockComments =
      raw.replaceAll("(?s)/\\*.*?\\*/", "")

    val withoutLineComments =
      withoutBlockComments.replaceAll("(?m)//.*$", "")

    val lines =
      withoutLineComments.linesIterator.toVector

    val result = scala.collection.mutable.Map.empty[String, RelationSchema]

    var i = 0

    val relationStart: Regex =
      """^\s*([A-Za-z_][A-Za-z0-9_]*)\s*\(\s*$""".r

    while i < lines.length do
      lines(i) match
        case relationStart(name) =>
          val bodyLines = scala.collection.mutable.ArrayBuffer.empty[String]

          i += 1

          var done = false
          while i < lines.length && !done do
            val line = lines(i)
            val trimmed = line.trim

            if trimmed == ")" || trimmed == ");" then
              done = true
            else
              bodyLines += line

            i += 1

          val body = bodyLines.mkString("\n").trim

          if body.nonEmpty then
            val columnTexts =
              splitTopLevelColumns(body)
                .map(_.trim)
                .filter(_.nonEmpty)
                .toVector

            val columns =
              columnTexts.map(parseColumn)

            result += name -> RelationSchema(name, columns)

        case _ =>
          i += 1

    result.toMap


  private def splitTopLevelColumns(body: String): Seq[String] =
    val columns = scala.collection.mutable.ArrayBuffer.empty[String]
    val current = new StringBuilder

    var i = 0
    while i < body.length do
      val ch = body.charAt(i)

      if ch == ',' then
        columns += current.toString()
        current.clear()
      else
        current.append(ch)

      i += 1

    val last = current.toString()
    if last.trim.nonEmpty then columns += last

    columns.toSeq


  private def parseColumn(text: String): Column =
    val cleaned =
    text
      .replaceAll("""\bunique\b""", "")
      .trim

    val columnRegex: Regex =
    """^(int|string|float|boolean|date)\s+[A-Za-z_][A-Za-z0-9_]*\s*:\s*(@?[A-Za-z_][A-Za-z0-9_]*)\s*(?:ref)?\s*$""".r

    cleaned match
      case columnRegex(storageType, qlType) =>
        Column(storageType, qlType)

      case _ =>
        throw RuntimeException(s"Could not parse dbscheme column: '$text'")

  private def locateJavaDbscheme(databaseDir: Path): Path =
    // Optional manual override.
    sys.env.get("CODEQL_JAVA_DBSCHEME") match
      case Some(path) if Files.isRegularFile(Paths.get(path)) =>
        return Paths.get(path)
      case _ =>

    // Try inside the created database.
    val dbschemesInDb =
      findFiles(databaseDir, p => p.getFileName.toString.endsWith(".dbscheme"))

    if dbschemesInDb.nonEmpty then
      return dbschemesInDb.head

    // Try CodeQL pack resolver.
    val packsJson =
      runOrFail(
        Seq("codeql", "resolve", "packs", "--format=json"),
        cwd = None
      ).stdout

    val pathRegex: Regex =
      """"path"\s*:\s*"([^"]+)"""".r

    val candidatePackDirs =
      pathRegex
        .findAllMatchIn(packsJson)
        .map(m => unescapeJsonString(m.group(1)))
        .map(Paths.get(_))
        .filter(Files.isDirectory(_))
        .toVector

    val candidates =
      candidatePackDirs.flatMap { packDir =>
        val qlpack = packDir.resolve("qlpack.yml")

        if !Files.isRegularFile(qlpack) then Vector.empty
        else
          val text = Files.readString(qlpack, StandardCharsets.UTF_8)

          val isJavaPack =
            text.contains("name: codeql/java") ||
              packDir.toString.replace('\\', '/').contains("/java/ql/lib")

          if !isJavaPack then Vector.empty
          else
            val dbschemeLine =
              text
                .linesIterator
                .map(_.trim)
                .find(_.startsWith("dbscheme:"))

            dbschemeLine match
              case None => Vector.empty
              case Some(line) =>
                val relative =
                  line
                    .stripPrefix("dbscheme:")
                    .trim
                    .stripPrefix("\"")
                    .stripSuffix("\"")
                    .stripPrefix("'")
                    .stripSuffix("'")

                val candidate = packDir.resolve(relative).normalize()

                if Files.isRegularFile(candidate) then Vector(candidate)
                else Vector.empty
      }

    candidates.headOption.getOrElse {
      throw RuntimeException(
        """Could not locate Java dbscheme.
          |
          |Set this environment variable explicitly:
          |
          |  CODEQL_JAVA_DBSCHEME=/path/to/codeql/java/ql/lib/config/semmlecode.dbscheme
          |""".stripMargin
      )
    }

  private def findFiles(root: Path, predicate: Path => Boolean): Vector[Path] =
    if !Files.exists(root) then Vector.empty
    else
      val stream = Files.walk(root)
      try stream.iterator().asScala.filter(predicate).toVector
      finally stream.close()

  private def runOrFail(cmd: Seq[String], cwd: Option[Path]): CmdResult =
    val result = run(cmd, cwd)

    if result.exitCode != 0 then
      throw RuntimeException(
        s"""Command failed.
           |
           |Exit code:
           |${result.exitCode}
           |
           |Command:
           |${cmd.mkString(" ")}
           |
           |stdout:
           |${result.stdout}
           |
           |stderr:
           |${result.stderr}
           |""".stripMargin
      )

    result

  private def run(cmd: Seq[String], cwd: Option[Path]): CmdResult =
    val pb = ProcessBuilder(cmd*)
    cwd.foreach(path => pb.directory(path.toFile))

    val process = pb.start()

    val stdout = readAsync(process.getInputStream)
    val stderr = readAsync(process.getErrorStream)

    val exitCode = process.waitFor()

    CmdResult(
      exitCode = exitCode,
      stdout = stdout(),
      stderr = stderr()
    )

  private def readAsync(input: InputStream): () => String =
    @volatile var result: String = ""

    val thread =
      Thread(() =>
        val baos = ByteArrayOutputStream()
        val buffer = Array.ofDim[Byte](8192)

        var read = input.read(buffer)
        while read != -1 do
          baos.write(buffer, 0, read)
          read = input.read(buffer)

        result = baos.toString(StandardCharsets.UTF_8)
      )

    thread.start()

    () =>
      thread.join()
      result

  private def deleteRecursively(path: Path): Unit =
    if Files.exists(path) then
      Files
        .walk(path)
        .iterator()
        .asScala
        .toSeq
        .reverse
        .foreach(p => Files.deleteIfExists(p))

  private def unescapeJsonString(s: String): String =
    s
      .replace("""\\""", "\\")
      .replace("""\"""", "\"")
      .replace("""\/""", "/")
      .replace("""\n""", "\n")
      .replace("""\r""", "\r")
      .replace("""\t""", "\t")