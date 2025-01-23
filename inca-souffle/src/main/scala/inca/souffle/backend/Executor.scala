package inca.souffle.backend

import inca.ir.execution.ThreadCount.Auto
import inca.ir.execution.{ExecutorEngine, IRExecutor, Relation, RelationUpdateListener, ThreadCount, transformEDBInput}
import inca.ir.{CompiledUnit, string2name}
import inca.souffle.syntax.{Attribute, DirectiveQualifier, ProgramContent, QualifiedName, Type}
import inca.util.FileUtil

import java.io.File
import scala.sys.process.*
import scala.util.{Failure, Success, Try}
import ujson.*

import java.nio.file.{Files, Paths}

class SouffleLogger extends ProcessLogger {
  enum MessageType:
    case Warning
    case Error
    case None

  var warnings: Seq[String] = Seq()
  var errors: Seq[String] = Seq()

  var msgType: MessageType = MessageType.None

  def clear(): Unit =
    warnings = Seq()
    errors = Seq()

  override def out(s: => String): Unit = {
    // nothing, ignore stdout
  }

  def buffer[T](f: => T): T = f

  def err(msg: => String): Unit =
    if msg.startsWith("Error") then
      msgType = MessageType.Error
      errors :+= ""
    else if msg.startsWith("Warning:") then
      msgType = MessageType.Warning
      warnings :+= ""

    msgType match
      case MessageType.Error =>
        errors = errors.dropRight(1) :+ errors.last + "\n" + msg
      case MessageType.Warning =>
        warnings = warnings.dropRight(1) :+ warnings.last + "\n" + msg
      case _ => // nothing
}

final case class SouffleCompileException(errList: Seq[String]) extends Exception(errList.mkString("\n"))

// TODO we assume that directives use defaults
// inputs are in <name>.facts of directory
// tab is default delimiter
// outputs are in <name>.csv of directory
// tab is default delimiter
class Executor(numThreads: ThreadCount = Auto) extends IRExecutor:

  case class ProgramConfig(dirFile: File, progFile: File, flags: Map[String, String]):
    val dirFilePath: String = dirFile.getAbsolutePath
    val progFilePath: String = progFile.getAbsolutePath
    val profileFilePath: String = s"$dirFilePath/profile.log"

    def flagsToString(fls: Map[String, String]): String = fls.map((k, v) => s"-$k $v").mkString(" ")

    lazy val process: ProcessBuilder =
      val fls = flagsToString(flags)
      Process(s"souffle $fls --fact-dir=$dirFilePath/ --output-dir=$dirFilePath/ $progFilePath")

    lazy val showRamProcess: ProcessBuilder =
      val fls = flagsToString(flags)
      Process(s"souffle $fls $progFilePath --show=transformed-ram")

    lazy val profilingProcess: ProcessBuilder =
      val fls = flagsToString(flags + ("p" -> profileFilePath))
      Process(s"souffle $fls --fact-dir=$dirFilePath/ --output-dir=$dirFilePath/ $progFilePath")


  class Engine(config: ProgramConfig, inputFiles: Map[String, ProgramContent.Directive], outputFiles: Map[String, ProgramContent.Directive], relationDecl: Map[String, ProgramContent.RelationDecl]) extends ExecutorEngine:
    private var inputDirty = true
    private var cachedResult: Option[Seq[Relation]] = None

    val logger = SouffleLogger()

    private def stopOnError(): Unit =
      if logger.errors.nonEmpty then
        throw SouffleCompileException(logger.errors)

    private def execute(): Unit =
      logger.clear()
      if (inputDirty)
        config.process.!(logger)
        stopOnError()
        inputDirty = false

    def transformedRam(): String =
      logger.clear()
      val ramCode = config.showRamProcess.!!(logger)
      stopOnError()
      ramCode

    // This method measures the pure execution time without any disk I/O.
    override def measure(rel: Relation): Long =
      logger.clear()
      // Read runtime information from a profiling run
      config.profilingProcess.!!(logger)
      stopOnError()

      val profileJson = FileUtil.readFile(config.profileFilePath)

      // Get runtime (including savetimes)
      val res = ujson.read(profileJson)
      val program = res.obj("root").obj("program")
      val runtime = program.obj("runtime")
      val startTimeInUs = runtime.obj("start").num
      val endTimeInUs = runtime.obj("end").num
      val runtimeInUs = (endTimeInUs - startTimeInUs).toLong

      // Get savetime aka time spend for disk I/O
      val savetimeInUs = program.obj("relation").obj.map {
        // We include the save time for the measured relation
        // case (relName, _) if relName == GenerateSouffle.cleanName(rel.name) => 0
        // Only relations with an .output directive have a savetime
        case (relName, relObj) if relObj.obj.contains("savetime") =>
          val startTimeInUs = relObj.obj("savetime").obj("start").num
          val endTimeInUs = relObj.obj("savetime").obj("end").num
          (endTimeInUs - startTimeInUs).toLong
        case _ => 0
      }.sum

      (runtimeInUs - savetimeInUs) * 1000

    // Create empty input files for all input relations
    // This is necessary if a module has more than one main function
    inputFiles.foreach((_, d: ProgramContent.Directive) => FileUtil.writeFile(getPath(d), ""))

    def read(rel: Relation): Relation =
      readAll().find(_.name == GenerateSouffle.cleanName(rel.name)) match
        case Some(r) if rel.isEmpty => r
        case Some(r) =>
          // filter the result based on the input query
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

    def readAll(): Seq[Relation] = cachedResult match
      case Some(result) if !inputDirty => result
      case _ =>
        execute()
        val result = outputFiles.map { case (relName, file) =>
          val directive = outputFiles(relName)
          val file = outputFiles(relName)
          if (Files.exists(Paths.get(getPath(file))))
            val content = FileUtil.readFile(getPath(file))
            stringToRel(content, directive)
          else
            // TODO: Technically we should get the param from somewhere and list them here
            Relation.from(relName, Seq(), Seq())
        }.toSeq
        cachedResult = Some(result)
        result

    def insert(edb: Relation): Unit =
      inputDirty = true
      val directive = inputFiles(GenerateSouffle.cleanName(edb.name))
      val file = getPath(directive)
      val content = relToString(edb, directive)
      FileUtil.writeFile(file, content)

    override def remove(edb: Relation): Unit = throw new UnsupportedOperationException()

    override def addUpdateListener(up: RelationUpdateListener): Unit = throw new UnsupportedOperationException()

    override def removeUpdateListener(up: RelationUpdateListener): Unit = throw new UnsupportedOperationException()

    private def relToString(edb: Relation, directive: ProgramContent.Directive): String =
      val delimiter = getSeperator(directive)
      val tupleStrs = edb.entries.map { t =>
        val entries = edb.flattenEntry(t)
        entries.map(souffleifyTupleEntry).mkString(delimiter)
      }
      tupleStrs.mkString("\n")

    private def souffleifyTupleEntry(v: Any): Any =
      transformEDBInput(v)(identity, _.toString, _.toString, transformADT)

    private def transformADT(dataName: String, caseName: String, args: Seq[Any]): Any =
      if (args.nonEmpty)
        s"""$$${GenerateSouffle.cleanName(caseName)}${args.mkString("(", ",", ")")}"""
      else
        s"""$$${GenerateSouffle.cleanName(caseName)}"""

    private def cast(el: String, attr: Attribute): Any = attr match
      case _ if el.isEmpty => null
      case Attribute(_, Type.Symbol) => el
      case Attribute(_, Type.Number | Type.Unsigned) => Try(el.toInt) match
        case Success(d) => d
        case Failure(_) => throw IllegalArgumentException(s"Argument $el can not be interpreted as int")
      case Attribute(_, Type.Float) => Try(el.toFloat) match
        case Success(d) => d
        case Failure(_) => throw IllegalArgumentException(s"Argument $el can not be interpreted as float")
      // TODO: handle DataTypes
      case Attribute(_, Type.Name(qualName)) => el

    private def stringToRel(content: String, directive: ProgramContent.Directive): Relation =
      val delimiter = getSeperator(directive)
      val lines = content.split("\n")
      val attrs = lines.head.split(delimiter).toSeq
      val size = attrs.size

      val relName = directive.names.head.toString
      val relation = relationDecl(relName)

      val tuples = lines.map { t =>
        val elements = t.split(delimiter)
        elements.toSeq.zip(relation.attrs).map { (el, attr) =>
          cast(el, attr)
        }
      }.toList
      val params = (0 until size).map(idx => s"param_$idx")
      Relation.from(directive.names.head.toString, params, tuples)

    private def getPath(dir: ProgramContent.Directive): String =
      dir.dirQualifier match
        case DirectiveQualifier.Input => s"${config.dirFilePath}/${dir.names.head}.facts"
        case DirectiveQualifier.Output => s"${config.dirFilePath}/${dir.names.head}.csv"

    private def getSeperator(dir: ProgramContent.Directive): String = dir.dirQualifier match
      case DirectiveQualifier.Input => "\t"
      case DirectiveQualifier.Output => "\t"

  override def instantiate(m: CompiledUnit): Engine =
    // write Souffle program to file
    val souffleProgFile = File.createTempFile(m.name.name + "_syntax", ".dl")

    val Seq(lowered) = m.compiled
    val souffleProg = GenerateSouffle.compileModule(lowered)

    //println(m.lowered)
    //println()

    //println(souffleProg.toString)
    //println()

    FileUtil.writeFile(souffleProgFile, souffleProg.toString)
    val dirFile = souffleProgFile.getParentFile

    // Souffle 2.4.1 crashes when it automatically guesses the thread count
    val flags = numThreads match
      case ThreadCount.Auto => Map("j" -> ThreadCount.numberOfAvailableThreads().toString)
      case ThreadCount.Fixed(n) if n > 1 => Map("j" -> n.toString)
      case _ => Map()

    val programConfig = ProgramConfig(dirFile, souffleProgFile, flags)

    // collect input and output directives
    val inputFiles = souffleProg.content.flatMap {
      case d@ProgramContent.Directive(DirectiveQualifier.Input, names, _) => names.map { n => n.toString -> d }
      case _ => Seq()
    }.toMap

    val outputFiles = souffleProg.content.flatMap {
      case d@ProgramContent.Directive(DirectiveQualifier.Output, names, _) => names.map { n => n.toString -> d }
      case _ => Seq()
    }.toMap

    val relationDecl = souffleProg.content.flatMap {
      case d@ProgramContent.RelationDecl(name, _, _, _) => name.map(_ -> d)
      case _ => None
    }.toMap
    new Engine(programConfig, inputFiles, outputFiles, relationDecl)


