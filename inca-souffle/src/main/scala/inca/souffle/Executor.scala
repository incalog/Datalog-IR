package inca.souffle

import inca.ir.CompiledModule
import inca.ir.execution.{ExecutorEngine, IRExecutor, Relation}
import inca.souffle.compile.GenerateSouffle
import inca.souffle.syntax.{DirectiveQualifier, ProgramContent}
import inca.util.FileUtil
import inca.ir.string2name

import scala.util.{Failure, Success, Try}
import java.io.File
import scala.sys.process.*

// TODO we assume that directives use defaults
// inputs are in <name>.facts of directory
// tab is default delimiter
// outputs are in <name>.csv of directory
// tab is default delimiter
object Executor extends IRExecutor:

  class Engine(dirFile: File, executable: ProcessBuilder, inputFiles: Map[String, ProgramContent.Directive], outputFiles: Map[String, ProgramContent.Directive]) extends ExecutorEngine:
    private var inputDirty = false

    private def execute(): Unit =
      // if (inputDirty)
        executable.!

    // Create empty input files for all input relations
    // This is necessary if a module has more than one main function
    inputFiles.foreach((_, d: ProgramContent.Directive) => FileUtil.writeFile(getPath(d), ""))

    // TODO relations do not support joins currently
    def read(rel: Relation): Relation =
      readAll().find(_.name == rel.name) match
        case Some(r) => r
        case _ => throw IllegalStateException(s"No relation named ${rel.name} found")

    def readAll(): Seq[Relation] =
      execute()
      outputFiles.map { case (relName, file) =>
        val directive = outputFiles(relName)
        val file = outputFiles(relName)
        val content = FileUtil.readFile(getPath(file))
        stringToRel(content, directive)
      }.toSeq


    def insert(edb: Relation): Unit =
      inputDirty = true
      val directive = inputFiles(GenerateSouffle.cleanName(edb.name))
      val file = getPath(directive)
      val content = relToString(edb, directive)
      FileUtil.writeFile(file, content)

    private def relToString(edb: Relation, directive: ProgramContent.Directive): String =
      val delimiter = getSeperator(directive)
      val tupleStrs = edb.entries.map { t =>
        val entries = edb.flattenEntry(t)
        entries.map(souffleifyTupleEntry).mkString(delimiter)
      }
      tupleStrs.mkString("\n")

    // we have strings, arithmetic and data as primitives
    // TODO support data
    private def souffleifyTupleEntry(s: Any): String = s match
      case i: Int => i.toString
      case s: String => s.toString
      case s => throw IllegalArgumentException(s"Do not support $s which is of type ${s.getClass} as input")

    private def stringToRel(content: String, directive: ProgramContent.Directive): Relation =
      val delimiter = getSeperator(directive)
      val lines = content.split("\n")
      val size = lines.head.split(delimiter).length
      val tuples = lines.map { t =>
        val elements = t.split(delimiter)
        elements.toSeq.map { el =>
          Try(el.toDouble) match
            case Success(d) => d
            case Failure(_) => el
        }
      }.toList
      val params = (0 until size).map(idx => s"param_${idx}")
      Relation.from(directive.name.toString, params, tuples)

    // TODO we just use the defaults currently
    private def getPath(dir: ProgramContent.Directive): String =
      dir.dirQualifier match
        case DirectiveQualifier.Input => s"${dirFile.getAbsolutePath}/${dir.name}.facts"
        case DirectiveQualifier.Output => s"${dirFile.getAbsolutePath}/${dir.name}.csv"
    private def getSeperator(dir: ProgramContent.Directive): String = dir.dirQualifier match
      case DirectiveQualifier.Input => "\t"
      case DirectiveQualifier.Output => "\t"


  override def instantiate(m: CompiledModule): Engine =
    // write Souffle program to file
    val souffleProgFile = File.createTempFile(m.name.name + "_syntax", ".dl")
    val souffleProg = GenerateSouffle.compileModule(m.lowered)
    FileUtil.writeFile(souffleProgFile, souffleProg.toString)
    val dirFile = souffleProgFile.getParentFile
    // create process
    val process = Process(s"souffle --fact-dir=${dirFile.getAbsolutePath}/ --output-dir=${dirFile.getAbsolutePath}/ ${souffleProgFile.getAbsolutePath}")
    // collect input and output directives
    val inputFiles = souffleProg.content.collect {
      case d@ProgramContent.Directive(DirectiveQualifier.Input, name, _) => name.toString -> d
    }.toMap

    val outputFiles = souffleProg.content.collect {
      case d@ProgramContent.Directive(DirectiveQualifier.Output, name, _) => name.toString -> d
    }.toMap
    new Engine(dirFile, process, inputFiles, outputFiles)


