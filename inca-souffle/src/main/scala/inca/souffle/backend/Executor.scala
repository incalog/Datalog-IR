package inca.souffle.backend

import inca.ir.execution.{ExecutorEngine, IRExecutor, Relation, RelationUpdateListener}
import inca.ir.{CompiledModule, string2name}
import inca.souffle.syntax.{Attribute, DirectiveQualifier, ProgramContent, QualifiedName, Type}
import inca.util.FileUtil

import java.io.File
import scala.sys.process.*
import scala.util.{Failure, Success, Try}

// TODO we assume that directives use defaults
// inputs are in <name>.facts of directory
// tab is default delimiter
// outputs are in <name>.csv of directory
// tab is default delimiter
object Executor extends IRExecutor:

  class Engine(dirFile: File, executable: ProcessBuilder, inputFiles: Map[String, ProgramContent.Directive], outputFiles: Map[String, ProgramContent.Directive], relationDecl: Map[String, ProgramContent.RelationDecl]) extends ExecutorEngine:
    private var inputDirty = false
    private var cachedResult: Option[Seq[Relation]] = None

    private def execute(): Unit =
      // if (inputDirty)
        executable.!

    // Create empty input files for all input relations
    // This is necessary if a module has more than one main function
    inputFiles.foreach((_, d: ProgramContent.Directive) => FileUtil.writeFile(getPath(d), ""))

    // TODO: Support joins in relations
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
          val content = FileUtil.readFile(getPath(file))
          stringToRel(content, directive)
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

    // we have strings, arithmetic and data as primitives
    // TODO support data
    private def souffleifyTupleEntry(s: Any): String = s match
      case i: Int => i.toString
      case s: String => s.toString
      case s => throw IllegalArgumentException(s"Do not support $s which is of type ${s.getClass} as input")

    private def cast(el: String, attr: Attribute): Any = attr match
      case _ if el.isEmpty => null
      case Attribute(_, Type.Symbol) => el
      case Attribute(_, Type.Number | Type.Unsigned) => Try(el.toInt) match
        case Success(d) => d
        case Failure(_) => throw IllegalArgumentException(s"Argument $el can not be interpreted as int")
      case Attribute(_, Type.Float) => Try (el.toFloat) match
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
      val params = (0 until size).map(idx => s"param_${idx}")
      Relation.from(directive.names.head.toString, params, tuples)

    // TODO we just use the defaults currently
    private def getPath(dir: ProgramContent.Directive): String =
      dir.dirQualifier match
        case DirectiveQualifier.Input => s"${dirFile.getAbsolutePath}/${dir.names.head}.facts"
        case DirectiveQualifier.Output => s"${dirFile.getAbsolutePath}/${dir.names.head}.csv"
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
    val inputFiles = souffleProg.content.flatMap {
      case d@ProgramContent.Directive(DirectiveQualifier.Input, names, _) => names.map { n => n.toString -> d }
      case _ => Seq()
    }.toMap

    val outputFiles = souffleProg.content.flatMap {
      case d@ProgramContent.Directive(DirectiveQualifier.Output, names, _) => names.map { n => n.toString -> d }
      case _ => Seq()
    }.toMap

    println(outputFiles)

    val relationDecl = souffleProg.content.flatMap {
      case d@ProgramContent.RelationDecl(name, _, _, _) => name.map(_ -> d)
      case _ => None
    }.toMap
    new Engine(dirFile, process, inputFiles, outputFiles, relationDecl)


