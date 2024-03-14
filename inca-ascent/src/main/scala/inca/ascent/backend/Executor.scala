package inca.ascent.backend

import inca.ascent.backend.GenerateAscent.cleanName
import inca.ascent.syntax.*
import inca.ir
import inca.ir.execution.{ExecutorEngine, IRExecutor, Relation}
import inca.ir.extension.data.Construct
import inca.ir.{CompiledModule, Name}

import java.io.{File, PrintWriter}
import scala.io.Source
import scala.sys.process.*
import scala.util.{Failure, Success, Try}

object Executor extends IRExecutor:
  class Engine(rustFilePath: String, executable: ProcessBuilder, contents: Seq[ProgramContent], outputs: Seq[ProgramContent.RelDecl]) extends ExecutorEngine:
    var inputDirty = true
    var inputs: Seq[ProgramContent] = Seq()
    var cachedResult: Option[Seq[Relation]] = None

    private def execute(): String = {
      val progString = createRustString(contents, inputs)
      createRustFile(progString, rustFilePath)
      executable.!!
    }

    def insert(edb: Relation): Unit = {
      inputDirty = true
      val content = relToFact(edb)
      inputs = inputs ++ content
    }

    def addUpdateListener(up: inca.ir.execution.RelationUpdateListener): Unit = ???

    def remove(edb: inca.ir.execution.Relation): Unit = ???

    def removeUpdateListener(up: inca.ir.execution.RelationUpdateListener): Unit = ???

    def readAll(): Seq[Relation] = cachedResult match {
      case Some(result) if !inputDirty => result
      case _ =>
        val output = execute()
        val result = OutputParser.parse(output)
        cachedResult = Some(result)
        inputDirty = false
        result
    }

    def read(rel: Relation): Relation = {
      val facts = readAll().find(_.name == cleanName(Name(rel.name))) match {
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

    def createRustString(contents: Seq[ProgramContent], inputFacts: Seq[ProgramContent]): String =
      Program(contents ++ inputFacts, outputs).toString()

  private def createRustFile(s: String, filepath: String): Unit = {
    val newPath = filepath + "/src/main.rs"
    val newFile = new File(newPath)
    newFile.getParentFile.mkdirs()
    val pw = new PrintWriter(newFile)
    pw.write(s)
    pw.close()
  }

  def instantiate(m: CompiledModule): Engine = {
    var currentDir = new File("./").getCanonicalFile

    // we might be in a subproject when running with sbt
    while (currentDir.getName != "inca-scala") {
      currentDir = currentDir.getParentFile
    }
    val projectDir = currentDir.getCanonicalPath + "/inca-ascent"
    val rustProjectDir = projectDir + "/ascent_project"
    val contents = (new GenerateAscent).compileModule(m.lowered)

    val process = stringToProcess(s"cargo run --manifest-path $rustProjectDir/Cargo.toml")

    // all outputs
    val outputs = contents.collect {
      case relDecl@ProgramContent.RelDecl(k, v) => relDecl
    }
    new Engine(rustProjectDir, process, contents, outputs)
  }

  private def relToFact(edb: Relation): Seq[ProgramContent] = {
    val name = cleanName(Name(edb.name))
    edb.entries.map { t =>
      val entry = edb.flattenEntry(t)
      val e = entry.map(p => ascentifyTupleEntry(p))
      ProgramContent.Fact(name, e)
    }.toSeq
  }

  private def ascentifyTupleEntry(s: Any): Term = s match {
    case i: Int => Term.NumberLit(i)
    case s: String => Term.StringLit(s)
    case d: Double => Term.FloatLit(d.toFloat)
    case s => throw IllegalArgumentException(s"Do not support $s which is of type ${s.getClass} as input")
  }
