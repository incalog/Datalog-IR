package inca.ascent.backend

import inca.ascent.backend.GenerateAscent.cleanName
import inca.ascent.syntax.*
import inca.ir
import inca.ir.execution.ThreadCount.Auto
import inca.ir.execution.{ExecutorEngine, IRExecutor, Relation, ThreadCount}
import inca.ir.{CompiledUnit, Name}
import inca.util.FileUtil
import ujson._

import java.io.{File, PrintWriter}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import scala.sys.process.*
import scala.sys.process.ProcessBuilder
import scala.language.implicitConversions
import scala.jdk.OptionConverters._

object Executor:
  private lazy val ascentProjectPath = Files.createTempDirectory("ascent-project")

class Executor(numThreads: ThreadCount = Auto) extends IRExecutor:
  class Engine(executable: ProcessBuilder, inputs: Map[String, ProgramContent.EDBFile]) extends ExecutorEngine:
    var inputDirty = true
    var cachedResult: Option[Seq[Relation]] = None

    private def execute(): String = executable.!!

    override def measure(rel: Relation): Long =
      executable.!!.lines().findFirst().toScala match
        case Some(l) => l.toLong
        case _ => throw IllegalStateException("Could not read execution time!")

    def insert(edb: Relation): Unit = {
      inputDirty = true

      val content = edb.entries.map(t => edb.flattenEntry(t).mkString("\t")).mkString("\n")
      val name = cleanName(Name(edb.name))

      // Reuse existing file if it exists, so we don't have to recompile the rust project
      val edbFile = inputs.get(name) match
        case Some(ProgramContent.EDBFile(name, path)) => Path.of(path)
        case None => throw IllegalStateException(s"Edb input $name not found in inputs map")

      Files.write(edbFile, content.getBytes(StandardCharsets.UTF_8))
      edbFile.toFile.deleteOnExit()
    }

    def remove(edb: inca.ir.execution.Relation): Unit = throw new UnsupportedOperationException()

    def addUpdateListener(up: inca.ir.execution.RelationUpdateListener): Unit = throw new UnsupportedOperationException()

    def removeUpdateListener(up: inca.ir.execution.RelationUpdateListener): Unit = throw new UnsupportedOperationException()

    def readAll(): Seq[Relation] = cachedResult match {
      case Some(result) if !inputDirty => result
      case _ =>
        // Skip the first line, which includes the execution time
        val jsonOutputs = execute().split("\n").tail.toSeq
        val result = jsonOutputs.map { line =>
          val res = ujson.read(line)
          val rel = res.obj("name").str
          val size = res.obj("size").num.toInt
          val elements = res.obj("elements").arr.map(_.arr.toSeq.map(_.value))
          Relation.from(rel, 0.until(size).map(i => s"Param$i"), elements)
        }
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

  private def createRustFile(s: String, filepath: String): Unit = {
    val newPath = filepath + "/src/main.rs"
    val newFile = new File(newPath)
    newFile.getParentFile.mkdirs()
    val pw = new PrintWriter(newFile)
    pw.write(s)
    pw.close()
  }

  // Rust compilation is slow, therefore we compile once on instantiate
  def instantiate(m: CompiledUnit): Engine = {
    // Create project structure
    val rustProjectDir = Executor.ascentProjectPath.toFile.getCanonicalPath
    Files.createDirectories(Paths.get(rustProjectDir, "src"))
    val cargoFile = FileUtil.readFileFromResource("Cargo.toml")
    val cargoFilePath = Paths.get(rustProjectDir, "Cargo.toml")
    FileUtil.writeFile(cargoFilePath.toFile.getCanonicalPath, cargoFile)

    val Seq(lowered) = m.lowered
    val contents = GenerateAscent.compileModule(lowered)

    // all inputs and outputs
    val (inputs, outputs) = contents.collect {
      case relDecl@ProgramContent.RelDecl(k, v, _) => relDecl
    }.partition(_.isEdb)

    // create edb inputs
    val fileInputs: Seq[ProgramContent.EDBFile] = inputs.map { i =>
      val name = cleanName(Name(i.name))
      val edbFile = Files.createTempFile(name, ".facts")
      ProgramContent.EDBFile(name, edbFile.toAbsolutePath.toString)
    }

    // create the rust program file
    val parallel = numThreads.requiresParallelExec
    val progString = Program(contents ++ fileInputs, outputs, parallel).toString()
    createRustFile(progString, rustProjectDir)

    // build the rust project
    val buildProcess = stringToProcess(s"cargo build --manifest-path $rustProjectDir/Cargo.toml --release")
    buildProcess.! match {
      case 0 => // ok
      case _ => throw IllegalStateException("Failed to build rust project")
    }

    // create the engine
    val env = numThreads match
      case ThreadCount.Auto => Seq()
      case ThreadCount.Fixed(n) if n > 1 => Seq("RAYON_NUM_THREADS" -> n.toString)
      case _ => Seq()
    val execProcess = Process(s"$rustProjectDir/target/release/ascent_project", None, env: _*)
    new Engine(execProcess, fileInputs.map(i => (i.name, i)).toMap)
  }
