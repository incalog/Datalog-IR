package inca.ascent.backend

import inca.ascent.backend.GenerateAscent.cleanName
import inca.ascent.syntax.*
import inca.ir
import inca.ir.execution.{ExecutorEngine, IRExecutor, Relation}
import inca.ir.extension.data.Construct
import inca.ir.{CompiledModule, Name}
import inca.util.FileUtil
import ujson.{Arr, Bool, Null, Num, Obj, Str}

import java.io.{File, PrintWriter}
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import scala.io.Source
import scala.sys.process.*
import scala.util.{Failure, Success, Try}

object Executor extends IRExecutor:
  class Engine(rustFilePath: String, executable: ProcessBuilder, contents: Seq[ProgramContent], outputs: Seq[ProgramContent.RelDecl]) extends ExecutorEngine:
    var inputDirty = true
    //var inputs: Seq[ProgramContent] = Seq()
    var inputs: Map[String, ProgramContent.EDBFile] = Map()
    var cachedResult: Option[Seq[Relation]] = None

    private def execute(): String = {
      //val progString = createRustString(contents, inputs)
      val progString = createRustString(contents)
      createRustFile(progString, rustFilePath)
      executable.!!
    }

    def insert(edb: Relation): Unit = {
      inputDirty = true
      //val content = relToFact(edb)
      //inputs = inputs ++ content

      val content = edb.entries
        .map(t => edb.flattenEntry(t).mkString("\t")).mkString("\n")
      val name = cleanName(Name(edb.name))
      val edbFile = Files.createTempFile(name, ".facts")
      Files.write(edbFile, content.getBytes(StandardCharsets.UTF_8))
      //edbFile.toFile.deleteOnExit()
      inputs += name -> ProgramContent.EDBFile(name, edbFile.toAbsolutePath.toString)
    }

    def addUpdateListener(up: inca.ir.execution.RelationUpdateListener): Unit = ???

    def remove(edb: inca.ir.execution.Relation): Unit =
      inputDirty = true
      val name = cleanName(Name(edb.name))
      inputs -= name

    def removeUpdateListener(up: inca.ir.execution.RelationUpdateListener): Unit = ???

    // Generate a string representation of objects
    private def valuefyObject(v: ujson.Value): Any = v match
      case obj@Obj(kvs) =>
        val sb = new StringBuilder
        kvs.foreach { case (k, v) =>
          val argS = v match
            case _: Arr => valuefyObject(v)
            case _ => "(" + valuefyObject(v) + ")"
          sb.append(s"$k$argS")
        }
        sb.toString
      case a : Arr => v.arr.map(valuefyObject).mkString("(", ",", ")")
      case i: Num if i.value.toInt == i.value => i.value.toInt
      case _ => v.value

    def readAll(): Seq[Relation] = cachedResult match {
      case Some(result) if !inputDirty => result
      case _ =>
        val jsonOutput = execute()
        val result = jsonOutput.split("\n").map { line =>
          val res = ujson.read(line)
          val rel = res.obj("name").str
          val size = res.obj("size").num.toInt
          val elements = res.obj("elements").arr.map(_.arr.map {
            case obj : ujson.Obj => valuefyObject(obj)
            case j => j.value
          }.toSeq)
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

    def createRustString(contents: Seq[ProgramContent]): String =
      Program(contents ++ inputs.values, outputs).toString()

    //def createRustString(contents: Seq[ProgramContent], inputFacts: Seq[ProgramContent]): String =
    //  Program(contents ++ inputFacts, outputs).toString()

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

    val process = stringToProcess(s"cargo run --manifest-path $rustProjectDir/Cargo.toml --release")

    // all outputs
    val outputs = contents.collect {
      case relDecl@ProgramContent.RelDecl(k, v, false) => relDecl
    }
    new Engine(rustProjectDir, process, contents, outputs)
  }

  /*private def relToFact(edb: Relation): Seq[ProgramContent] = {
    val name = cleanName(Name(edb.name))
    edb.entries.map { t =>
      val entry = edb.flattenEntry(t)
      val e = entry.map(p => ascentifyTupleEntry(p))
      ProgramContent.Fact(name, e)
    }.toSeq
  }*/

  private def ascentifyTupleEntry(s: Any): Term = s match {
    case i: Int => Term.NumberLit(i)
    case s: String => Term.StringLit(s)
    case d: Double => Term.FloatLit(d.toFloat)
    case s => throw IllegalArgumentException(s"Do not support $s which is of type ${s.getClass} as input")
  }
