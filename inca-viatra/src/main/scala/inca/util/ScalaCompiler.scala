package inca.util

import dotty.tools.dotc
import dotty.tools.io.AbstractFile
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.Driver
import dotty.tools.dotc.util.SourceFile
import dotty.tools.io.{VirtualDirectory, VirtualFile}

import java.nio.charset.StandardCharsets
import dotty.tools.repl.AbstractFileClassLoader
import inca.util.compileroptions.CompilerOptions

import scala.io.Codec


// TODO: Neither the current implementation, nor the alternative in the comment work with sbt... why ?
/*
import dotty.tools.dotc.interfaces.{CompilerCallback, Diagnostic, SimpleReporter, SourceFile}

import java.nio.charset.StandardCharsets
import inca.util.compileroptions.CompilerOptions

import java.io.{File, PrintWriter}
import java.lang.Thread.sleep
import java.net.{URI, URL}
import java.util.UUID
import scala.collection.mutable.ListBuffer
import scala.io.Codec
import scala.reflect.internal.util.ScalaClassLoader.fromURLs
import scala.runtime.LazyRef

class ScalaCompiler(val options: CompilerOptions) {
  val viatraLogging = options("viatra_logging")
  val logPsystem = viatraLogging.readBoolean("psystem")

  private var compilerCache: Map[String, Any] = Map()

  private class CustomSimpleReporter extends SimpleReporter {
    var errorCount = 0
    var warningCount = 0

    def report(diag: Diagnostic): Unit = {
      if (diag.level == Diagnostic.ERROR)
        errorCount += 1
      if (diag.level == Diagnostic.WARNING)
        warningCount += 1

      println(diag.message)
    }
  }

  private class CustomCompilerCallback extends CompilerCallback {
    private val pathsBuffer = new ListBuffer[String]
    def paths = pathsBuffer.toList

    override def onSourceCompiled(source: SourceFile): Unit = {
      if (source.jfile.isPresent)
        pathsBuffer += source.jfile.get.getPath
    }
  }

  private def createTempFile(content: String): File =
    val tmpFile = File.createTempFile("VIATRA-", "")
    tmpFile.deleteOnExit()
    new PrintWriter(tmpFile) {
      try {
        write(content)
      } finally {
        close()
      }
    }
    tmpFile


  private def compileCode(code: String): String = {
    val inputFile = createTempFile(code)
    val outputDir = new File(inputFile.getParent + "/" + UUID.randomUUID().toString)
    outputDir.mkdir()

    val args = inputFile.getAbsolutePath +: List(
      "-d",
      outputDir.getAbsolutePath,
      "-classpath", "", // Avoid the default "."
      "-usejavacp",
    )

    val mainClass = Class.forName("dotty.tools.dotc.Main")
    val process = mainClass.getMethod("process",
      classOf[Array[String]], classOf[SimpleReporter], classOf[CompilerCallback])

    val reporter = new CustomSimpleReporter
    val callback = new CustomCompilerCallback

    // Run the compiler by calling dotty.tools.dotc.Main.process
    process.invoke(null, args.toArray, reporter, callback)

    // Wait for the compilation to finish
    while (!callback.paths.contains(inputFile.getAbsolutePath))
      sleep(10)

    outputDir.getAbsolutePath
  }

  private def runObjectMethod(objectName: String, classLoader: ClassLoader, methodName: String, paramClasses: Seq[Class[?]], arguments: Any*): Any = {
    val clazz = Class.forName(s"$objectName$$", true, classLoader)
    val module = clazz.getField("MODULE$").get(null)
    val method = module.getClass.getMethod(methodName, paramClasses *)
    method.invoke(module, arguments *)
  }

  private def runScala3(code: String, pkg: String = "mypackage", mainCls: String = "Main", mainMethod: String = "main"): Any = {
    val prog =
      s"""
         |package $pkg
         |
         |object $mainCls {
         |  def $mainMethod(args: Array[String]): Any = {
         |    $code
         |  }
         |}""".stripMargin

    if (logPsystem)
      println("Psystem: ")
      println(prog)
      println()

    val outputDir = compileCode(prog)
    val pkgDir = new File(s"$outputDir/")

    val urls = Array[URL](pkgDir.toURI.toURL)
    val classLoader = fromURLs(urls, this.getClass.getClassLoader)
    println(classLoader.getDefinedPackages.mkString(","))

    runObjectMethod(s"$pkg.$mainCls", classLoader, mainMethod, Seq(classOf[Array[String]]), Array.empty[String])
  }


  def compileAndLoadScala[T](source: String): T = {
    compilerCache.get(source) match
      case Some(value) => value.asInstanceOf[T]
      case None =>
        val result = runScala3(source)
        compilerCache += source -> result
        result.asInstanceOf[T]
  }
}
 */

class ScalaCompiler(val options: CompilerOptions) {
  val viatraLogging = options("viatra_logging")
  val logPsystem = viatraLogging.readBoolean("psystem")

  private var compilerCache: Map[String, Any] = Map()

  private case class DriverImpl(classpathDirectories: List[AbstractFile], outputDirectory: AbstractFile) extends Driver {
    private val compileCtx0 = initCtx.fresh

    val ctx = compileCtx0.fresh
      .setSetting(
        compileCtx0.settings.classpath,
        classpathDirectories.map(_.path).mkString(":")
      ).setSetting(
        compileCtx0.settings.usejavacp,
        true
      ).setSetting(
        compileCtx0.settings.outputDir,
        outputDirectory
      )

    val compiler: dotc.Compiler = newCompiler(using ctx)
  }

  private def runObjectMethod(objectName: String, classLoader: ClassLoader, methodName: String, paramClasses: Seq[Class[?]], arguments: Any*): Any = {
    val clazz = Class.forName(s"$objectName$$", true, classLoader)
    val module = clazz.getField("MODULE$").get(null)
    val method = module.getClass.getMethod(methodName, paramClasses *)
    method.invoke(module, arguments *)
  }

  private def compileCode(code: String, classpathDirectories: List[AbstractFile], outputDirectory: AbstractFile): Unit = {
    val driver = DriverImpl(classpathDirectories, outputDirectory)
    val sourceFile = SourceFile(VirtualFile("(inline)", code.getBytes(StandardCharsets.UTF_8)), Codec.UTF8)
    val run = driver.compiler.newRun(using driver.ctx)
    run.compileSources(List(sourceFile))
  }

  private def runScala3(code: String, pkg: String = "mypackage", mainCls: String = "Main", mainMethod: String = "main"): Any = {
    val prog =
      s"""
         |package $pkg
         |
         |object $mainCls {
         |  def $mainMethod(args: Array[String]): Any = {
         |    $code
         |  }
         |}""".stripMargin

    if (logPsystem)
      println("Psystem: ")
      println(prog)
      println()

    val outputDirectory = VirtualDirectory("(memory)")
    compileCode(prog, List() /*files.map(f => AbstractFile.getFile(f.toURI.toURL.getPath)).toList*/ , outputDirectory)
    val classLoader = AbstractFileClassLoader(outputDirectory, this.getClass.getClassLoader /*depClassLoader*/)
    runObjectMethod(s"$pkg.$mainCls", classLoader, mainMethod, Seq(classOf[Array[String]]), Array.empty[String])
  }


  def compileAndLoadScala[T](source: String): T = {
    compilerCache.get(source) match
      case Some(value) => value.asInstanceOf[T]
      case None =>
        val result = runScala3(source)
        compilerCache += source -> result
        result.asInstanceOf[T]
  }
}
