package inca.util

import dotty.tools.dotc
import dotty.tools.io.AbstractFile
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.Driver
import dotty.tools.dotc.util.SourceFile
import dotty.tools.io.{VirtualDirectory, VirtualFile}

import java.net.URLClassLoader
import java.nio.charset.StandardCharsets
import dotty.tools.repl.AbstractFileClassLoader
import inca.util.compileroptions.CompilerOptions

import scala.io.Codec

class ScalaCompiler(val options: CompilerOptions) {
  val viatraLogging = options("viatra_logging")
  val logPsystem = viatraLogging.readBoolean("psystem")

  private var compilerCache: Map[String, Any] = Map()

  private case class DriverImpl(classpathDirectories: List[AbstractFile], outputDirectory: AbstractFile) extends Driver {
    private val compileCtx0 = initCtx.fresh

    given Context = compileCtx0.fresh
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

    val compiler: dotc.Compiler = newCompiler
  }

  private def runObjectMethod(objectName: String, classLoader: ClassLoader, methodName: String, paramClasses: Seq[Class[?]], arguments: Any*): Any = {
    val clazz = Class.forName(s"$objectName$$", true, classLoader)
    val module = clazz.getField("MODULE$").get(null)
    val method = module.getClass.getMethod(methodName, paramClasses *)
    method.invoke(module, arguments *)
  }

  private def compileCode(code: String, classpathDirectories: List[AbstractFile], outputDirectory: AbstractFile): Unit = {
    val driver = DriverImpl(classpathDirectories, outputDirectory)
    import driver.given Context

    val sourceFile = SourceFile(VirtualFile("(inline)", code.getBytes(StandardCharsets.UTF_8)), Codec.UTF8)
    val run = driver.compiler.newRun
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
    runObjectMethod("mypackage.Main", classLoader, "main", Seq(classOf[Array[String]]), Array.empty[String])
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
