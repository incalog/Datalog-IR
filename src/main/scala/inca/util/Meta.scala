package inca.util

import inca.lang.fun.{CompileToGP, Fun}
import inca.lang.funext.desugar.{Desugar, Desugarable}
import inca.lang.gp.{CompileToPSystem, GP, Printer}
import inca.lang.psystem.PSystem

import scala.collection.mutable
import scala.meta.{Term, Type}
import scala.reflect.ClassTag

object Meta {

  val TAB = "  "

  def typeOf[T:ClassTag](implicit tag: ClassTag[T]): Type =
    mkQualTypename(tag.runtimeClass.getCanonicalName)

  def symbolOf[T:ClassTag](implicit tag: ClassTag[T]): Term =
    mkQualName(tag.runtimeClass.getCanonicalName)

  def objectOf(o: Any): Term = {
    val name = o.getClass.getCanonicalName
    mkQualName(name.substring(0, name.length - 1))
  }

  def mkQualName(s: String): Term.Ref = {
    val ss = s.split('.')
    var t: Term.Ref = Term.Name(ss(0))
    for (i <- 1 until ss.length)
      t = Term.Select(t, Term.Name(ss(i)))
    t
  }

  def mkQualTypename(s: String): Type = {
    val ss = s.split('.')
    if (ss.length == 1)
      return Type.Name(ss(0))

    var qual: Term.Ref = Term.Name(ss(0))
    for (i <- 1 until (ss.length - 1))
      qual = Term.Select(qual, Term.Name(ss(i)))
    Type.Select(qual, Type.Name(ss(ss.length-1)))
  }

  def loadModule(module: Fun.Module, desugarables: Desugarable*): PSystem.Module = {
    val desugared = Desugar(desugarables:_*)(module)
    println(desugared.prettyprint(""))
    val gp = CompileToGP.transformModule(desugared)
    println(Printer.prettyModule(gp))
    loadModule(gp)
  }

  def loadModule(module: GP.Module): PSystem.Module = {
    val Seq(source) = CompileToPSystem.transAnalysis(Seq(module))
    println(source)
    compileModule(module.name, source.syntax)()
  }

  def compileModule(moduleName: String, source: String): () => PSystem.Module = {
    compileScala(source + "\n" + moduleName)
  }

  private val compilerCache: mutable.Map[String, () => AnyRef] = mutable.Map()
  def compileScala[A](source: String): () => A = {
    compilerCache.get(source).map(v => return v.asInstanceOf[() => A])

    import reflect.runtime.currentMirror
    import tools.reflect.ToolBox

    val toolbox = currentMirror.mkToolBox()
    val tree = toolbox.parse(source)
    val compiled = toolbox.compile(tree)
    val result = () => compiled().asInstanceOf[PSystem.Module]
    compilerCache += source -> result
    result.asInstanceOf[() => A]
  }

}
