package inca.util

import inca.backend.ir.{CompileToPSystem, GP, PSystem, Printer}
import inca.frontend.fun.{CompileToGP, Fun}
import inca.frontend.funext.desugar.{Desugar, Desugarable}

import scala.collection.mutable
import scala.meta.Name.Indeterminate
import scala.meta.{Import, Importee, Importer, Term, Type}
import scala.reflect.ClassTag

object Meta {

  val TAB = "  "

  def typeOf[T:ClassTag](implicit tag: ClassTag[T]): Type =
    mkQualTypename(tag.runtimeClass.getCanonicalName)

  def symbolOf[T:ClassTag](implicit tag: ClassTag[T]): Term =
    mkQualName(tag.runtimeClass.getCanonicalName)

  def symbolOf(o: Any): Term = {
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

  def importOf[T:ClassTag](implicit tag: ClassTag[T]): Import =
    mkImport(tag.runtimeClass.getCanonicalName)

  def importOf(o: Any): Import = {
    val name = o.getClass.getCanonicalName
    mkImport(name.substring(0, name.length - 1))
  }

  def mkImport(s: String): Import = {
    val ss = s.split('.')
    var t: Term.Ref = Term.Name(ss(0))
    for (i <- 1 until ss.length - 1)
      t = Term.Select(t, Term.Name(ss(i)))
    Import(List(Importer(t, List(Importee.Name(Indeterminate(ss.last))))))
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
