package inca.util

import scala.collection.mutable
import scala.meta.Name.Indeterminate
import scala.meta.{Import, Importee, Importer, Term, Type}
import scala.reflect.ClassTag

object Meta {

  val TAB = "  "

  def typeOf[T:ClassTag](implicit tag: ClassTag[T]): Type.Ref =
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

  def mkQualTypename(s: String): Type.Ref = {
    val ss = s.split('.')
    if (ss.length == 1)
      return Type.Name(ss(0))

    var qual: Term.Ref = Term.Name(ss(0))
    for (i <- 1 until (ss.length - 1))
      qual = Term.Select(qual, Term.Name(ss(i)))
    Type.Select(qual, Type.Name(ss(ss.length-1)))
  }

  class Scala[+T <: meta.Tree](val tree: T) {
    lazy val structure: String = this.tree.structure

    def syntax: String = tree.syntax

    override def hashCode(): Int =
      structure.hashCode

    override def equals(obj: Any): Boolean = obj match {
      case that: Scala[_] => this.structure == that.structure
      case _ => false
    }

    override def toString: String = tree.syntax
  }
  object Scala {
    def apply[T <: meta.Tree](tree: T): Scala[T] = new Scala(tree)
    def unapply[T <: meta.Tree](s: Scala[T]): Option[T] = Some(s.tree)
  }


  private val compilerCache: mutable.Map[String, () => Any] = mutable.Map()
  def compileAndLoadScala[A](source: String): () => A = {
    compilerCache.get(source).map(v => return v.asInstanceOf[() => A])

    import reflect.runtime.currentMirror
    import tools.reflect.ToolBox

    //    println(source)

    val toolbox = currentMirror.mkToolBox(options = "-Ymacro-annotations")
    val tree = toolbox.parse(source)
    val compiled = toolbox.compile(tree)
    val result = () => compiled()
    compilerCache += source -> result
    result.asInstanceOf[() => A]
  }

  class ScalaCompiler {
    import reflect.runtime.{currentMirror, universe}
    import tools.reflect.ToolBox

    private val toolbox: ToolBox[universe.type] = currentMirror.mkToolBox(options = "-Ymacro-annotations")

    private val compilerCache: mutable.Map[String, Any] = mutable.Map()

    def compileAndLoadScala[A](source: String): A = {
      compilerCache.get(source).map(v => return v.asInstanceOf[A])

      val tree = toolbox.parse(source)
      val compiled = toolbox.compile(tree)
      val result = compiled().asInstanceOf[A]
      compilerCache += source -> result
      result
    }

    def define(source: String): String = {
      val tree = toolbox.parse(source)
      val sym = toolbox.define(tree.asInstanceOf[universe.ImplDef])
      sym.fullName
    }
  }
}
