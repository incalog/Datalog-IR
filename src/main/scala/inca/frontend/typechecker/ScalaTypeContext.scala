package inca.frontend.typechecker

import inca.frontend.core.{Name, ScalaBlockDef, ScalaImport}
import inca.util.Meta.Scala

import scala.collection.mutable
import scala.meta.parsers._
import scala.meta.quasiquotes._
import scala.meta.{Defn, Importee, Lit, Pat, Term, Tree}

trait ScalaTypeContext extends TypeContext {
  import scala.reflect.runtime.{currentMirror, universe}
  import scala.tools.reflect.{ToolBox, ToolBoxError}

  private val toolbox: ToolBox[universe.type] = currentMirror.mkToolBox()

  // remember the imports
  private var imports: mutable.ListBuffer[Scala[meta.Import]] = mutable.ListBuffer()

  // remember the code that has already been seen to consider blockdefs and values when typing scala code
  // wanted to use ToolBox.define but it only allows toplevel declarations such as objects and classes
  private var seenCode: mutable.ListBuffer[Scala[meta.Stat]] = mutable.ListBuffer()

  // remember which name are bound
  // TODO name with sourcelocation
  protected var boundNames: mutable.ListBuffer[String] = mutable.ListBuffer()


  override def scopedTypeContext[T](f: => T): T = {
    val prevSeenCode = seenCode
    val prevImports = imports
    val prevBoundNames = boundNames
    val t = super.scopedTypeContext(f)
    seenCode = prevSeenCode
    imports = prevImports
    boundNames = prevBoundNames
    t
  }

  def registerImport(imp: ScalaImport): Unit = {
    imp.tree.importers.head.importees.foreach {
      case Importee.Name(n) => boundNames += n.value
      case Importee.Rename(_, n) => boundNames += n.value
      case _: Importee.Unimport => // nothing
      case _: Importee.Wildcard =>
        // wildcards are not allowed
        error("Wildcard Scala imports are now allowed", imp)
      case _ =>
        error("Unsupported Scala import", imp)
    }
    imports += imp
  }

  def registerBlockDef(bd: ScalaBlockDef): Unit = {
    bd.tree match {
      case Defn.Trait(_, name, _, _, _) =>
        boundNames = boundNames :+ name.value
      case Defn.Class(_, name, _, _, _) =>
        boundNames = boundNames :+ name.value
      case Defn.Object(_, name, _) =>
        boundNames = boundNames :+ name.value
      case Defn.Val(_, pats, _, _) =>
        val bound = pats.flatMap(collectVars)
        boundNames = boundNames ++ bound
      case Defn.Def(_, name, _, _, _, _) =>
        boundNames = boundNames :+ name.value
      case Defn.Type(_, name, _, _) =>
        boundNames = boundNames :+ name.value
      case _: Defn.Var =>
        // vars have no meaning in IncA
        error("Top-level variable definition is not supported", bd)
    }

    seenCode += bd
  }

  private def collectVars(pat: meta.Pat): Set[String] = pat match {
    case Term.Name(str) => Set(str)
    case Pat.Var(name) =>  Set(name.value)
    case Lit(_) => Set()
    case Pat.Wildcard() => Set()
    case Pat.Tuple(pats) => pats.flatMap(collectVars).toSet
    case Pat.SeqWildcard() => Set()
    case Pat.Typed(pat, _) => collectVars(pat)
    case Term.Select(term, _) => term match {
      case Term.Name(n) => Set(n)
      case inner: Term.Select => collectVars(inner)
    }
    case Pat.Extract(_, value) =>
      value.flatMap(collectVars).toSet
    case Pat.Alternative(lhs, rhs) =>
      collectVars(lhs) ++ collectVars(rhs)
    case Pat.ExtractInfix(pat, _, value) =>
      collectVars(pat) ++ value.flatMap(collectVars)

  }

  def typecheckScala(code: String): Either[String, Throwable] = {
    val completeCode = (imports.toList ++ seenCode.toSeq).mkString("\n") + s"\n$code"

    val tree = toolbox.parse(completeCode)
    try {
      val typechecked = toolbox.typecheck(tree)

      val typ = typechecked.tpe.dealias
      Left(typ.toString)
    } catch {
      case err@ToolBoxError(msg, _) =>
        Right(err)
    }
  }

  def subtypeScala(ty1: meta.Type, ty2: meta.Type): Boolean = {
    val code =
      s"""{
         |  ${imports.mkString("\n")}
         |  val v1: ${ty1.syntax} = ???
         |  val v2: ${ty2.syntax} = v1
         |}""".stripMargin

    typecheckScala(code) match {
      case Left(str) =>
        str == "Unit"
      case Right(_) =>
        false
    }
  }
}

