package inca.backend.ir

import inca.backend.ir.GP._
import inca.runtime.context.LanguageMetaInfo
import inca.util.Meta.Scala
import truechange.SortType

import scala.collection.mutable
import scala.meta.{Defn, Importee, Lit, Pat, Term}

// TODO: This is an almost exact copy of ScalaTypeContext, can we generalize this?
trait TypeOps {

  import scala.reflect.runtime.{currentMirror, universe}
  import scala.tools.reflect.{ToolBox, ToolBoxError}

  private lazy val toolbox: ToolBox[universe.type] = currentMirror.mkToolBox()

  // remember the imports
  private var imports: mutable.ListBuffer[Scala[meta.Import]] = mutable.ListBuffer()

  // remember the code that has already been seen to consider blockdefs and values when typing scala code
  // wanted to use ToolBox.define but it only allows toplevel declarations such as objects and classes
  private var seenCode: mutable.ListBuffer[Scala[meta.Stat]] = mutable.ListBuffer()

  // remember which name are bound by importing
  // TODO name with sourcelocation
  private var importBoundNames: mutable.ListBuffer[String] = mutable.ListBuffer()

  // remember which names are top-level definitions
  private var toplevelBoundNames: mutable.ListBuffer[String] = mutable.ListBuffer()

  def boundNames: Seq[String] = importBoundNames.toSeq ++ toplevelBoundNames

  def initializeScala(module: GP.Module): Unit = {
    module.scalaImports.foreach(imp => registerImport(Scala(imp.tree)))
    module.scalaBlockDefs.foreach(bd => registerBlockDef(Scala(bd.tree)))
  }

  def registerImport(imp: Scala[meta.Import]): Unit = {
    imp.tree.importers.head.importees.foreach {
      case Importee.Name(n) => importBoundNames += n.value
      case Importee.Rename(_, n) => importBoundNames += n.value
      case _: Importee.Unimport => // nothing
      case _: Importee.Wildcard => // nothing cannot happen
      case _ => // nothing cannot happen
    }
    imports += imp
  }


  private var topLevelObject: universe.Symbol = _

  protected def typecheckTopLevelObject(): Unit = {
    if (topLevelObject == null) {
      val scalaObject =
        s"""
           |object ScalaObject {
           |${imports.mkString("\n")}
           |${seenCode.mkString("\n")}
           |}
           |""".stripMargin
      val tree = toolbox.parse(scalaObject)
      topLevelObject = toolbox.define(tree.asInstanceOf[universe.ImplDef])
    }
  }

  def registerBlockDef(bd: Scala[meta.Stat]): Unit = {
    bd.tree match {
      case Defn.Trait(_, name, _, _, _) =>
        toplevelBoundNames += name.value
      case Defn.Class(_, name, _, _, _) =>
        toplevelBoundNames += name.value
      case Defn.Object(_, name, _) =>
        toplevelBoundNames += name.value
      case Defn.Val(_, pats, _, _) =>
        val bound = pats.flatMap(collectVars)
        toplevelBoundNames ++= bound
      case Defn.Var(_, pats, _, _) =>
        val bound = pats.flatMap(collectVars)
        toplevelBoundNames ++= bound
      case Defn.Def(_, name, _, _, _, _) =>
        toplevelBoundNames += name.value
      case Defn.Type(_, name, _, _) =>
        toplevelBoundNames += name.value
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
    typecheckTopLevelObject()
    val completeCode =
      s"""{
         |  import ${topLevelObject.fullName}._
         |  ${imports.mkString("\n")}
         |  $code
         |}
         |""".stripMargin
    val tree = toolbox.parse(completeCode)
    try {
      val typechecked = toolbox.typecheck(tree)
      val typ = typechecked.tpe.dealias
      val normalizedType =
        typ.toString.replace(s"${topLevelObject.fullName}.ScalaObject$$", "")
      Left(normalizedType)
    } catch {
      case err@ToolBoxError(msg, throwable) =>
        val cleanMsg = msg.replace(s"${topLevelObject.fullName}.", "")
        Right(ToolBoxError(cleanMsg, throwable))
    }
  }

  def subtypeScala(ty1: meta.Type, ty2: meta.Type): Boolean = {
    val code =
      s"""{
         |  val v1: ${ty1.syntax} = ???
         |  val v2: ${ty2.syntax} = v1
         |}""".stripMargin

    typecheckScala(code) match {
      case Left(str) =>
        str == "Unit"
      case Right(err) =>
        false
    }
  }

  def subtype(ty1: Type, ty2: Type, languageMetaInfo: LanguageMetaInfo): Boolean =
    meet(ty1, ty2, languageMetaInfo).contains(ty1)

  def meet(ty1: Type, ty2: Type, languageMetaInfo: LanguageMetaInfo): Option[Type] = (ty1, ty2) match {
    case (_, _) if ty1 == ty2 => Some(ty1)
    case (TAny, _) => Some(ty2)
    case (_, TAny) => Some(ty1)
    case (TAnyLinked, _:TLinked) => Some(ty2)
    case (_:TLinked,TAnyLinked) => Some(ty1)
    case (TNode(name1), TNode(name2)) =>
      if (languageMetaInfo.nodeSupertypes.containsEntry(SortType(name1) -> SortType(name2)))
        Some(ty1)
      else if (languageMetaInfo.nodeSupertypes.containsEntry(SortType(name2) -> SortType(name1)))
        Some(ty2)
      else
        None
    case (TList(s1), TList(s2)) => meet(s1, s2, languageMetaInfo).map(t => TList(t.asInstanceOf[TLinked]))
    case (TScala(s1), TScala(s2)) =>
      if (subtypeScala(s1.tree, s2.tree))
        Some(ty1)
      else if (subtypeScala(s2.tree, s1.tree))
        Some(ty2)
      else
        None
    case (_, TScala(_)) => meet(TScala(Scala(ty1.asScala)), ty2, languageMetaInfo)
    case (TScala(_), _) => meet(ty1, TScala(Scala(ty2.asScala)), languageMetaInfo)
    case _ => None
  }

  def meet(tys: Iterable[Type], languageMetaInfo: LanguageMetaInfo): Option[Type] = {
    if (tys.isEmpty)
      None
    else {
      var ty = tys.head
      for (other <- tys.tail) {
        ty = meet(ty, other, languageMetaInfo).getOrElse(return None)
      }
      Some(ty)
    }
  }
}
