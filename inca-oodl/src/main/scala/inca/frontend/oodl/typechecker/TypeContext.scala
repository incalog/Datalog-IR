package inca.frontend.oodl.typechecker

import inca.frontend.oodl.syntax.*
import inca.ir.Name

import scala.collection.immutable.MultiDict

trait TypeContext extends TypeIO:
  private var vars: Map[Name, (Var.Target, Type)] = Map()
  //private var tyVars: Map[Name, TName.Target] = Map()
  private var classDefs: MultiDict[Name, (Module, ClassDef)] = MultiDict()

  //private var funs: MultiDict[Name, (Module, (Var.Target, TFun))] = MultiDict()
  //private var dataDefs: Map[Name, DataDef] = Map()
  private var modules: Map[Name, Module] = Map()

  def scopedTypeContext[T](f: => T): T = {
    val varsSaved = vars
    //val tyVarsSaved = tyVars
    val c = classDefs
    //val funsSaved = funs
    val modulesSaved = modules
    val t = f
    vars = varsSaved
    //tyVars = tyVarsSaved
    classDefs = c
    //funs = funsSaved
    modules = modulesSaved
    t
  }

  def bindVar(name: Name, decl: Var.Target, ty: Type): Unit = {
    vars.get(name) foreach { case (previousDecl, _) =>
      error(s"Variable $name shadows previously defined variable $previousDecl", name, previousDecl)
    }
    vars += (name -> (decl, ty))
  }

  def lookupVar(name: Name): Option[(Var.Target,Type)] =
    vars.get(name) match
      case Some(entry) => Some(entry)
      case None => None

  def isFreeVar(name: Name): Boolean =
    !vars.contains(name)

  def getBindings: Map[Name, Type] =
    vars.view.mapValues(_._2).toMap

  /*def bindTyVar(name: Name, decl: TName.Target): Unit = {
    tyVars.get(name) match {
      case Some(prevDecl) => error(s"Type Variable $name shadows previously defined type variable $name at $prevDecl")
      case None =>
    }
    tyVars += name ->decl
  }

  def lookupTyVar(name: Name): Option[TName.Target] = {
    tyVars.get(name) match {
      case Some(decl) => Some(decl)
      case None =>
        error(s"Unbound type variable $name", name)
        None
    }
  }*/

  //def isTypeVar(name: Name): Boolean = tyVars.contains(name)

  def bindModule(module: Module): Unit = {
    val name = module.name
    modules.get(name) foreach { bound =>
      error(s"Found multiple modules with same name $name", name, bound.name)
    }
    modules += (name -> module)
  }

  def lookupModule(name: Name): Option[Module] =
    modules.get(name) match {
      case Some(module) => Some(module)
      case None =>
        error(s"Unknown module $name", name)
        None
    }

  def bindClass(clazz: ClassDef, module: Module): Unit =
    classDefs += clazz.name -> (module, clazz)

  def lookupClass(name: Name): Option[ClassDef] = classDefs.get(name) match {
    case set if set.size == 1 =>
      Some(set.head._2)
    case set if set.size >= 2 =>
      val modules = set.toSeq.map(_._1)
      val modulesStr = modules.map(_.name).mkString(", ")
      error(s"Ambiguous call to $name, found definitions in $modulesStr", (name +: modules): _*)
      None
    case _ =>
      error(s"Undefined class $name", name)
      None
  }
