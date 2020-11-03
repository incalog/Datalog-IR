package inca.frontend.typechecker

import inca.frontend.core._

import scala.collection.immutable.MultiDict

trait TypeContext extends TypeIO {
  private var vars: Map[Name, (Name, Type)] = Map()
  private var funs: MultiDict[Name, (Module, PatternFunction)] = MultiDict()
  private var modules: Map[Name, Module] = Map()

  def scopedTypeContext[T](f: => T): T = {
    val varsSaved = vars
    val funsSaved = funs
    val modulesSaved = modules
    val t = f
    vars = varsSaved
    funs = funsSaved
    modules = modulesSaved
    t
  }

  def bindVar(name: Name, ty: Type): Unit = {
    vars.get(name) foreach { case (bound, _) =>
      error(s"Variable $name shadows previously defined variable $bound", name, bound)
    }
    vars += (name -> (name, ty))
  }

  def lookupVar(name: Name): Option[Type] =
    vars.get(name) match {
      case Some((_, ty)) => Some(ty)
      case None =>
        error(s"Unbound variable $name", name)
        None
    }

  def getBindings: Map[Name, Type] =
    vars.view.mapValues(_._2).toMap


  def bindFun(fun: PatternFunction, module: Module): Unit = {
    funs += fun.name -> (module, fun)
  }

  def lookupFun(name: Name): Option[PatternFunction] =
    funs.get(name) match {
      case set if set.isEmpty =>
        error(s"Unbound function $name", name)
        None
      case set if set.size == 1 =>
        Some(set.head._2)
      case set if set.size >= 2 =>
        val modules = set.toSeq.map(_._1)
        val modulesStr = modules.map(_.name).mkString(", ")
        error(s"Ambiguous function call $name, found definitions in $modulesStr", (name +: modules): _*)
        None
    }


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

  //  // TODO need to think about binding refinement
//  // look at type refinement type systems
//  def refineBinding(name: Name, ty: Type): TypeContext = {
//    val prevTy = vars.get(name)
//    prevTy match {
//      case Some(value) =>
//        TypeContext(vars + (name -> ty), funs, fun)
//      case None =>
//        TypeContext(vars + (name -> ty), funs, fun)
//    }
//  }
}