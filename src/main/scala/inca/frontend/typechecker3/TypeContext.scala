package inca.frontend.typechecker3

import inca.frontend.core.Core.{Module, Name, PatternFunction, TypeAnno}

import scala.collection.immutable.MultiDict

trait TypeContext extends TypeIO {
  private var vars: Map[Name, (Name, TypeAnno)] = Map()
  private var funs: MultiDict[Name, (Module, PatternFunction)] = MultiDict()
  private var modules: Map[Name, Module] = Map()


  def bindVar(name: Name, ty: TypeAnno): Unit = {
    vars.get(name) foreach { case (bound, _) =>
      warn(s"Variable $name shadows previously defined variable $bound", name, bound)
    }
    vars += (name -> (name, ty))
  }

  def bindVars(bindings: Seq[(Name, TypeAnno)]): Unit = {
    bindings.foreach(b => bindVar(b._1, b._2))
  }

  def lookupVar(name: Name): Option[TypeAnno] =
    vars.get(name) match {
      case Some((_, ty)) => Some(ty)
      case None =>
        error(s"Unbound variable $name", name)
        None
    }


  def bindFun(fun: PatternFunction, module: Module): Unit = {
    funs += fun.name -> (module, fun)
  }

  def bindFuns(bindings: Seq[(PatternFunction, Module)]): Unit = {
    bindings.foreach(b => bindFun(b._1, b._2))
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


  //  // TODO need to think about binding refinement
//  // look at type refinement type systems
//  def refineBinding(name: Name, ty: TypeAnno): TypeContext = {
//    val prevTy = vars.get(name)
//    prevTy match {
//      case Some(value) =>
//        TypeContext(vars + (name -> ty), funs, fun)
//      case None =>
//        TypeContext(vars + (name -> ty), funs, fun)
//    }
//  }
}