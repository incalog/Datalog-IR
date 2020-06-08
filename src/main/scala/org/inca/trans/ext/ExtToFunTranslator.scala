package org.inca.trans.ext

import org.inca.lang.FunLang._

class ExtToFunTranslator {
//  def transformModule(module: Module): Module = {
//    val undefs = module.funs.flatMap(collectPathInUndef)
//    val ninsts = module.funs.flatMap(collectNotInstanceOf)
//    // generate helpers
//    val helpers = undefs.map(genUndefPathHelper) ++ ninsts.map(genNotInstanceOfHelper)
//    // construct map Name => Fun
//    val funs = module.funs.map { fun => fun.name -> fun}.toMap
//    val patters = (module.funs ++ helpers).map { fun => transFun(fun) }
//    Module(module.name, module.imports, module.funs ++ helpers)
//  }
//
//  def collectNotInstanceOf(fun: PatternFunction): Seq[NotInstanceOf] = fun.bodies.flatMap {
//    case Alternative(stmts) => stmts.collect {
//      case Assert(cond) => cond match {
//        case ninst@NotInstanceOf(_, _) => Seq(ninst)
//        case _ => Nil
//      }
//    }.flatten
//  }
//
//  def collectPathInUndef(fun: PatternFunction): Seq[PathAccess] = fun.bodies.flatMap {
//    case Alternative(stmts) => stmts.collect {
//      case Assert(cond) => cond match {
//        case Undef(cond: PathAccess) => Seq(cond)
//        case _ => Nil
//      }
//    }.flatten
//  }
//
//  def nameOfUndefPathHelper(access: PathAccess): String = "generated_helper_undefpath_" + access.path.map(_.toString).mkString(".")
//
//  def genUndefPathHelper(path: PathAccess): PatternFunction = {
//    PatternFunction(
//      Some(Private),
//      nameOfUndefPathHelper(path),
//      List(Param("in", Some(path.path.head.nodeType))),
//      List(),
//      List(Alternative(List(Assert(Def(path))))))
//  }
//
//  def nameOfNotInstanceOfHelper(ninst: NotInstanceOf): String = "generated_helper_notinstanceof_" + nameOfType(ninst.typ)
//
//  def nameOfType(typ: Type): String = typ match {
//    case TNodeType(wrapped) => wrapped.cls.getName.replace(".", "_")
//    case TBool => "TBool"
//    case TInt => "TInt"
//    case TLong => "TLong"
//    case TDouble => "TDouble"
//    case TString => "TString"
//  }
//
//  def genNotInstanceOfHelper(ninst: NotInstanceOf): PatternFunction =
//    PatternFunction(
//      Some(Private),
//      nameOfNotInstanceOfHelper(ninst),
//      List(Param("in", Some(ninst.typ))),
//      List(),
//      // body is empty because relation is only applicable if c is actually of type ninst.typ
//      List())
//
//  def transFun(fun: PatternFunction): PatternFunction = {
//    PatternFunction(fun.vis, fun.name, fun.params, fun.outParams, fun.bodies.map(transAlternative))
//  }
//
//  def transAlternative(alt: Alternative): Alternative = {
//    Alternative(alt.stmts.map(transStatement))
//  }
//
//  def transStatement(stmt: Statement): Statement = stmt match {
//    case Assert(cond) => cond match {
//      case Undef(exp) =>
//      case ninst@NotInstanceOf(exp, typ) =>
//        Call(PatternCall(nameOfNotInstanceOfHelper(ninst), Seq(exp)), count = false)
//    }
//    case _ => stmt
//  }
}
