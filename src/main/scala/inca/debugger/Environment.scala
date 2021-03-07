package inca.debugger

import inca.compiler.CompiledFunModule
import inca.frontend.core.tree.{Name, Param, PatternFunction, Statement}

import scala.collection.mutable


private[debugger] class Environment(fun: String, module: CompiledFunModule) {

  private[debugger] val funName = Name(fun)
  private[debugger] val funParams: Map[Name, Seq[Param]] = module.fun.content.map({
    case pf: PatternFunction => (pf.name, pf.params)
  }).toMap

  private[debugger] val stmts: Map[Name, Seq[Statement]] = { // Function name -> Seq of Statements
    module.fun.content.map {
      case pf: PatternFunction =>
        pf.name -> pf.bodies.head.stmts // TODO Consider multiple bodies!
    }.toMap
  }
  private[debugger] val typedStmts: Map[Name, Seq[String]] = {
    module.typed.content.map {
      case pf: PatternFunction =>
        pf.name -> pf.bodies.head.stmts.map(s => s.prettyprint(""))
    }.toMap
  }

  private[debugger] val env: mutable.Map[Name, mutable.Map[Name, Set[ColumnValue]]] = mutable.Map() // Function -> Env

}
