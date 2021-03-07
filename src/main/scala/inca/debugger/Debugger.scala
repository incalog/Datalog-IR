package inca.debugger

import inca.compiler.CompiledFunModule
import inca.frontend.core.tree._
import inca.runtime.{Database, DatabaseAccessor}
import inca.runtime.Query.Matcher

import scala.collection.mutable

class Debugger(feed: Database, matcher: Matcher, module: CompiledFunModule) {
  private[debugger] val db = new DatabaseAccessor(feed)
  private[debugger] val debugEnv = new Environment(matcher.getPatternName.replace(module.fun.name + "_", ""), module)

  private var cf = debugEnv.funName // "Current" function

  private val pntr: mutable.Map[Name, Int] = mutable.Map(cf -> 0) // Pointers for each function traversed
  private val callers: mutable.Map[Name, Name] = mutable.Map() // Caller for each function

  val matches = new Matches(db, matcher, debugEnv)

  def allMatches: Seq[Match] = matches.matches

  def load(mat: Match): Unit = {
    debugEnv.env += debugEnv.funName -> mutable.Map()
    mat.inputs.foreach(col => {
      debugEnv.env(debugEnv.funName).updateWith(Name(col.name)) {
        case Some(v) => Some(v + col.value)
        case None => Some(Set(col.value))
      }
    })
  }

  private[debugger] def stepOver(): Unit = {
    val p = pntr(cf)
    if(p > 0) parseStmt(cf, debugEnv.stmts(cf)(p - 1))

    if(p < debugEnv.stmts(cf).length) {
      println(":> " + debugEnv.typedStmts(cf)(p))
      pntr(cf) += 1
    } else
      stepOut()
  }

  private[debugger] def stepInto(): Unit = {
    val callExp = debugEnv.stmts(cf)(pntr(cf) - 1).ensureCore match {
      case Assign(_, exp) => exp
      case Assert(cond) => cond
      case Yield(exp) => exp
      case _ => throw new IllegalArgumentException("Statement has no function calls")
    }

    val fun = callExp.ensureCore match {
      case Call(name, args, _) =>
        initFunParams(name, args.map(arg => parseExp(cf, arg)))
        name
      case _ => throw new IllegalArgumentException("Statement has no function calls")
    }

    pntr += fun -> 0
    callers += fun -> cf

    cf = fun

    println("Stepping into: " + fun + " Pointers: " + pntr + " Callers: " + callers)
    stepOver()
  }

  private[debugger] def stepOut(): Unit = {
    println("Stepping out of : " + cf + " Pointers: " + pntr + " Callers: " + callers)

    pntr(cf) = 0
    cf = callers.getOrElse(cf, {
      println("End of program reached")
      cf
    })

    stepOver() // FIXME Ugly hack? Idk. Terminator map?
  }


  private def parseStmt(fun: Name, s: Statement): Set[ColumnValue] = s.ensureCore match {
    case Assign(names, exp) =>
      debugEnv.env(fun) += names.head -> parseExp(fun, exp) // TODO Multiple assignments
      Set()

    case Yield(exp) => parseExp(fun, exp)

    case Assert(cond) => Set()
    case Values(name, typ) => Set()
    case FailStatement => Set()
  }

  private def parseExp(fun: Name, e: Expression): Set[ColumnValue] = e.ensureCore match {
    case Var(name) => debugEnv.env(fun)(name)
    case Constant(lit) => Set(ScalaValue(lit))

    case PathAccess(receiver, link) =>
      parseExp(fun, receiver).flatMap(cv => cv match {
        case uriValue: URIValue => getLinks(uriValue, link)
        case _ => Set()
      })

    case Call(name, args, transitive) =>
      initFunParams(name, args.map(arg => parseExp(fun, arg)))
      run(name)

    case Cast(src, targetTyp) =>
      if(parseExp(fun, src).forall(cv => isOfType(cv, targetTyp)))
        parseExp(fun, src) // Check and ensure cast-ability?
      else Set()

    case InstanceOf(exp, ty) => Set(ScalaValue(parseExp(fun, exp).forall(cv => isOfType(cv, ty)))) // Confirm behavior
    case NotInstanceOf(exp, ty) => Set(ScalaValue(parseExp(fun, exp).forall(cv => !isOfType(cv, ty))))

    case Eq(lhs, rhs) => Set(ScalaValue(parseExp(fun, lhs) == parseExp(fun, rhs))) // Confirm primitives/ScalaVals
    case Neq(lhs, rhs) => Set(ScalaValue(parseExp(fun, lhs) != parseExp(fun, rhs)))

    case Def(exp) => parseExp(fun, exp) // ??

    case Count(call) => Set(ScalaValue(parseExp(fun, call).size)) // ??
    case Tuple(exps) => Set(ScalaValue(exps.flatMap(e => parseExp(fun, e)))) // ??

    case _ => Set()
  }

  private def initFunParams(fun: Name, args: Seq[Set[ColumnValue]]): Unit = {
    debugEnv.env += fun -> mutable.Map()
    debugEnv.funParams(fun).zip(args).foreach {
      case (param, arg) => debugEnv.env(fun) += param.name -> arg
    }
  }

  private def run(fun: Name): Set[ColumnValue] = { // TODO Multiple outputs
    debugEnv.stmts(fun).flatMap(stmt => parseStmt(fun, stmt)).toSet
  }

  private def getLinks(uriValue: URIValue, link: Link): Set[ColumnValue] = {
    val lnkNodes = db.linkNodeInstances.filter {
      case (lnk, uris) => lnk._2 == link.prettyprint && uris.index.containsKey(uriValue.uri)
    }

    if(lnkNodes.isEmpty) {
      db.linkPrimitiveInstancesByValue1(uriValue.uri).filter {
        case (lnk, _) => lnk._2 == link.prettyprint
      }.flatMap {
        case (_, vals) => vals.index(uriValue.uri).toSeq.map(prim => ScalaValue(prim))
      }.toSet

    } else {
      lnkNodes.map {
        case (_, uris) =>
          val uri = uris.index.get(uriValue.uri)
          URIValue(uri, db.nodeInstancesByValue(uri).keys.toSeq)
      }.toSet
    }
  }

  private def isOfType(cv: ColumnValue, ty: Type): Boolean = {
    /*println("COMP Type " + (cv match {
      case u: URIValue => u.types.mkString(",")
      case sv: ScalaValue => sv.v
    }) + " TO " + ty)*/
    true // TODO
  }
}
