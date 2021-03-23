package inca.debugger

import inca.compiler.CompiledFunModule
import inca.frontend.core.tree._
import inca.runtime.Query.Matcher
import inca.runtime.{Database, DatabaseAccessor}

class Debugger(feed: Database, matcher: Matcher, module: CompiledFunModule) {
  private[debugger] val db = new DatabaseAccessor(feed)

  private[debugger] val funName = Name(matcher.getPatternName.replace(module.fun.name + "_", ""))
  private[debugger] val funParams: Map[Name, Seq[Param]] = module.fun.content.map({
    case pf: PatternFunction => (pf.name, pf.params)
  }).toMap

  private[debugger] val stmts: Map[Name, Seq[Seq[Statement]]] = {
    module.fun.content.map {
      case pf: PatternFunction =>
        pf.name -> pf.bodies.map(body => body.stmts)
    }.toMap
  }
  private[debugger] val typedStmts: Map[Name, Seq[Seq[String]]] = {
    module.typed.content.map {
      case pf: PatternFunction =>
        pf.name -> pf.bodies.map(body => body.stmts.map(s => s.prettyprint("")))
    }.toMap
  }
  private[debugger] val funSigs: Map[Name, String] = {
    module.typed.content.map {
      case pf: PatternFunction =>
        val vis = if (pf.vis.contains(Private)) "private " else ""
        val params = pf.params.map(_.prettyprint).mkString(", ")
        val outType = pf.outType.prettyprint
        pf.name -> s"${vis}def ${pf.name}($params): $outType"
    }.toMap
  }

  private[debugger] val callStack: CallStack = new CallStack()

  val matches = new Matches(db, matcher, funName, funParams)

  def allMatches: Seq[Match] = matches.matches

  def load(mat: Match): Unit = {
    val frame: StackFrame = new StackFrame(callStack.currentAddr, funName, Seq(), if(hasMultipleBodies(funName)) -1 else 0)
    funParams(funName).zip(mat.inputs.map(col => Set(col.value))).foreach {
      case (param, arg) => frame.env += param.name -> arg
    }
    callStack.push(frame)
  }


  private[debugger] def stepOver(): Unit = {
    if(hasMultipleBodies(callStack.currentFun) && callStack.currentPntr == -1)
      throw MultipleBodiesException(s"Function has ${stmts(callStack.currentFun).size} bodies")

    if(callStack.currentPntr >= 0) {
      val ret = traverseStmt(currentStatement)
      if(currentStatement.isInstanceOf[TerminatorStatement])
        finishFrame(ret)
      else
        callStack.frame().incrementPntr()

    } else callStack.frame().incrementPntr()

    println(":> " + currentLine)
  }

  private[debugger] def stepInto(): Unit = {
    val calls = collectFunCalls(currentStatement)

    if(calls.isEmpty)
      throw NoFunctionCallsException(s"No function calls on line: $currentLine")
    else if(calls.size > 1)
      throw MultipleFunctionCallsException(s"Found ${calls.size} function calls", calls)
    else
      stepIntoFunction(calls.head._1, calls.head._2)
  }

  private[debugger] def stepOut(): Unit = {
    if(callStack.currentBody >= 0) {
      val remainingStmts: Seq[Statement] = stmts(callStack.currentFun)(callStack.currentBody).drop(callStack.currentPntr)
      val ret = remainingStmts.flatMap(stmt => traverseStmt(stmt)).toSet
      finishFrame(ret)

    } else callStack.pop()
  }


  private[debugger] def stepIntoBody(b: Int): Unit = {
    if(b >= stmts(callStack.currentFun).size)
      throw InvalidCommandException(s"Function only has ${stmts(callStack.currentFun).size} bodies")

    val frame: StackFrame = new StackFrame(callStack.frame().caller, callStack.currentFun, callStack.frame().args, b)
    funParams(callStack.currentFun).map(p => (p.name, callStack.frame().env(p.name))).foreach {
      case (p, v) => frame.env += p -> v
    }

    callStack.push(frame)
    callStack.frame().incrementPntr()
    println(":> " + currentLine)
  }

  private[debugger] def stepIntoFunction(fun: Name, args: Seq[Expression]): Unit = {
    initFun(fun, args.map(arg => traverseExp(arg)), if(hasMultipleBodies(fun)) -1 else 0)
    stepOver()
  }

  private def finishFrame(retVal: Set[ColumnValue]): Unit = {
    val sf = callStack.pop()
    if(sf.caller >= 0)
      callStack.frame(sf.caller).intermVars += (sf.funName, sf.args, sf.body) -> retVal
  }

  private def initFun(fun: Name, args: Seq[Set[ColumnValue]], body: Int): Unit = {
    val frame: StackFrame = new StackFrame(callStack.currentAddr, fun, args, body)
    funParams(fun).zip(args).foreach {
      case (param, arg) => frame.env += param.name -> arg
    }
    callStack.push(frame)
  }

  private def currentStatement: Statement = {
    try {
      stmts(callStack.currentFun)(callStack.currentBody)(callStack.currentPntr)
    } catch {
      case _: IndexOutOfBoundsException =>
        throw InvalidPointerException(s"Invalid indices f:${callStack.currentFun} b:${callStack.currentBody} p:${callStack.currentPntr}")
    }
  }

  private[debugger] def currentLine: String = {
    if(callStack.currentPntr >= 0 && callStack.currentPntr < typedStmts(callStack.currentFun)(callStack.currentBody).size)
      typedStmts(callStack.currentFun)(callStack.currentBody)(callStack.currentPntr)
    else
      funSigs(callStack.currentFun)
  }

  private def hasMultipleBodies(fun: Name): Boolean = stmts(fun).size > 1


  private def run(fun: Name, args: Seq[Expression]): Set[ColumnValue] = {
    stmts(fun).zipWithIndex.flatMap {
      case (body, i) =>
        val resolvedArgs = args.map(arg => traverseExp(arg))
        callStack.frame().intermVars.getOrElse((fun, resolvedArgs, i), {
          initFun(fun, resolvedArgs, i)
          val ret = body.flatMap(stmt => traverseStmt(stmt))
          finishFrame(ret.toSet)
          ret
        })
    }.toSet
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

  private def collectFunCalls(s: Statement): Seq[(Name, Seq[Expression])] = s.ensureCore match {
    case Assign(_, exp) => collectFunCalls(exp)
    case Assert(cond) => collectFunCalls(cond)
    case Yield(exp) => collectFunCalls(exp)
    case _ => Seq()
  }

  private def collectFunCalls(e: Expression): Seq[(Name, Seq[Expression])] = e.ensureCore match {
    case Var(_) => Seq()
    case Eq(lhs, rhs) => collectFunCalls(lhs) ++ collectFunCalls(rhs)
    case Neq(lhs, rhs) => collectFunCalls(lhs) ++ collectFunCalls(rhs)
    case InstanceOf(exp, _) => collectFunCalls(exp)
    case NotInstanceOf(exp, _) => collectFunCalls(exp)
    case Cast(src, _) => collectFunCalls(src)
    case Def(exp) => collectFunCalls(exp)
    case Undef(exp) => collectFunCalls(exp)
    case Wildcard => Seq()
    case Constant(_) => Seq()
    case PathAccess(receiver, _) => collectFunCalls(receiver)
    case Call(name, args, _) => Seq((name, args)) ++ args.flatMap(collectFunCalls)
    case Count(call) => collectFunCalls(call)
    case Tuple(exps) => exps.flatMap(collectFunCalls)
    case Eval(_) => Seq()
    case Aggregate(agg, bodies) => collectFunCalls(agg) ++ bodies.flatMap(
      b => b.stmts.flatMap(stmt => collectFunCalls(stmt))
    )
  }

  private def traverseStmt(s: Statement): Set[ColumnValue] = s.ensureCore match {
    case Assign(names, exp) =>
      if(names.size > 1) {
        (traverseExp(exp).head match {
          case ScalaValue(tup: List[Set[ColumnValue]]) => tup
          case _ => Seq()
        }).zip(names).foreach {
          case (v, name) => callStack.frame().env += name -> v
        }
      } else
        callStack.frame().env += names.head -> traverseExp(exp)
      Set()

    case Yield(exp) =>
      traverseExp(exp)

    case Assert(cond) =>
      val c = traverseExp(cond)
      if(c.isEmpty || c.size > 1)
        throw EndOfTraversalReachedException(s"Failed assertion at $currentLine")
      else c.head match {
        case ScalaValue(true) => Set(ScalaValue(true))
        case _ => throw EndOfTraversalReachedException(s"Failed assertion at $currentLine")
      }

    case Values(name, typ) => // TODO
      Set()

    case FailStatement =>
      throw EndOfTraversalReachedException("Fail statement reached")
  }

  private def traverseExp(e: Expression): Set[ColumnValue] = e.ensureCore match {
    case Var(name) => callStack.frame().env(name)
    case Constant(lit) => Set(ScalaValue(lit))

    case PathAccess(receiver, link) =>
      traverseExp(receiver).flatMap(cv => cv match {
        case uriValue: URIValue => getLinks(uriValue, link)
        case _ => Set()
      })

    case Call(name, args, _) => run(name, args)

    case Cast(src, targetTyp) =>
      if(traverseExp(src).forall(cv => isOfType(cv, targetTyp)))
        traverseExp(src) // TODO
      else Set()

    case InstanceOf(exp, ty) => Set(ScalaValue(traverseExp(exp).forall(cv => isOfType(cv, ty)))) // TODO
    case NotInstanceOf(exp, ty) => Set(ScalaValue(traverseExp(exp).forall(cv => !isOfType(cv, ty))))

    case Eq(lhs, rhs) => Set(ScalaValue(traverseExp(lhs) == traverseExp(rhs)))
    case Neq(lhs, rhs) => Set(ScalaValue(traverseExp(lhs) != traverseExp(rhs)))

    case Def(exp) => Set(ScalaValue(traverseExp(exp).nonEmpty))
    case Undef(exp) => Set(ScalaValue(traverseExp(exp).isEmpty))

    case Count(call) => Set(ScalaValue(traverseExp(call).size)) // TODO
    case Tuple(exps) => Set(ScalaValue(exps.map(e => traverseExp(e))))

    case Aggregate(agg, bodies) => Set() // TODO

    case Wildcard => Set() // CONFIRM
    case Eval(code) => Set() // CONFIRM

    case _ => Set()
  }

  private def isOfType(cv: ColumnValue, ty: Type): Boolean = {
//    println("COMP Type " + (cv match {
//      case u: URIValue => u.types.head.isAssignableFrom(ty))
//      case sv: ScalaValue => sv.v
//    }) + " TO " + ty)
    true // TODO
  }

}
