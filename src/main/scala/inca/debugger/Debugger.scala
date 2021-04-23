package inca.debugger

import inca.compiler.CompiledFunModule
import inca.debugger.Util._
import inca.frontend.core.tree._
import inca.runtime.Query.Matcher
import inca.runtime.{Database, DatabaseAccessor}
import truechange.{Link => _, Type => _, _}

import scala.collection.mutable
import scala.meta.{Pat, Term}
import scala.reflect.runtime.universe
import scala.tools.reflect.ToolBox

class Debugger(feed: Database, matcher: Matcher, module: CompiledFunModule) {
  private val toolBox = universe.runtimeMirror(getClass.getClassLoader).mkToolBox()

  private[debugger] val db = new DatabaseAccessor(feed)
  private[debugger] val callStack: CallStack = new CallStack()

  private[debugger] val funName = Name(matcher.getPatternName.replace(module.fun.name + "_", ""))
  private[debugger] val funParams: Map[Name, Seq[Param]] = module.fun.content.map({
    case pf: PatternFunction => (pf.name, pf.params)
  }).toMap

  private var rootFun: (Name, Seq[Set[EnvValue]]) = (funName, Seq())
  val matches = new Matches(db, matcher, funName, funParams)

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


  def allMatches: Seq[Match] = matches.matches

  def load(mat: Match): Unit = {
    rootFun = (funName, mat.inputs.map(col => Set(EnvValue(col.value, Set()))))
  }


  private def hasMultipleBodies(fun: Name): Boolean = stmts(fun).size > 1

  private def currentStatement: Statement = {
    try {
      stmts(callStack.currentFun)(callStack.currentBody)(callStack.currentPtr)
    } catch {
      case _: IndexOutOfBoundsException =>
        throw InvalidPointerException(s"Invalid indices f:${callStack.currentFun} b:${callStack.currentBody} p:${callStack.currentPtr}")
    }
  }

  private[debugger] def currentLine(n: Int = 1): Seq[String] = {
    if(callStack.nonEmpty)
      callStack.frame match {
        case sf: StackFrame =>
          val sz = typedStmts(sf.funName)(sf.bodyPtr).size
          if(sf.ptr >= 0 && sf.ptr < sz) {
            typedStmts(sf.funName)(sf.bodyPtr).slice(
              Math.min(sf.ptr + (n/2) + 1, sz) - n,
              Math.max(0, sf.ptr - ((n-1)/2)) + n
            )
          } else Seq()

        case cf: ContainerFrame =>
          Seq(funSigs(cf.funName))
      }
    else Seq(funSigs(funName))
  }

  private[debugger] def currentFun(): Seq[String] = {
    val fName = if(callStack.nonEmpty) callStack.frame.funName else funName
    val f: ModuleContent = module.fun.content.filter {
      case pf: PatternFunction => pf.name == fName
      case _ => false
    }.head
    f.prettyprint("").split("\n")
  }


  private[debugger] def stepOver(): Unit = {
    if(callStack.isEmpty)
      throw EndOfTraversalReachedException("Not in any function or body to step over")
    if(!callStack.isStackFrame())
      throw MultipleBodiesException(s"Function has ${stmts(callStack.currentFun).size} bodies")

    if(callStack.currentPtr >= 0) {
      val ret = traverseStmt(currentStatement)
      if(currentStatement.isInstanceOf[TerminatorStatement])
        finishFrame(ret)
      else
        callStack.stackFrame.incrementPntr()

    } else callStack.stackFrame.incrementPntr()

    println(currentLine().head)
  }

  private[debugger] def stepInto(): Unit = {
    val calls = if(callStack.nonEmpty) collectFunCalls(currentStatement) else Seq()
    val resolvedCalls = if(callStack.nonEmpty) calls.map {
      case (name, args) => (name, args.map(arg => traverseExp(arg)))
    } else
      Seq(rootFun)

    if(resolvedCalls.isEmpty)
      throw NoFunctionCallsException(s"No function calls on line: ${currentLine().head}")
    else if(resolvedCalls.size > 1)
      throw MultipleFunctionCallsException(s"Found ${calls.size} function calls", calls, resolvedCalls)
    else
      stepIntoFunction(resolvedCalls.head._1, resolvedCalls.head._2)
  }

  private[debugger] def stepOut(): Unit = {
    if(callStack.isEmpty)
      throw EndOfTraversalReachedException("Not in any function or body to step out from")

    if(callStack.isStackFrame()) {
      val remainingStmts: Seq[Statement] = stmts(callStack.currentFun)(callStack.currentBody).drop(callStack.currentPtr)
      val ret = remainingStmts.flatMap(stmt => traverseStmt(stmt)).toSet
      finishFrame(ret)

    } else callStack.pop()
  }

  private[debugger] def stepIntoBody(b: Int): Unit = {
    if(b >= stmts(callStack.currentFun).size)
      throw InvalidCommandException(s"Function only has ${stmts(callStack.currentFun).size} bodies")

    val frame: StackFrame = new StackFrame(callStack.frame.parent, callStack.currentFun, callStack.frame.args, b)
    val resolvedParams = funParams(callStack.currentFun).map(p => (p.name, callStack.frame match {
      case sf: StackFrame => sf.env.getOrElse(p.name, Set())
      case cf: ContainerFrame => cf.params.getOrElse(p.name, Set())
    }))
    resolvedParams.foreach {
      case (p, v) => frame.env += p -> v
    }

    callStack.push(frame)
    callStack.stackFrame.incrementPntr()
    println(currentLine().head)
  }

  private[debugger] def stepIntoFunction(fun: Name, args: Seq[Set[EnvValue]]): Unit = {
    initFun(fun, args, if(hasMultipleBodies(fun)) -1 else 0)
    stepOver()
  }

  private def finishFrame(retVal: Set[EnvValue]): Seq[Set[EnvValue]] = {
    val popped = callStack.pop()
    if(callStack.nonEmpty && !callStack.isStackFrame())
      callStack.pop()

    popped match {
      case sf: StackFrame =>
        val resArgs = sf.env.filter(v => funParams(sf.funName).exists(param => param.name == v._1)).values.toSeq
        if(sf.parent >= 0) {
          callStack.frame(sf.parent) match {
            case pSf: StackFrame => pSf.intermVars += (sf.funName, sf.args, sf.bodyPtr) -> (retVal, resArgs)
            case _ =>
          }
        }
        resArgs
      case _ => Seq()
    }
  }

  private def initFun(fun: Name, args: Seq[Set[EnvValue]], body: Int): Unit = {
    val frame: Frame = if(body >= 0) {
      new StackFrame(callStack.currentAddr, fun, args, body)
    } else {
      new ContainerFrame(callStack.currentAddr, fun, args)
    }

    funParams(fun).zip(args).foreach {
      case (param, arg) => frame match {
        case sf: StackFrame =>
          val filArgs = arg.filter(ev => isOfType(ev.columnValue, param.typ))
          sf.env += param.name -> filArgs
        case cf: ContainerFrame =>
          cf.params += param.name -> arg.filter(ev => isOfType(ev.columnValue, param.typ))
      }
    }
    callStack.push(frame)
  }

  private def run(fun: Name, args: Seq[Expression]): (Set[EnvValue], Seq[Set[EnvValue]]) = {
    val resolvedArgs = args.map(arg => traverseExp(arg))
    val traversedArgs: mutable.Map[Name, Set[EnvValue]] = mutable.Map()
    funParams(fun).foreach(p => traversedArgs += p.name -> Set())

    val runRes = stmts(fun).zipWithIndex.flatMap {
      case (body, i) =>
        val (res, resArgs) = callStack.stackFrame.intermVars.getOrElse((fun, resolvedArgs, i), {
          initFun(fun, resolvedArgs, i)
          val ret = body.flatMap(stmt => traverseStmt(stmt))
          val retArgs = finishFrame(ret.toSet)
          (ret, retArgs)
        })
        val mappedResArgs = funParams(fun).zip(resArgs)
        mappedResArgs.foreach {
          case (p, evs) => traversedArgs(p.name) = traversedArgs(p.name) ++ evs
        }
        res
    }.toSet
    (runRes, traversedArgs.values.toSeq)
  }


  private[debugger] def findRootParent(boundVar: (Name, ColumnValue)): Set[(Name, ColumnValue)] = boundVar match {
    case (name, cv) =>
      val boundVs = callStack.stackFrame.env.getOrElse(name, Set())
      if(boundVs.exists(ev => ev.columnValue == cv)) {
        val filtered = boundVs.filter(ev => ev.columnValue == cv)
        filtered.flatMap(ev =>
          if(ev.parents.isEmpty || ev.parents.forall(p => p == (name, cv))) Set(boundVar)
          else ev.parents.flatMap(b => findRootParent(b))
        )
      } else
        Set(boundVar)
  }

  private def propagateChanges(keep: Set[EnvValue], lose: Set[EnvValue]): Unit = {
    val parentsK = keep.flatMap(ev => ev.parents)
    val parentsL = lose.flatMap(ev => ev.parents)
    val groupedL = parentsL.groupMap(m => m._1) {
      case (_, cv) => cv
    }
    groupedL.foreach {
      case (name, cvs) =>
        val loseCVs = cvs.filter(cv => !parentsK.contains((name, cv)))
        if(callStack.stackFrame.env.contains(name)) {
          val keepVals = callStack.stackFrame.env(name).filter(ev => !loseCVs.contains(ev.columnValue))
          val loseVals = callStack.stackFrame.env(name) -- keepVals
          callStack.stackFrame.env(name) = keepVals
          propagateChanges(keepVals, loseVals)
        }
        propagateChangesDown(name, loseCVs)
    }
  }

  private def propagateChangesDown(name: Name, lose: Set[ColumnValue]): Unit = {
    val changedVars = callStack.stackFrame.env.map {
      case (envName, evs) => envName -> evs.filter(ev =>
        ev.parents.exists {
          case (n, cv) => n == name && !lose.contains(cv)
        } || !ev.parents.exists {
          case (n, _) => n == name
        })
    }.filter { case (n, evs) => callStack.stackFrame.env(n) != evs }

    changedVars.foreach {
      case (envName, evs) =>
        val loseVals = callStack.stackFrame.env(envName) -- evs
        callStack.stackFrame.env(envName) = evs
        propagateChangesDown(envName, loseVals.map(ev => ev.columnValue))

        val loseLinks = loseVals.map(ev => EnvValue(ev.columnValue, ev.parents.filterNot {
          case (n, cv) => n == name && lose.contains(cv)
        }))
        val keepLinks = evs.map(ev => EnvValue(ev.columnValue, ev.parents))
        propagateChanges(keepLinks, loseLinks)
    }
  }

  private def traverseStmt(s: Statement): Set[EnvValue] = s.ensureCore match {
    case Assign(names, exp) =>
      if(names.size > 1) {
        val res = traverseExp(exp)
        val unwrappedRes =
          if(res.isEmpty) names.map(_ => Set[EnvValue]())
          else res.head.columnValue match {
            case ScalaValue(tup: List[Set[EnvValue]]) => tup
            case _ => Seq()
          }
        names.zip(unwrappedRes).foreach {
          case (name, res) =>
            callStack.stackFrame.env += name -> res
        }
      } else {
        val res = traverseExp(exp)
        callStack.stackFrame.env += names.head -> res
      }
      Set()

    case Yield(exp) =>
      val res = traverseExp(exp)
      res.map(ev => ev.columnValue match {
        case ScalaValue(tup: List[Set[EnvValue]]) =>
          val res = tup.map(evs => evs.map(ev => EnvValue(ev.columnValue, ev.parents.flatMap(findRootParent))))
          EnvValue(ScalaValue(res), Set())
        case _ =>
          EnvValue(ev.columnValue, ev.parents.flatMap(findRootParent))
      })

    case Assert(cond) =>
      val res = traverseExp(cond)
      val passed = res.filter(ev => ev.columnValue == ScalaValue(true))
      propagateChanges(passed, res -- passed)
      if(passed.isEmpty && res.nonEmpty && currentLine().nonEmpty)
        println(s"Failed assertion at ${currentLine().head}")
      Set()

    case Values(name, typ) =>
      val vals: Set[ColumnValue] = typ match {
        case TAny => getNodeInstances(db) ++ getPrimitiveInstances(db)
        case TLiteral(litType) => getPrimitiveInstances(db, Some(litType))
        case TAnyLinked => getNodeInstances(db)
        case TNode(name) => getNodeInstances(db, SortType(name))
        case _ => Set()
      }
      callStack.stackFrame.env += name -> vals.map(v => EnvValue(v, Set()))
      Set()

    case FailStatement =>
      throw EndOfTraversalReachedException("Fail statement reached")
  }

  private def traverseExp(e: Expression): Set[EnvValue] = e.ensureCore match {
    case Var(name) => callStack.stackFrame.env(name).map(ev => EnvValue(ev.columnValue, Set((name, ev.columnValue))))
    case Constant(lit) => Set(ScalaValue(getLitVal(lit))).map {
      cv: ColumnValue => EnvValue(cv, Set())
    }

    case PathAccess(receiver, link) =>
      val recv = traverseExp(receiver)
      val res = recv.flatMap(envVal => envVal.columnValue match {
        case uriVal: URIValue => getLinks(db, uriVal, link).map(cv => EnvValue(cv, envVal.parents))
        case _ => Set()
      })
      propagateChanges(res, recv -- res)
      res

    case Call(name, args, _) =>
      val befArgs = args.map(traverseExp)
      val (res, resArgs) = run(name, args)
      befArgs.zip(resArgs).foreach(arg => propagateChanges(arg._2, arg._1 -- arg._2))
      res

    case Cast(src, targetTyp) =>
      val tSrc = traverseExp(src)
      val passed = tSrc.filter(ev => isOfType(ev.columnValue, targetTyp))
      propagateChanges(passed, tSrc -- passed)
      passed

    case InstanceOf(exp, ty) =>
      val tExp = traverseExp(exp)
      val res = tExp.map(ev => EnvValue(ScalaValue(isOfType(ev.columnValue, ty)), ev.parents))
      res.groupBy(ev => ev.columnValue).map {
        case (cv, evs) => EnvValue(cv, evs.flatMap(ev => ev.parents))
      }.toSet

    case NotInstanceOf(exp, ty) =>
      val tExp = traverseExp(exp)
      val res = tExp.map(ev => EnvValue(ScalaValue(!isOfType(ev.columnValue, ty)), ev.parents))
      res.groupBy(ev => ev.columnValue).map {
        case (cv, evs) => EnvValue(cv, evs.flatMap(ev => ev.parents))
      }.toSet

    case Eq(lhs, rhs) =>
      val (lExp, rExp) = (traverseExp(lhs), traverseExp(rhs))
      val lRes = lExp.map(lEv => EnvValue(ScalaValue(rExp.exists(rEv => rEv.columnValue == lEv.columnValue)), lEv.parents))
      val rRes = rExp.map(rEv => EnvValue(ScalaValue(lExp.exists(lEv => lEv.columnValue == rEv.columnValue)), rEv.parents))
      (lRes ++ rRes).groupBy(ev => ev.columnValue).map {
        case (cv, evs) => EnvValue(cv, evs.flatMap(ev => ev.parents))
      }.toSet

    case Neq(lhs, rhs) =>
      val (lExp, rExp) = (traverseExp(lhs), traverseExp(rhs))
      val lRes = lExp.map(lEv => EnvValue(ScalaValue(rExp.exists(rEv => rEv.columnValue != lEv.columnValue)), lEv.parents))
      val rRes = rExp.map(rEv => EnvValue(ScalaValue(lExp.exists(lEv => lEv.columnValue != rEv.columnValue)), rEv.parents))
      (lRes ++ rRes).groupBy(ev => ev.columnValue).map {
        case (cv, evs) => EnvValue(cv, evs.flatMap(ev => ev.parents))
      }.toSet

    case Def(exp) => exp match {
      case Call(name, args, _) =>
        val beforeArgs = args.map(traverseExp)
        val (_, afterArgs) = run(name, args)
        val grouped = beforeArgs.zip(afterArgs)
        val res = grouped.flatMap {
          case (bef, aft) => bef.map(arg => {
            val keep = aft.exists(a => a.columnValue == arg.columnValue)
            EnvValue(ScalaValue(keep), arg.parents)
          })
        }
        res.groupBy(ev => ev.columnValue).map {
          case (cv, evs) => EnvValue(cv, evs.flatMap(ev => ev.parents).toSet)
        }.toSet

      case pa@PathAccess(receiver, _) =>
        val rec = traverseExp(receiver)
        val tExp = traverseExp(pa)
        val res = rec.map(ev => {
          val keep = tExp.exists(e => e.parents.exists(p => p._2 == ev.columnValue))
          EnvValue(ScalaValue(keep), ev.parents)
        })
        res.groupBy(ev => ev.columnValue).map {
          case (cv, evs) => EnvValue(cv, evs.flatMap(ev => ev.parents))
        }.toSet
    }

    case Undef(exp) => exp match {
      case Call(name, args, _) =>
        val beforeArgs = args.map(traverseExp)
        val (_, afterArgs) = run(name, args)
        val grouped = beforeArgs.zip(afterArgs)
        val res = grouped.flatMap {
          case gr@(bef, aft) => bef.map(arg => {
            val keep = !aft.exists(a => a.columnValue == arg.columnValue) ||
              grouped.filter(g => g != gr).exists(g => g._1 != g._2)

            EnvValue(ScalaValue(keep), arg.parents)
          })
        }
        res.groupBy(ev => ev.columnValue).map {
          case (cv, evs) => EnvValue(cv, evs.flatMap(ev => ev.parents).toSet)
        }.toSet

      case PathAccess(receiver, link) =>
        val rec = traverseExp(receiver)
        val paRes = rec.flatMap(envVal => envVal.columnValue match {
          case uriVal: URIValue => getLinks(db, uriVal, link).map(cv => EnvValue(cv, envVal.parents))
          case _ => Set()
        })
        val res = rec.map(ev => {
          val keep = !paRes.exists(e => e.parents.exists(p => p._2 == ev.columnValue))
          EnvValue(ScalaValue(keep), ev.parents)
        })
        res.groupBy(ev => ev.columnValue).map {
          case (cv, evs) => EnvValue(cv, evs.flatMap(ev => ev.parents))
        }.toSet
    }

    case Count(call) =>
      val funArgs = funParams(call.name).zip(call.args.map(traverseExp))
      val countPs = funArgs.flatMap {
        case (param, evs) => evs.map(ev => (param.name, ev.columnValue))
      }.toSet
      val (tc, _) = run(call.name, call.args)
      Set(EnvValue(ScalaValue(tc.size), countPs))

    case Tuple(exps) =>
      val res = exps.map(e => traverseExp(e))
      Set(EnvValue(ScalaValue(res), Set()))

    case Wildcard =>
      (getNodeInstances(db) ++ getPrimitiveInstances(db)).map(cv => EnvValue(cv, Set()))

    case Aggregate(agg, bodies) =>
      val res = bodies.flatMap(
        body => body.stmts.flatMap(stmt => traverseStmt(stmt)).toSet
      )
      Set() // TODO

    case ev@Eval(code) =>
      val params = ev.params.getOrElse(Seq())
      val resArgs = params.map(param => callStack.stackFrame.env.getOrElse(param.name,
        throw UnexpectedVarException(s"Unbound parameter ${param.name.name}")))

      val possibleTuples = tupleCombinations(params.map(p => p.name).zip(resArgs))
      possibleTuples.map(args => runEval(code.syntax, params, args.map(arg => arg._2))).toSet

    case _ => Set()
  }

  private def runEval(code: String, params: Seq[EvalParam], args: Seq[EnvValue]): EnvValue = {
    val unwrappedArgs = args.map(arg => arg.columnValue).map {
      case URIValue(uri, _) => uri
      case ScalaValue(v) => v
    }
    val paramBlock = params.zip(unwrappedArgs).map {
      case (param, arg) => s"val ${Pat.Var(Term.Name(param.name.name))}: ${param.typ.get.asScala} = $arg"
    }.mkString(";\n")

    val res = toolBox.eval(toolBox.parse(s"{$paramBlock;\n$code}"))
    EnvValue(ScalaValue(res), params.zip(args).map(p => (p._1.name, p._2.columnValue)).toSet)
  }

  private[debugger] def tupleCombinations(ls: Seq[(Name, Set[EnvValue])]): Seq[Seq[(Name, EnvValue)]] = ls match {
    case Nil => Nil :: Nil
    case (hName, hEvs) :: t =>
      val tRes = tupleCombinations(t)
      tRes.flatMap(tTup => {
        val hFil = hEvs.filter(hEv => {
          val pH = findRootParent((hName, hEv.columnValue))
          tTup.forall {
            case (tName, tEv) =>
              val pT = findRootParent((tName, tEv.columnValue))
              pT.map(p => p._1) != pH.map(p => p._1)
          } || tTup.forall {
            case (_, tEv) => tEv.columnValue == hEv.columnValue
          }
        })
        hFil.map(hEv => (hName, hEv) +: tTup)
      })
  }


  private def isOfType(cv: ColumnValue, ty: Type): Boolean = cv match {
    case URIValue(_, types) => types.map(toTType).contains(ty)
    case ScalaValue(sv) =>
      ty match {
        case TAny => true
        case TLiteral(litType) => litType.accepts(getLitVal(sv))
        case TScala(_) => ty == toTScala(getLitVal(sv))
        case _ => false
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
}
