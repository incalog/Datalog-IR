package inca.debugger

import inca.compiler.CompiledFunModule
import inca.frontend.core.tree._
import inca.runtime.Query.Matcher
import inca.runtime.{Database, DatabaseAccessor}
import truechange.{Link => _, Type => _, _}

import scala.meta.{Pat, Term}
import scala.reflect.runtime.universe
import scala.tools.reflect.ToolBox

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

  private val toolBox = universe.runtimeMirror(getClass.getClassLoader).mkToolBox()

  val matches = new Matches(db, matcher, funName, funParams)
  def allMatches: Seq[Match] = matches.matches

  private var rootFun: (Name, Seq[Set[ColumnValue]]) = (funName, Seq())
  def load(mat: Match): Unit = {
    rootFun = (funName, mat.inputs.map(col => Set(col.value)))
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
    funParams(callStack.currentFun).map(p => (p.name, callStack.frame match {
      case sf: StackFrame => sf.env(p.name)
      case cf: ContainerFrame => cf.params(p.name)
    })).foreach {
      case (p, v) => frame.env += p -> v
    }

    callStack.push(frame)
    callStack.stackFrame.incrementPntr()
    println(currentLine().head)
  }

  private[debugger] def stepIntoFunction(fun: Name, args: Seq[Set[ColumnValue]]): Unit = {
    initFun(fun, args, if(hasMultipleBodies(fun)) -1 else 0)
    stepOver()
  }

  private def finishFrame(retVal: Set[ColumnValue]): Unit = {
    callStack.pop() match {
      case sf: StackFrame => if(sf.parent >= 0)
        callStack.frame(sf.parent) match {
          case pSf: StackFrame => pSf.intermVars += (sf.funName, sf.args, sf.bodyPtr) -> retVal
          case _ =>
        }
      case _ =>
    }
    if(callStack.nonEmpty && !callStack.isStackFrame())
      callStack.pop()
  }

  private def initFun(fun: Name, args: Seq[Set[ColumnValue]], body: Int): Unit = {
    val frame: Frame = if(body >= 0) {
      new StackFrame(callStack.currentAddr, fun, args, body)
    } else {
      new ContainerFrame(callStack.currentAddr, fun, args)
    }

    funParams(fun).zip(args).foreach {
      case (param, arg) => frame match {
        case sf: StackFrame => sf.env += param.name -> arg.filter(cv => isOfType(cv, param.typ))
        case cf: ContainerFrame => cf.params += param.name -> arg.filter(cv => isOfType(cv, param.typ))
      }
    }
    callStack.push(frame)
  }

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
          } else
            throw InvalidPointerException(s"Invalid indices f:${sf.funName} b:${sf.bodyPtr} p:${sf.ptr}")

        case cf: ContainerFrame =>
          Seq(funSigs(cf.funName))
      }
    else Seq(funSigs(funName))
  }

  private def hasMultipleBodies(fun: Name): Boolean = stmts(fun).size > 1


  private def run(fun: Name, args: Seq[Expression]): Set[ColumnValue] = {
    stmts(fun).zipWithIndex.flatMap {
      case (body, i) =>
        val resolvedArgs = args.map(arg => traverseExp(arg))
        callStack.stackFrame.intermVars.getOrElse((fun, resolvedArgs, i), {
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

  private def getNodeInstances(ty: truechange.Type = AnyType): Set[ColumnValue] = {
    db.nodeInstances.get(ty) match {
      case Some(vs) => vs.entries.map(uri => URIValue(uri, db.nodeInstancesByValue(uri).keys.toSeq)).toSet
      case None => Set()
    }
  }

  private def getPrimitiveInstances(litType: Option[LitType] = None): Set[ColumnValue] = litType match {
    case Some(ty) =>
      db.primitiveInstances.get(ty) match {
        case Some(vs) => vs.entries.map(v => ScalaValue(v)).toSet
        case None => Set()
      }
    case None =>
      db.primitiveInstances.values.flatMap(ind => ind.entries.map(v => ScalaValue(v))).toSet
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
          case (v, name) => callStack.stackFrame.env += name -> v
        }
      } else
        callStack.stackFrame.env += names.head -> traverseExp(exp)
      Set()

    case Yield(exp) =>
      traverseExp(exp)

    case Assert(cond) =>
      if(traverseExpFilter(cond))
        Set(ScalaValue(true))
      else
        throw EndOfTraversalReachedException(s"Failed assertion at ${currentLine().head}")

    case Values(name, typ) =>
      val vals: Set[ColumnValue] = typ match {
        case TAny => getNodeInstances() ++ getPrimitiveInstances()
        case TLiteral(litType) => getPrimitiveInstances(Some(litType))
        case TAnyLinked => getNodeInstances()
        case TNode(name) => getNodeInstances(SortType(name))
        case _ => Set()
      }
      callStack.stackFrame.env += name -> vals
      Set()

    case FailStatement =>
      throw EndOfTraversalReachedException("Fail statement reached")
  }

  private def traverseExpFilter(e: Expression): Boolean = e.ensureCore match { // FIXME See notes p. 16
    case Eq(lhs, rhs) =>
      val res = traverseExp(lhs).intersect(traverseExp(rhs))
      println("L: " + traverseExp(lhs) + " R: " + traverseExp(rhs) + " RES " + res)
      lhs match {
        case Var(name) => callStack.stackFrame.env(name) = res
        case _ =>
      }
      rhs match {
        case Var(name) => callStack.stackFrame.env(name) = res
        case _ =>
      }
      res.nonEmpty

    case Neq(lhs, rhs) =>
      val (lExp, rExp) = (traverseExp(lhs), traverseExp(rhs))
      val (lRes, rRes) = (lExp.filter(cv => rExp.excl(cv).nonEmpty), rExp.filter(cv => lExp.excl(cv).nonEmpty))
      lhs match {
        case Var(name) => callStack.stackFrame.env(name) = lRes
        case _ =>
      }
      rhs match {
        case Var(name) => callStack.stackFrame.env(name) = rRes
        case _ =>
      }
      lRes.nonEmpty && rRes.nonEmpty

    case InstanceOf(exp, ty) =>
      val res = traverseExp(exp).filter(cv => isOfType(cv, ty))
      exp match {
        case Var(name) => callStack.stackFrame.env(name) = res
        case _ =>
      }
      res.nonEmpty

    case NotInstanceOf(exp, ty) =>
      val res = traverseExp(exp).filter(cv => !isOfType(cv, ty))
      exp match {
        case Var(name) => callStack.stackFrame.env(name) = res
        case _ =>
      }
      res.nonEmpty

    case _ =>
      val res = traverseExp(e)
      res.nonEmpty && res.forall {
        case ScalaValue(true) => true
        case _ => false
      }
  }

  private def traverseExp(e: Expression): Set[ColumnValue] = e.ensureCore match {
    case Var(name) => callStack.stackFrame.env(name)
    case Constant(lit) => Set(ScalaValue(getLitVal(lit)))

    case PathAccess(receiver, link) =>
      traverseExp(receiver).flatMap(cv => cv match {
        case uriValue: URIValue => getLinks(uriValue, link)
        case _ => Set()
      })

    case Call(name, args, _) => run(name, args)

    case Cast(src, targetTyp) =>
      val res = traverseExp(src)
      if(res.forall(cv => isOfType(cv, targetTyp)))
        res
      else Set()

    case InstanceOf(exp, ty) => Set(ScalaValue(traverseExp(exp).forall(cv => isOfType(cv, ty))))
    case NotInstanceOf(exp, ty) => Set(ScalaValue(traverseExp(exp).forall(cv => !isOfType(cv, ty))))

    case Eq(lhs, rhs) => Set(ScalaValue(traverseExp(lhs) == traverseExp(rhs)))
    case Neq(lhs, rhs) => Set(ScalaValue(traverseExp(lhs) != traverseExp(rhs)))

    case Def(exp) => exp match {
      case c: Call => Set(ScalaValue(traverseExp(c).nonEmpty))
      case pa: PathAccess => Set(ScalaValue(traverseExp(pa).nonEmpty))
    }
    case Undef(exp) => exp match {
      case c: Call => Set(ScalaValue(traverseExp(c).isEmpty))
      case pa: PathAccess => Set(ScalaValue(traverseExp(pa).isEmpty))
    }

    case Count(call) => Set(ScalaValue(traverseExp(call).size))
    case Tuple(exps) => Set(ScalaValue(exps.map(e => traverseExp(e))))

    case Wildcard => getNodeInstances() ++ getPrimitiveInstances()

    case Aggregate(agg, bodies) =>
      val res = bodies.flatMap(
        body => body.stmts.flatMap(stmt => traverseStmt(stmt)).toSet
      )
      Set() // TODO

    case ev@Eval(code) =>
      val params = ev.params.getOrElse(Seq())
      val resArgs = params.map(param => callStack.stackFrame.env.getOrElse(param.name,
        throw UnexpectedVarException(s"Unbound parameter ${param.name.name}")))

      tupleCombinations(resArgs).map(args => runEval(code.syntax, params, args)).toSet

    case _ => Set()
  }

  private def runEval(code: String, params: Seq[EvalParam], args: Seq[ColumnValue]): ScalaValue = {
    val paramBlock = params.zip(args.map {
      case URIValue(uri, _) => uri
      case ScalaValue(v) => v
    }).map {
      case (param, arg) => s"val ${Pat.Var(Term.Name(param.name.name))}: ${param.typ.get.asScala} = $arg"
    }.mkString(";\n")

    ScalaValue(toolBox.eval(toolBox.parse(s"{$paramBlock;\n$code}")))
  }

  private def tupleCombinations(ls: Seq[Set[ColumnValue]]): Seq[Seq[ColumnValue]] = ls match {
    case Nil => Nil :: Nil
    case head :: tail =>
      val rec = tupleCombinations(tail)
      rec.flatMap(r => head.map(cv => cv +: r))
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

  private def getLitVal(sv: Any): Any = sv match {
    case BooleanLiteral(v) => v
    case IntLiteral(v) => v
    case LongLiteral(v) => v
    case DoubleLiteral(v) => v
    case StringLiteral(v) => v
    case UnitLiteral => UnitLiteral
  }

  private def toTType(ty: truechange.Type): Type = ty match {
    case NothingType => TNothing
    case AnyType => TAny
    case SortType(name) => TNode(name)
    case ListType(ty) => TList(TNode(ty.toString))
  }

  private def toTScala(v: Any): TScala = v match {
    case _: Boolean => TScalaBoolean
    case _: Int => TScalaInt
    case _: Long => TScalaLong
    case _: Double => TScalaDouble
    case _: String => TScalaString
    case _: Any => TScalaAny
  }

}

// TODO Warnings instead of some exceptions

