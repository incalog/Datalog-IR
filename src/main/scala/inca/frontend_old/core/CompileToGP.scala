package inca.frontend_old.core

import inca.backend.ir.GP
import inca.frontend_old.core.CompileToGP.BodyMustFail
import inca.frontend_old.core.tree._
import inca.util.Gensym
import inca.util.Meta.Scala

import scala.collection.mutable.ListBuffer
import scala.meta.{Name => _, Type => _}

object CompileToGP {
  case object BodyMustFail extends Exception
}

class CompileToGP {
  val gensym = new Gensym(Iterable.empty)

  private val generatedPatterns = ListBuffer[GP.Pattern]()

  def transformModule(module: Module): GP.Module = {
    // construct map Name => Fun
    val Module(name, imports, contents) = module
    gensym.register(module.usedModuleNames.map(_.name))
    gensym.register(module.usedDefNames.map(_.name))

    val ScalaModuleContents = ListBuffer[meta.Import]()
    val blockDefs = ListBuffer[meta.Stat]()
    contents.foreach {
      case fun: PatternFunction => generatedPatterns += transform(fun)
      case _: ValDef => // will be inlined
      case ScalaModuleContent(Scala(imp: meta.Import)) => ScalaModuleContents += imp
      case ScalaModuleContent(Scala(stat: meta.Stat)) => blockDefs += stat
    }

    val scalaContent = ScalaModuleContents.toList ++ blockDefs.toList
    GP.Module(name.name, imports.map(_.name.name), generatedPatterns.toList, scalaContent.map(Scala.apply))
  }

  def transform(fun: PatternFunction): GP.Pattern = {
    // TODO meta analysis negation in recusion
    rewriteFunction(fun)
  }

  def transType(typ: Type): GP.Type = typ match {
    case TAny => GP.TAny
    case TLiteral(lit) => GP.TLiteral(lit)
    case TAnyLinked => GP.TAnyLinked
    case TNode(name) => GP.TNode(name)
    case TList(ty) => GP.TList(transType(ty).asInstanceOf[GP.TLinked])
    case TScala(ty) => GP.TScala(ty)
  }


  def rewriteFunction(fun: PatternFunction): GP.Pattern = {
    gensym.register(fun.freeVars.keys.map(_.name))
    gensym.register(fun.boundNames.map(_.name))

    val vis = fun.vis.map {
      case Private => GP.Private
    }

    val params = fun.params.map { param => GP.Param(param.name.name, transType(param.typ)) }
    val outParams = fun.outParams.map { out =>
      val name = gensym.fresh("out")
      GP.Param(name, transType(out))
    }
    val outVars = outParams.map(_.name)

    val bodies = fun.bodies.flatMap(b => transBody(b, outVars)(gensym))
    GP.Pattern(vis, fun.name.name, params ++ outParams, bodies)
  }

  def generateCompareConstraints(comp: GP.Comparator)(lhs: Seq[String], rhs: Seq[String]): Seq[GP.Constraint] = {
    if (lhs.size != rhs.size)
      throw new IllegalArgumentException("Cannot create equalites for different sized variable lists")
    (lhs zip rhs).map { case (l, r) => GP.Compare(comp, GP.Var(l), GP.Var(r)) }
  }

  val genEqs: (Seq[String], Seq[String]) => Seq[GP.Constraint] = generateCompareConstraints(GP.EqComparator)
  val genNeqs: (Seq[String], Seq[String]) => Seq[GP.Constraint] = generateCompareConstraints(GP.NeqComparator)

  type Res = (Seq[String], Seq[GP.Constraint])

  def transBody(alt: Body, outVars: Seq[String])(implicit gensym: Gensym): Option[GP.Body] = {
    try {
      val constraints = alt.stmts.flatMap { s =>
        transStatement(s.ensureCore, outVars)
      }
      Some(GP.Body(constraints))
    } catch {
      case BodyMustFail => None
    }
  }

  def transStatement(stmt: CoreStatement, outVars: Seq[String])(implicit gensym: Gensym): Seq[GP.Constraint] = stmt match {
    case Values(name, typ) =>
      (Seq(GP.HasType(GP.Var(name.name), transType(typ))))

    case assign@Assign(names, exp) =>
      if (exp.typ.isEmpty)
        throw new IllegalArgumentException(s"Cannot compile untyped assignment $stmt")

      gensym.register(names.map(_.name))
      val expTy = exp.typ.get

      if (shouldInlineAssign(assign)) {
        Seq()
      } else {
        val (rvars, rconstraints) = transExp(exp.ensureCore)
        val eqConstraints = genEqs(names.map(_.name), rvars)
        rconstraints ++ eqConstraints
      }

    case Assert(Constant(BooleanLiteral(v))) =>
      if (v) Seq()
      else throw BodyMustFail
    case Assert(cond) => transExp(cond.ensureCore) match {
      case (Nil, cons) =>
        cons
      case (Seq(v), cons) =>
        cons :+ GP.Compare(GP.EqComparator, GP.Var(v), GP.Constant(GP.BooleanLiteral(true)))
    }

    case Yield(exp) =>
      val (vars, constraints) = transExp(exp.ensureCore)
      constraints ++ genEqs(vars, outVars)

    case FailStatement =>
      throw BodyMustFail
  }

  def tryInlineVar(exp: Expression): Expression = exp match {
    case v: Var => v.target.getOrElse(throw new IllegalArgumentException(s"Unbound variable $v")) match {
      case assign@Assign(Seq(_), exp) if shouldInlineAssign(assign) =>
        tryInlineVar(exp)
      case ValDef(_, _, _, exp) =>
        tryInlineVar(exp)
      case _: Param | _: Values | _: Assign => v
      case target => throw new IllegalArgumentException(s"Unknown variable target $target for $v")
    }
    case _ => exp
  }



  def transExp(exp: CoreExpression)(implicit gensym: Gensym): Res = exp match {
    case v@Var(_) =>
      tryInlineVar(v) match {
        case Var(name) => (Seq(name.name), Seq())
        case other => transExp(other.ensureCore)
      }

    case Eq(lhs, rhs) =>
      val (lvars, lconstraints) = transExp(lhs.ensureCore)
      val (rvars, rconstraints) = transExp(rhs.ensureCore)
      val eqConstraints = genEqs(lvars, rvars)
      (Seq(), lconstraints ++ rconstraints ++ eqConstraints)

    case Neq(lhs, rhs) =>
      val (lvars, lconstraints) = transExp(lhs.ensureCore)
      val (rvars, rconstraints) = transExp(rhs.ensureCore)
      val eqConstraints = genNeqs(lvars, rvars)
      (Seq(), lconstraints ++ rconstraints ++ eqConstraints)

    case InstanceOf(exp, typ) =>
      val (vars, constraints) = transExp(exp.ensureCore)
      if (vars.size != 1) throw new IllegalArgumentException("Number of variables of exp of instance of need to be 1")
      (Seq(), constraints :+ GP.HasType(GP.Var(vars.head), transType(typ)))

    case NotInstanceOf(exp, typ) =>
      val (vars, constraints) = transExp(exp.ensureCore)
      if (vars.size != 1) throw new IllegalArgumentException("Number of variables of exp of instance of need to be 1")
      (Seq(), constraints :+ GP.NotHasType(GP.Var(vars.head), transType(typ)))

    case Cast(exp, typ) =>
      val (vars, constraints) = transExp(exp.ensureCore)
      if (vars.size != 1) throw new IllegalArgumentException("Number of variables of exp of instance of need to be 1")
      val v = vars.head
      (Seq(v), constraints :+ GP.HasType(GP.Var(v), transType(typ)))

    case Def(exp) =>
      exp match {
        case call@Call(name, args, transitive) =>
          (Seq(), genDefCallConstraint(call, args, transitive, neg = false))
        case pa: PathAccess =>
          val tmp = gensym.fresh("_")
          (Seq(), transPathAccess(pa, GP.Var(tmp)))
        case _ => throw new IllegalArgumentException(s"Cannot support Def($exp)")
      }

    case Undef(exp) =>
      exp match {
        case call@Call(name, args, transitive) =>
          (Seq(), genDefCallConstraint(call, args, transitive, neg = true))
        case pathAccess@PathAccess(receiver, _) =>
          val (Seq(src), econstraints) = transExp(receiver.ensureCore)
          val srcTy = transType(receiver.typ.getOrElse(throw new IllegalArgumentException(s"Cannot compile path access with untyped receiver $receiver")))
          val path = pathAccess.link match {
            case ParentLink =>
              GP.NoPath(GP.Var(src), srcTy, GP.ParentLink, termIsSource = true)
            case ChildrenLink =>
              GP.NoPath(GP.Var(src), srcTy, GP.ParentLink, termIsSource = false)
            case NextLink =>
              GP.NoPath(GP.Var(src), srcTy, GP.NextLink, termIsSource = true)
            case PreviousLink =>
              GP.NoPath(GP.Var(src), srcTy, GP.NextLink, termIsSource = false)
            case SizeLink =>
              GP.NoPath(GP.Var(src), srcTy, GP.SizeLink, termIsSource = true)
            case NamedLink(field) =>
              val nodeType = receiver.typ match {
                case Some(TNode(name)) => GP.TNode(name)
                case _ => throw new IllegalArgumentException(s"$receiver should have node type, but has ${receiver.typ}")
              }
              GP.NoPath(GP.Var(src), srcTy, GP.NamedLink(nodeType, field.name), termIsSource = true)
          }
          (Seq(), econstraints :+ path)

        case _ => throw new IllegalArgumentException("Cannot support in Undef " + exp)
      }

    case Constant(lit) =>
      transLiteral(lit) match {
        case None => (Seq(), Seq())
        case Some(gplit) =>
          val tmpVar = gensym.fresh("tmp")
          val compare = GP.Compare(GP.EqComparator, GP.Var(tmpVar), GP.Constant(gplit))
          (Seq(tmpVar), Seq(compare))
      }

    case Wildcard =>
      val dummyVar = gensym.fresh("wildcard")
      (Seq(dummyVar), Seq())

    case Tuple(exps) =>
      val (vars, constraints) = exps.map(e => transExp(e.ensureCore)).unzip
      (vars.flatten, constraints.flatten)

    case pa: PathAccess =>
      val trg = gensym.fresh("trg")
      (Seq(trg), transPathAccess(pa, GP.Var(trg)))

    case funcall@Call(name, args, transitive) =>
      val (inVars, outVars, constraints) = transCallArgs(funcall, args)
      val allvars = (inVars ++ outVars).map(GP.Var)
      val call = GP.Call(name.name, allvars, transitive, neg = false)
      (outVars, constraints :+ call)

    case Count(funcall) =>
      val (inVars, outVars, constraints) = transCallArgs(funcall, funcall.args)
      val countVar = gensym.fresh("count")
      val allvars = (inVars ++ outVars).map(GP.Var)
      val countConstraint = GP.Computed(GP.Var(countVar), GP.CountAggregation(funcall.name.name, allvars))
      (Seq(countVar), constraints :+ countConstraint)

    case eval@Eval(code) =>
      import scala.meta._
      val params = eval.params.getOrElse(Seq())

      val evalVar = gensym.fresh("eval")
      val argConstraints = ListBuffer[GP.Constraint]()
      val paramsTyped = params.map { param =>
        param"${Term.Name(param.name.name)}: ${param.typ.get.asScala}"
      }.toList
      val args = params.map { param =>
        param.target.getOrElse(throw new IllegalArgumentException(s"Unbound eval parameter $param")) match {
          case assign@Assign(Seq(_), exp) if shouldInlineAssign(assign) =>
            // inline exp
            val (Seq(arg), cons) = transExp(exp.ensureCore)
            argConstraints ++= cons
            (GP.Var(arg), transType(exp.typ.get))
          case _: Param | _: Values | _: Assign => (GP.Var(param.name.name), transType(param.typ.get))
          case target => throw new IllegalArgumentException(s"Unknown eval param target $target for $param")
        }
      }
      val funCode = q"(..$paramsTyped) => {${code.tree}}"
      val resType = eval.typ.getOrElse(throw new IllegalStateException("untyped Eval"))
      val evalConstraint = GP.Computed(GP.Var(evalVar), GP.Evaluation(args, transType(resType), Scala(funCode)))
      (Seq(evalVar), (argConstraints :+ evalConstraint).toSeq)

    case Aggregate(agg, bodies) =>
      val aggCode = tryInlineVar(agg) match {
        case Eval(code) => code
        case _ => throw new IllegalArgumentException(s"Requires aggregation code, but got $agg")
      }

      val funname = gensym.fresh("AggregateCollection")

      val inVars = bodies.flatMap(_.freeVars).toMap
      val params = inVars.map(kv => Param(kv._1, kv._2.getOrElse(throw new IllegalArgumentException(s"untyped var ${kv._1} in $exp")))).toSeq
      val allvars = params.map(p => GP.Var(p.name.name)) :+ GP.Var(gensym.fresh("aggregand"))

      val resultType = exp.typ.getOrElse(throw new IllegalArgumentException("untyped aggregate"))
      val fun = PatternFunction(None, Name(funname), params, resultType, bodies)
      generatedPatterns += transform(fun)

      val aggregation = GP.CustomAggregation(transType(resultType), aggCode, funname, allvars, allvars.size - 1)
      val resultVar = gensym.fresh("tmp")
      val compare = GP.Computed(GP.Var(resultVar), aggregation)
      (Seq(resultVar), Seq(compare))
  }

  private def shouldInlineAssign(assign: Assign): Boolean =
    assign.names.size == 1 && (assign.exp.typ.contains(TScalaBoolean) || assign.exp.typ.contains(TLiteral.Bool))

  def transCallArgs(call: Call, args: Seq[Expression])(implicit gensym: Gensym): (Seq[String], Seq[String], Seq[GP.Constraint]) = {
    val (vars, constraints) = args.map(e => transExp(e.ensureCore)).unzip
    val outVars = call.target match {
      case Some(fun@PatternFunction(_, _, _, _, _)) => fun.outParams.map { _ =>
        val argVar = gensym.fresh("arg")
        argVar
      }
      case target => throw new IllegalArgumentException(s"Unknown call target $target for $call")
    }
    (vars.flatten, outVars, constraints.flatten)
  }

  def genDefCallConstraint(funcall: Call, args: Seq[Expression], transitive: Boolean, neg: Boolean)(implicit gensym: Gensym): Seq[GP.Constraint] = {
    val (inVars, outVars, constraints) = transCallArgs(funcall, args)
    val allvars = (inVars ++ outVars).map(GP.Var)
    val call = GP.Call(funcall.name.name, allvars, transitive, neg)
    constraints :+ call
  }

  def transPathAccess(pathAccess: PathAccess, trg: GP.Term)(implicit gensym: Gensym): Seq[GP.Constraint] = {
    val receiver = pathAccess.receiver
    val (Seq(src), econstraints) = transExp(receiver.ensureCore)
    val srcTy = transType(receiver.typ.getOrElse(throw new IllegalArgumentException(s"Cannot compile path access with untyped receiver $receiver")))
    val trgTy = transType(pathAccess.typ.getOrElse(throw new IllegalArgumentException(s"Cannot compile untyped $pathAccess")))
    val path = pathAccess.link match {
      case ParentLink =>
        GP.Path(GP.Var(src), srcTy, GP.ParentLink, trg, trgTy)
      case ChildrenLink =>
        GP.Path(trg, trgTy, GP.ParentLink, GP.Var(src), srcTy)
      case NextLink =>
        GP.Path(GP.Var(src), srcTy, GP.NextLink, trg, trgTy)
      case PreviousLink =>
        GP.Path(trg, trgTy, GP.NextLink, GP.Var(src), srcTy)
      case SizeLink =>
        GP.Path(GP.Var(src), srcTy, GP.SizeLink, trg, trgTy)
      case NamedLink(field) =>
        val nodeType = receiver.typ match {
          case Some(TNode(name)) => GP.TNode(name)
          case _ => throw new IllegalArgumentException(s"$receiver should have node type, but has ${receiver.typ}")
        }
        GP.Path(GP.Var(src), srcTy, GP.NamedLink(nodeType, field.name), trg, trgTy)
    }
    econstraints :+ path
  }

  def transLiteral(lit: Literal): Option[GP.Literal] = lit match {
    case UnitLiteral => None
    case IntLiteral(v) => Some(GP.IntLiteral(v))
    case LongLiteral(v) => Some(GP.LongLiteral(v))
    case DoubleLiteral(v) => Some(GP.DoubleLiteral(v))
    case StringLiteral(v) => Some(GP.StringLiteral(v))
    case BooleanLiteral(v) => Some(GP.BooleanLiteral(v))
  }
}
