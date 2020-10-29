package inca.frontend.core

import inca.backend.ir.GP
import inca.frontend.core.Core.{Name, TTuple}
import inca.util.Gensym

import scala.meta._

object CompileToGP {

  case object BodyMustFail extends Exception


  type FunEnv = Map[String, Core.PatternFunction]

  def transformModule(module: Core.Module): GP.Module = {
    // construct map Name => Fun
    val funs = module.funs.map { fun => fun.name.name -> fun }.toMap
    val patterns = module.funs.map { fun => transform(fun, funs) }
    GP.Module(module.name.name, module.imports.map(_.name), patterns, module.stats)
  }

  def transform(fun: Core.PatternFunction, funs: FunEnv): GP.Pattern = {
    // TODO meta analysis negation in recusion
    rewriteFunction(fun, funs)
  }

  def transType(typ: Core.TypeAnno): GP.TypeAnno = typ match {
    case Core.TAny => GP.TAny
    case Core.TBool => GP.TBool
    case Core.TInt => GP.TInt
    case Core.TLong => GP.TLong
    case Core.TDouble => GP.TDouble
    case Core.TString => GP.TString
    case Core.TAnyLinked => GP.TAnyLinked
    case Core.TNode(name) => GP.TNode(name)
    case Core.TList(ty) => GP.TList(transType(ty).asInstanceOf[GP.TLinked])
    case dt: Core.DataType => GP.TDataType(resolveDataType(dt))
  }


  def rewriteFunction(fun: Core.PatternFunction, funs: FunEnv): GP.Pattern = {
    val gensym = new Gensym(fun.freeVars.keys.map(_.name))
    gensym.register(fun.boundNames.map(_.name))

    val vis = fun.vis.map {
      case Core.Private => GP.Private
      case Core.Public => GP.Public
    }

    val params = fun.params.map { param => GP.Param(param.name.name, transType(param.typ)) }
    val env = fun.params.map {
      case Core.Param(name, ty) => name -> Binding(ty, None, None)
    }.toMap

    val outParams = fun.outParams.map { param =>
      val name =
        if (param.name.isDefined) param.name.get.name
        else gensym.fresh("out")
      GP.Param(name, transType(param.typ))
    }
    val outVars = outParams.map(_.name)

    val bodies = fun.bodies.flatMap(b => transBody(b, funs, outVars, env)(gensym))
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

  case class Binding(typ: Core.TypeAnno, exp: Option[Core.CoreExp], index: Option[Int]) {
    def shouldInline: Boolean = typ match {
      case Core.TBool => exp.nonEmpty && index.isEmpty
      case _ => false
    }
  }

  type Env = Map[Core.Name, Binding]


  def transBody(alt: Core.Body, funs: FunEnv, outVars: Seq[String], env: Env)(implicit gensym: Gensym): Option[GP.Body] = gensym.scoped {
    try {
      var currentEnv: Env = env
      val constraints = alt.stmts.flatMap { s =>
        val (cons, newenv) = transStatement(s.ensureCore, funs, outVars, currentEnv)
        currentEnv = newenv
        cons
      }
      Some(GP.Body(constraints))
    } catch {
      case BodyMustFail => None
    }
  }

  def transStatement(stmt: Core.CoreStatement, funs: FunEnv, outVars: Seq[String], env: Env)(implicit gensym: Gensym): (Seq[GP.Constraint], Env) = stmt match {
    case Core.Values(name, typ) =>
      if (env.contains(name))
        throw new IllegalArgumentException(s"Program tries to rebind $name in $stmt")
      val binding = Binding(typ, None, None)
      val newenv = env + (name -> binding)
      (Seq(GP.HasType(GP.Var(name.name), transType(typ))), newenv)

    case Core.Assign(names, exp) =>
      names.foreach { n =>
        if (env.contains(n))
          throw new IllegalArgumentException(s"Program tries to rebind $n in $stmt")
      }
      if (exp.typ.isEmpty)
        throw new IllegalArgumentException(s"Cannot compile untyped assignment $stmt")

      gensym.register(names.map(_.name))
      val expTy = exp.typ.get
      val bindings = names match {
        case Nil => Seq()
        case Seq(_) => Seq(Binding(expTy, Some(exp.ensureCore), None))
        case ns => ns.zipWithIndex.map { case (n, i) =>
          val ty = expTy match {
            case tup@Core.TTuple(ts) =>
              if (i < ts.size)
                ts(i)
              else
                throw new IllegalArgumentException(s"Cannot assign ${ts.size}-ary tuple $tup to ${ns.size} variables $ns")
            case _ => throw new IllegalArgumentException(s"Cannot assign non-tuple $expTy to variables $ns")
          }
          Binding(ty, Some(exp.ensureCore), Some(i))
        }
      }

      val shouldInline = bindings.forall(_.shouldInline)
      val newenv = env ++ names.zip(bindings)
      if (shouldInline) {
        (Seq(), newenv)
      } else {
        val (rvars, rconstraints) = transExp(exp.ensureCore)(funs, env, gensym)
        val eqConstraints = genEqs(names.map(_.name), rvars)
        (rconstraints ++ eqConstraints, newenv)
      }

    case Core.Assert(Core.Constant(Core.BooleanLiteral(v))) =>
      if (v) (Seq(), env)
      else throw BodyMustFail
    case Core.Assert(cond) => transExp(cond.ensureCore)(funs, env, gensym) match {
      case (Nil, cons) =>
        (cons, env)
      case (Seq(v), cons) =>
        (cons :+ GP.Compare(GP.EqComparator, GP.Var(v), GP.Constant(GP.BooleanLiteral(true))), env)
    }

    case Core.Yield(exp) =>
      val (vars, constraints) = transExp(exp.ensureCore)(funs, env, gensym)
      (constraints ++ genEqs(vars, outVars), env)

    case Core.Fail =>
      throw BodyMustFail
  }


  def transExp(cond: Core.CoreExp)(implicit funs: FunEnv, env: Env, gensym: Gensym): Res = cond match {
    case Core.Var(name) =>
      env.get(name) match {
        case Some(binding) =>
          if (binding.shouldInline)
            transExp(binding.exp.get)
          else
            (Seq(name.name), Seq())
        case None =>
          throw new IllegalArgumentException(s"Unbound variable $name")
      }

    case Core.Eq(lhs, rhs) =>
      val (lvars, lconstraints) = transExp(lhs.ensureCore)
      val (rvars, rconstraints) = transExp(rhs.ensureCore)
      val eqConstraints = genEqs(lvars, rvars)
      (Seq(), lconstraints ++ rconstraints ++ eqConstraints)

    case Core.Neq(lhs, rhs) =>
      val (lvars, lconstraints) = transExp(lhs.ensureCore)
      val (rvars, rconstraints) = transExp(rhs.ensureCore)
      val eqConstraints = genNeqs(lvars, rvars)
      (Seq(), lconstraints ++ rconstraints ++ eqConstraints)

    case Core.InstanceOf(exp, typ) =>
      val (vars, constraints) = transExp(exp.ensureCore)
      if (vars.size != 1) throw new IllegalArgumentException("Number of variables of exp of instance of need to be 1")
      (Seq(), constraints :+ GP.HasType(GP.Var(vars.head), transType(typ)))

    case Core.NotInstanceOf(exp, typ) =>
      val (vars, constraints) = transExp(exp.ensureCore)
      if (vars.size != 1) throw new IllegalArgumentException("Number of variables of exp of instance of need to be 1")
      (Seq(), constraints :+ GP.NotHasType(GP.Var(vars.head), transType(typ)))

    case Core.Cast(exp, typ) =>
      val (vars, constraints) = transExp(exp.ensureCore)
      if (vars.size != 1) throw new IllegalArgumentException("Number of variables of exp of instance of need to be 1")
      val v = vars.head
      (Seq(v), constraints :+ GP.HasType(GP.Var(v), transType(typ)))

    case Core.Def(exp) =>
      exp match {
        case Core.Call(name, args, transitive) =>
          (Seq(), genDefCallConstraint(name, args, transitive, neg = false))
        case pa: Core.PathAccess =>
          val tmp = gensym.fresh("_")
          (Seq(), transPathAccess(pa, GP.Var(tmp)))
        case _ => throw new IllegalArgumentException(s"Cannot support Def($exp)")
      }

    case Core.Undef(exp) =>
      exp match {
        case Core.Call(name, args, transitive) =>
          (Seq(), genDefCallConstraint(name, args, transitive, neg = true))
        case pathAccess@Core.PathAccess(receiver, _) =>
          val (Seq(src), econstraints) = transExp(receiver.ensureCore)
          val srcTy = transType(receiver.typ.getOrElse(throw new IllegalArgumentException(s"Cannot compile path access with untyped receiver $receiver")))
          val path = pathAccess.link match {
            case Core.ParentLink =>
              GP.NoPath(GP.Var(src), srcTy, GP.ParentLink, termIsSource = true)
            case Core.ChildrenLink =>
              GP.NoPath(GP.Var(src), srcTy, GP.ParentLink, termIsSource = false)
            case Core.NextLink =>
              GP.NoPath(GP.Var(src), srcTy, GP.NextLink, termIsSource = true)
            case Core.PreviousLink =>
              GP.NoPath(GP.Var(src), srcTy, GP.NextLink, termIsSource = false)
            case Core.SizeLink =>
              GP.NoPath(GP.Var(src), srcTy, GP.SizeLink, termIsSource = true)
            case Core.NamedLink(field) =>
              val nodeType = receiver.typ match {
                case Some(Core.TNode(name)) => GP.TNode(name)
                case _ => throw new IllegalArgumentException(s"$receiver should have node type, but has ${receiver.typ}")
              }
              GP.NoPath(GP.Var(src), srcTy, GP.NamedLink(nodeType, field.name), termIsSource = true)
          }
          (Seq(), econstraints :+ path)

        case _ => throw new IllegalArgumentException("Cannot support in Undef " + exp)
      }

    case Core.Constant(lit) =>
      transLiteral(lit) match {
        case None => (Seq(), Seq())
        case Some(gplit) =>
          val tmpVar = gensym.fresh("tmp")
          val compare = GP.Compare(GP.EqComparator, GP.Var(tmpVar), GP.Constant(gplit))
          (Seq(tmpVar), Seq(compare))
      }

    case Core.Wildcard =>
      val dummyVar = gensym.fresh("wildcard")
      (Seq(dummyVar), Seq())

    case Core.Tuple(exps) =>
      val (vars, constraints) = exps.map(e => transExp(e.ensureCore)).unzip
      (vars.flatten, constraints.flatten)

    case pa: Core.PathAccess =>
      val trg = gensym.fresh("trg")
      (Seq(trg), transPathAccess(pa, GP.Var(trg)))

    case Core.Call(name, args, transitive) =>
      // TODO why is there a distinction between exp and non exp args in MPS impl?
      val (inVars, outVars, constraints) = transCallArgs(name.name, args)
      val allvars = (inVars ++ outVars).map(GP.Var)
      val call = GP.Call(name.name, allvars, transitive, neg = false)
      (outVars, constraints :+ call)

    case Core.Count(call) =>
      val (inVars, outVars, constraints) = transCallArgs(call.name.name, call.args)
      val countVar = gensym.fresh("count")
      val allvars = (inVars ++ outVars).map(GP.Var)
      val countConstraint = GP.Computed(GP.Var(countVar), GP.CountAggregation(call.name.name, allvars))
      (Seq(countVar), constraints :+ countConstraint)

    case eval@Core.Eval(params, code) =>
      import scala.meta._

      val evalVar = gensym.fresh("eval")
      var argConstraints = Seq[GP.Constraint]()
      val paramsBindings = params.map(name => name -> env.getOrElse(name, throw new IllegalArgumentException(s"Unbound variable $name")))
      val paramsTyped = paramsBindings.map { case (name, bind) =>
        param"${Term.Name(name.name)}: ${scalaTypeAnno(bind.typ)}"
      }.toList
      val args = paramsBindings.map { case (name, binding) =>
        if (binding.shouldInline) {
          val (Seq(arg), cons) = transExp(binding.exp.get)
          argConstraints ++= cons
          (GP.Var(arg), transType(binding.typ))
        } else {
          (GP.Var(name.name), transType(binding.typ))
        }
      }
      val funCode = q"(..$paramsTyped) => {$code}"
      val resType = eval.typ.getOrElse(throw new IllegalStateException("untyped Eval"))
      val evalConstraint = GP.Computed(GP.Var(evalVar), GP.Evaluation(args, transType(resType), funCode))
      (Seq(evalVar), Seq(evalConstraint))

    case Core.Aggregate(init, join, unjoin, call) =>
      if (!join.isAssociative || !join.isCommutative)
        throw new IllegalArgumentException(s"Can only compile aggregations with join operators that are associative and commutative")

      val resultType = call.typ.getOrElse(throw new IllegalArgumentException(s"Cannot compile aggregation over untyped $call"))
      resultType match {
        case TTuple(ts) if ts.isEmpty => throw new IllegalArgumentException(s"Cannot aggregate over functions with Unit result type")
        case TTuple(ts) if ts.size > 1 => throw new IllegalArgumentException(s"Cannot aggregate over functions with multiple results $ts")
        case _ => // nothing
      }

      val (inVars, Seq(outVar), constraints) = transCallArgs(call.name.name, call.args)
      val allvars = (inVars :+ outVar).map(GP.Var)
      val initOp = resolveDataOp(init)
      val joinOp = resolveDataOp(join)
      val invOp = unjoin.map(resolveDataOp)
      val aggregation = GP.CustomAggregation(transType(resultType), initOp, joinOp, invOp, call.name.name, allvars, allvars.size - 1)

      val resultVar = gensym.fresh("tmp")
      val compare = GP.Computed(GP.Var(resultVar), aggregation)
      (Seq(resultVar), constraints :+ compare)
  }

  def transCallArgs(name: String, args: Seq[Core.Exp])(implicit funs: FunEnv, env: Env, gensym: Gensym): (Seq[String], Seq[String], Seq[GP.Constraint]) = {
    val (vars, constraints) = args.map(e => transExp(e.ensureCore)).unzip
    val outVars = funs(name).outParams.map { _ =>
      val argVar = gensym.fresh("arg")
      argVar
    }
    (vars.flatten, outVars, constraints.flatten)
  }

  def genDefCallConstraint(name: Core.Name, args: Seq[Core.Exp], transitive: Boolean, neg: Boolean)(implicit funs: FunEnv, env: Env, gensym: Gensym): Seq[GP.Constraint] = {
    val (vars, constraint) = args.map {
      case Core.Var(name) => (Seq(name.name), Seq())
      case arg =>
        val (vars, constraints) = transExp(arg.ensureCore)
        if (vars.size > 1)
          throw new IllegalArgumentException("More than one result variable for one argument " + arg)
        (vars, constraints)
    }.unzip

    val stillReq = (funs(name.name).params ++ funs(name.name).outParams).size - vars.flatten.size
    val tempVars = for (i <- 0 until stillReq) yield {
      GP.Var(gensym.fresh("arg"))
    }
    val compositionConstraint = GP.Call(name.name, vars.flatten.map(GP.Var) ++ tempVars, transitive, neg)
    constraint.flatten :+ compositionConstraint
  }

  def transPathAccess(pathAccess: Core.PathAccess, trg: GP.Term)(implicit funs: FunEnv, env: Env, gensym: Gensym): Seq[GP.Constraint] = {
    val receiver = pathAccess.receiver
    val (Seq(src), econstraints) = transExp(receiver.ensureCore)
    val srcTy = transType(receiver.typ.getOrElse(throw new IllegalArgumentException(s"Cannot compile path access with untyped receiver $receiver")))
    val trgTy = transType(pathAccess.typ.getOrElse(throw new IllegalArgumentException(s"Cannot compile untyped $pathAccess")))
    val path = pathAccess.link match {
      case Core.ParentLink =>
        GP.Path(GP.Var(src), srcTy, GP.ParentLink, trg, trgTy)
      case Core.ChildrenLink =>
        GP.Path(trg, trgTy, GP.ParentLink, GP.Var(src), srcTy)
      case Core.NextLink =>
        GP.Path(GP.Var(src), srcTy, GP.NextLink, trg, trgTy)
      case Core.PreviousLink =>
        GP.Path(trg, trgTy, GP.NextLink, GP.Var(src), srcTy)
      case Core.SizeLink =>
        GP.Path(GP.Var(src), srcTy, GP.SizeLink, trg, trgTy)
      case Core.NamedLink(field) =>
        val nodeType = receiver.typ match {
          case Some(Core.TNode(name)) => GP.TNode(name)
          case _ => throw new IllegalArgumentException(s"$receiver should have node type, but has ${receiver.typ}")
        }
        GP.Path(GP.Var(src), srcTy, GP.NamedLink(nodeType, field.name), trg, trgTy)
    }
    econstraints :+ path
  }

  def transLiteral(lit: Core.Literal): Option[GP.Literal] = lit match {
    case Core.UnitLiteral => None
    case Core.IntLiteral(v) => Some(GP.IntLiteral(v))
    case Core.LongLiteral(v) => Some(GP.LongLiteral(v))
    case Core.DoubleLiteral(v) => Some(GP.DoubleLiteral(v))
    case Core.StringLiteral(v) => Some(GP.StringLiteral(v))
    case Core.BooleanLiteral(v) => Some(GP.BooleanLiteral(v))
  }

  def scalaAnnoString(typ: Core.TypeAnno): String = typ match {
    case Core.TAny => "Any"
    case Core.TBool => "Boolean"
    case Core.TInt => "Int"
    case Core.TLong => "Long"
    case Core.TDouble => "Double"
    case Core.TString => "String"
    case dt: Core.DataType => resolveDataType(dt)
    case _: Core.TLinked => "truechange.URI"
  }

  def scalaTypeAnno(typ: Core.TypeAnno): meta.Type = typ match {
    case Core.TAny => t"Any"
    case Core.TBool => t"Boolean"
    case Core.TInt => t"Int"
    case Core.TLong => t"Long"
    case Core.TDouble => t"Double"
    case Core.TString => t"String"
    case dt: Core.DataType => t"${resolveDataType(dt)}"
    case _: Core.TLinked => t"truechange.URI"
  }

  def resolveDataOp(op: Core.DataOp): String = op.qualifier match {
    case Some(Name(q)) if q.isEmpty => op.operation.name
    case Some(q) => s"$q.${op.operation}"
    case None => throw new IllegalArgumentException(s"TODO resolve unqualified data op calls")
  }

  def resolveDataType(dt: Core.DataType): String = dt.qualifier match {
    case Some(Name(q)) if q.isEmpty => dt.name.name
    case Some(q) => s"$q.${dt.name}"
    case None => throw new IllegalArgumentException(s"TODO resolve unqualified data types")
  }
}
