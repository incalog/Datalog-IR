package inca.frontend.fun

import inca.backend.ir.GP
import inca.util.Gensym

object CompileToGP {

  case object BodyMustFail extends Exception


  type FunEnv = Map[String, Fun.PatternFunction]

  def transformModule(module: Fun.Module): GP.Module = {
    val undefs = CollectUndefPaths.transModule(module).toSet
    val ninsts = CollectNotInstanceOfTypes.transModule(module).toSet
    // generate helpers
    val helpers = undefs.map(genUndefPathHelper) ++ ninsts.map(genNotInstanceOfHelper)
    // construct map Name => Fun
    val funs = module.funs.map { fun => fun.name -> fun}.toMap
    val patterns = (module.funs ++ helpers).map { fun => transform(fun, funs) }
    GP.Module(module.name, module.imports, patterns)
  }

  def transform(fun: Fun.PatternFunction, funs: FunEnv): GP.Pattern = {
    // TODO meta analysis negation in recusion
    rewriteFunction(fun, funs)
  }

  def transType(typ: Fun.TypeAnno): GP.TypeAnno = typ match {
    case Fun.TBool => GP.TBool
    case Fun.TInt => GP.TInt
    case Fun.TLong => GP.TLong
    case Fun.TDouble => GP.TDouble
    case Fun.TString => GP.TString
    case Fun.TAnyLinked => GP.TAnyLinked
    case Fun.TNode(name) => GP.TNode(name)
    case Fun.TList(ty) => GP.TList(transType(ty).asInstanceOf[GP.TLinked])
  }



  def rewriteFunction(fun: Fun.PatternFunction, funs: FunEnv): GP.Pattern = {
    val gensym = new Gensym(fun.freeVars.keys)
    gensym.register(fun.boundNames)

    val vis = fun.vis.map {
      case Fun.Private => GP.Private
      case Fun.Public => GP.Public
    }

    val params = fun.params.map { param => GP.Param(param.name, param.typ.map(transType)) }
    val env = fun.params.map {
      case Fun.Param(name, ty) => name -> Binding(ty.getOrElse(Fun.TAnyLinked), None, None)
    }.toMap

    val outParams = fun.outParams.map { param =>
      val name =
        if (param.name.isDefined) param.name.get
        else gensym.fresh("out")
      GP.Param(name, Some(transType(param.typ)))
    }
    val outVars = outParams.map(_.name)

    val bodies = fun.bodies.flatMap(b => transBody(b, funs, outVars, env)(gensym))
    GP.Pattern(vis, fun.name, params ++ outParams, bodies)
  }

  def generateCompareConstraints(comp: GP.Comparator)(lhs: Seq[String], rhs: Seq[String]): Seq[GP.Constraint] = {
    if (lhs.size != rhs.size)
      throw new IllegalArgumentException("Cannot create equalites for different sized variable lists")
    (lhs zip rhs).map{ case (l,r) => GP.Compare(comp, GP.Var(l), GP.Var(r)) }
  }

  val genEqs: (Seq[String], Seq[String]) => Seq[GP.Constraint] = generateCompareConstraints(GP.EqComparator)
  val genNeqs: (Seq[String], Seq[String]) => Seq[GP.Constraint] = generateCompareConstraints(GP.NeqComparator)

  type Res = (Seq[String], Seq[GP.Constraint])

  case class Binding(typ: Fun.TypeAnno, exp: Option[Fun.CoreExp], index: Option[Int]) {
    def shouldInline: Boolean = typ match {
      case Fun.TBool => exp.nonEmpty && index.isEmpty
      case _ => false
    }
  }
  type Env = Map[Fun.Name, Binding]


  def transBody(alt: Fun.Body, funs: FunEnv, outVars: Seq[String], env: Env)(implicit gensym: Gensym): Option[GP.Body] = gensym.scoped {
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

  def transStatement(stmt: Fun.CoreStatement, funs: FunEnv, outVars: Seq[String], env: Env)(implicit gensym: Gensym): (Seq[GP.Constraint], Env) = stmt match {
    case Fun.Values(name, typ) =>
      if (env.contains(name))
        throw new IllegalArgumentException(s"Program tries to rebind $name in $stmt")
      val binding = Binding(typ, None, None)
      val newenv = env + (name -> binding)
      (Seq(GP.HasType(GP.Var(name), transType(typ))), newenv)

    case Fun.Assign(names, exp) =>
      names.foreach { n =>
        if (env.contains(n))
          throw new IllegalArgumentException(s"Program tries to rebind $n in $stmt")
      }
      if (exp.typ.isEmpty)
        throw new IllegalArgumentException(s"Cannot compile untyped assignment $stmt")

      gensym.register(names)
      val expTy = exp.typ.get
      val bindings = names match {
        case Nil => Seq()
        case Seq(_) => Seq(Binding(expTy, Some(exp.ensureCore), None))
        case ns => ns.zipWithIndex.map { case (n,i) =>
          val ty = expTy match {
            case tup@Fun.TTuple(ts) =>
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
        val eqConstraints = genEqs(names, rvars)
        (rconstraints ++ eqConstraints, newenv)
      }

    case Fun.Assert(Fun.Constant(Fun.BooleanLiteral(v))) =>
      if (v) (Seq(), env)
      else throw BodyMustFail
    case Fun.Assert(cond) => transExp(cond.ensureCore)(funs, env, gensym) match {
      case (Nil, cons) =>
        (cons, env)
      case (Seq(v), cons) =>
        (cons :+ GP.Compare(GP.EqComparator, GP.Var(v), GP.Constant(GP.BooleanLiteral(true))), env)
    }

    case Fun.Yield(exp) =>
      val (vars, constraints) = transExp(exp.ensureCore)(funs, env, gensym)
      (constraints ++ genEqs(vars, outVars), env)

    case Fun.Fail =>
      throw BodyMustFail
  }

  def transExp(cond: Fun.CoreExp)(implicit funs: FunEnv, env: Env, gensym: Gensym): Res = cond match {
    case Fun.Var(name) =>
      env.get(name) match {
        case Some(binding) =>
          if (binding.shouldInline)
            transExp(binding.exp.get)
          else
            (Seq(name), Seq())
        case None =>
          throw new IllegalArgumentException(s"Unbound variable $name")
      }

    case Fun.Eq(lhs, rhs) =>
      val (lvars, lconstraints) = transExp(lhs.ensureCore)
      val (rvars, rconstraints) = transExp(rhs.ensureCore)
      val eqConstraints = genEqs(lvars, rvars)
      (Seq(), lconstraints ++ rconstraints ++ eqConstraints)

    case Fun.Neq(lhs, rhs) =>
      val (lvars, lconstraints) = transExp(lhs.ensureCore)
      val (rvars, rconstraints) = transExp(rhs.ensureCore)
      val eqConstraints = genNeqs(lvars, rvars)
      (Seq(), lconstraints ++ rconstraints ++ eqConstraints)

    case Fun.InstanceOf(exp, typ) =>
      val (vars, constraints) = transExp(exp.ensureCore)
      if (vars.size != 1) throw new IllegalArgumentException("Number of variables of exp of instance of need to be 1")
      (Seq(), constraints :+ GP.HasType(GP.Var(vars.head), transType(typ)))

    case ninst@Fun.NotInstanceOf(exp, typ) => (Seq(), Seq())
      val (vars, constraints) = transExp(exp.ensureCore)
      val notInstanceOfHelper = nameOfNotInstanceOfHelper(typ)
      val composition = GP.Call(notInstanceOfHelper, Seq(GP.Var(vars.head)), transitive = false, neg = true)
      (Seq(), constraints :+ composition)

    case Fun.Def(exp) =>
      exp match {
        case Fun.Call(name, args, transitive, _) =>
          (Seq(), genDefCallConstraint(name, args, transitive, neg = false))
        case pa: Fun.PathAccess =>
          val tmp = gensym.fresh("_")
          (Seq(), transPathAccess(pa, GP.Var(tmp)))
        case _ => throw new IllegalArgumentException(s"Cannot support Def($exp)")
      }

    case Fun.Undef(exp) =>
      exp match {
        case Fun.Call(name, args, transitive, count) =>
          (Seq(), genDefCallConstraint(name, args, transitive, neg = true))
        case path@Fun.PathAccess(exp, _) =>
          val pathHelper = nameOfUndefPathHelper(path)
          val (_, constraints) = transExp(exp.ensureCore)
          val args = path.freeVars.toSeq.map(v => GP.Var(v._1))
          val compositionConstraint = GP.Call(pathHelper, args, transitive = false, neg = true)
          (Seq(), constraints :+ compositionConstraint)
        case _ => throw new IllegalArgumentException("Cannot support in Undef " + exp)
      }

    case Fun.Constant(lit) =>
      transLiteral(lit) match {
        case None => (Seq(), Seq())
        case Some(gplit) =>
          val tmpVar = gensym.fresh("tmp")
          val compare = GP.Compare(GP.EqComparator, GP.Var(tmpVar), GP.Constant(gplit))
          (Seq(tmpVar), Seq(compare))
      }

    case Fun.Tuple(exps) =>
      val (vars, constraints) = exps.map(e => transExp(e.ensureCore)).unzip
      (vars.flatten, constraints.flatten)

    case pa: Fun.PathAccess =>
      val trg = gensym.fresh("trg")
      (Seq(trg), transPathAccess(pa, GP.Var(trg)))

    case Fun.Call(name, args, transitive, count) =>
      // TODO why is there a distinction between exp and non exp args in MPS impl?
      val (vars, constraints) = args.map(e => transExp(e.ensureCore)).unzip
      val outVars = funs(name).outParams.map { _ =>
        val argVar = gensym.fresh("arg")
        GP.Var(argVar)
      }
      val allvars = vars.flatten.map(GP.Var) ++ outVars

      if (!count) {
        val call = GP.Call(name, allvars, transitive, neg = false)
        (outVars.map(_.name), constraints.flatten :+ call)
      } else {
        val countVar = gensym.fresh("count")
        val countConstraint = GP.Computed(GP.Var(countVar), GP.CountAggregation(name, allvars))
        (Seq(countVar), Seq(countConstraint))
      }

    case Fun.Eval(params, ty, code) =>
      val evalVar = gensym.fresh("eval")
      val evalConstraint = GP.Computed(GP.Var(evalVar), GP.Evaluation(params.keys.map(GP.Var), transType(ty), code))
      (Seq(evalVar), Seq(evalConstraint))
  }

  def genDefCallConstraint(name: Fun.Name, args: Seq[Fun.Exp], transitive: Boolean, neg: Boolean)(implicit funs: FunEnv, env: Env, gensym: Gensym): Seq[GP.Constraint] = {
    val (vars, constraint) = args.map {
      case arg@Fun.Var(name) => (Seq(name), Seq())
      case arg =>
        val (vars, constraints) = transExp(arg.ensureCore)
        if (vars.size > 1)
          throw new IllegalArgumentException("More than one result variable for one argument " + arg)
        (vars, constraints)
    }.unzip

    val stillReq = (funs(name).params ++ funs(name).outParams).size - vars.flatten.size
    val tempVars = for (i <- 0 until stillReq) yield {
      GP.Var(gensym.fresh("arg"))
    }
    val compositionConstraint = GP.Call(name, vars.flatten.map(GP.Var) ++ tempVars, transitive, neg)
    constraint.flatten :+ compositionConstraint
  }

  def transPathAccess(pathAccess: Fun.PathAccess, trg: GP.Term)(implicit funs: FunEnv, env: Env, gensym: Gensym): Seq[GP.Constraint] = {
    val receiver = pathAccess.receiver
    val (Seq(src), econstraints) = transExp(receiver.ensureCore)
    val ty = transType(pathAccess.typ.getOrElse(throw new IllegalArgumentException(s"Cannot compile untyped $pathAccess")))
    val path = pathAccess.link match {
      case Fun.ParentLink =>
        GP.Path(GP.Var(src), trg, GP.ParentLink, ty)
      case Fun.ChildrenLink =>
        GP.Path(trg, GP.Var(src), GP.ParentLink, ty)
      case Fun.NextLink =>
        GP.Path(GP.Var(src), trg, GP.NextLink, ty)
      case Fun.PreviousLink =>
        GP.Path(trg, GP.Var(src), GP.NextLink, ty)
      case Fun.SizeLink =>
        GP.Path(GP.Var(src), trg, GP.SizeLink, ty)
      case Fun.NamedLink(node, field) =>
        GP.Path(GP.Var(src), trg, GP.NamedLink(GP.TNode(node.name), field), ty)
    }
    econstraints :+ path
  }

  def transLiteral(lit: Fun.Literal): Option[GP.Literal] = lit match {
    case Fun.UnitLiteral => None
    case Fun.IntLiteral(v) => Some(GP.IntLiteral(v))
    case Fun.LongLiteral(v) => Some(GP.LongLiteral(v))
    case Fun.DoubleLiteral(v) => Some(GP.DoubleLiteral(v))
    case Fun.StringLiteral(v) => Some(GP.StringLiteral(v))
    case Fun.BooleanLiteral(v) => Some(GP.BooleanLiteral(v))
  }

  // TODO fullname
  def nameOfUndefPathHelper(access: Fun.PathAccess): String = "generated_helper_undefpath_" + access.link

  def genUndefPathHelper(access: Fun.PathAccess): Fun.PatternFunction = {
    if (access.receiver.typ.isEmpty)
      throw new IllegalArgumentException(s"Cannot support undef condition for untyped receiver of path access $access")

    val params = access.freeVars.toSeq.map{ case (v,t) => Fun.Param(v, t) }

    Fun.PatternFunction(
      Some(Fun.Private),
      nameOfUndefPathHelper(access),
      params,
      List(),
      List(Fun.Body(List(Fun.Assert(Fun.Def(access))))))
  }

  def nameOfNotInstanceOfHelper(ty: Fun.TypeAnno): String = "generated_helper_notinstanceof_" + ty.javastring

  def genNotInstanceOfHelper(ty: Fun.TypeAnno): Fun.PatternFunction =
    Fun.PatternFunction(
      Some(Fun.Private),
      nameOfNotInstanceOfHelper(ty),
      List(Fun.Param("in", Some(ty))),
      List(),
      // body is empty because relation is only applicable if c is actually of type ninst.typ
      List(Fun.Body(Seq())))
}
