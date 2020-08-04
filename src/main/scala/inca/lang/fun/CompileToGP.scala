package inca.lang.fun

import inca.lang.gp.GP
import inca.util.Gensym

object CompileToGP {

  private case object BodyMustFail extends Exception

  def transformModule(module: Fun.Module): GP.Module = {
    val undefs = module.funs.flatMap(collectPathInUndef).toSet
    val ninsts = module.funs.flatMap(collectNotInstanceOf).toSet
    // generate helpers
    val helpers = undefs.map(genUndefPathHelper) ++ ninsts.map(genNotInstanceOfHelper)
    // construct map Name => Fun
    val funs = module.funs.map { fun => fun.name -> fun}.toMap
    val patterns = (module.funs ++ helpers).map { fun => transform(fun, funs) }
    GP.Module(module.name, module.imports, patterns)
  }

  def collectNotInstanceOf(fun: Fun.PatternFunction): Seq[Fun.NotInstanceOf] = fun.bodies.flatMap {
    case Fun.Body(stmts) => stmts.collect {
      case Fun.Assert(cond) => cond match {
        case ninst@Fun.NotInstanceOf(_, _) => Seq(ninst)
        case _ => Nil
      }
    }.flatten
  }

  def collectPathInUndef(fun: Fun.PatternFunction): Seq[Fun.PathAccess] = fun.bodies.flatMap {
    case Fun.Body(stmts) => stmts.collect {
      case Fun.Assert(cond) => cond match {
        case Fun.Undef(cond: Fun.PathAccess) => Seq(cond)
        case _ => Nil
      }
    }.flatten
  }

  def transform(fun: Fun.PatternFunction, funs: Map[String, Fun.PatternFunction]): GP.Pattern = {
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



  def rewriteFunction(fun: Fun.PatternFunction, funs: Map[String, Fun.PatternFunction]): GP.Pattern = {
    val gensym = new Gensym(fun.usedvars.keys)
    val vis = fun.vis.map {
      case Fun.Private => GP.Private
      case Fun.Public => GP.Public
    }

    val params = fun.params.map { param => GP.Param(param.name, param.typ.map(transType)) }

    val outParams = fun.outParams.map { param =>
      val name =
        if (param.name.isDefined) param.name.get
        else gensym.fresh("out")
      GP.Param(name, Some(transType(param.typ)))
    }
    val outVars = outParams.map(_.name)

    def generateCompareConstraints(comp: GP.Comparator)(lhs: Seq[String], rhs: Seq[String]): Seq[GP.Constraint] = {
      if (lhs.size != rhs.size) throw new IllegalArgumentException("Cannot create equalites for different sized variable lists")
      for (i <- lhs.indices) yield {
        GP.Compare(comp, GP.Var(lhs(i)), GP.Var(rhs(i)))
      }
    }

    val genEqs: (Seq[String], Seq[String]) => Seq[GP.Constraint] = generateCompareConstraints(GP.EqComparator)
    val genNeqs: (Seq[String], Seq[String]) => Seq[GP.Constraint] = generateCompareConstraints(GP.NeqComparator)

    type Res = (Seq[String], Seq[GP.Constraint])

    def transBody(alt: Fun.Body): Option[GP.Body] =
      try {
        val constraints = alt.stmts.flatMap(s => transStatement(s.ensureCore))
        Some(GP.Body(constraints))
      } catch {
        case BodyMustFail => None
      }

    def transStatement(stmt: Fun.CoreStatement): Seq[GP.Constraint] = stmt match {
      case Fun.Assert(cond) => transCond(cond.ensureCore)
      case Fun.Assign(names, exp) =>
        val (rvars, rconstraints) = transExp(exp.ensureCore)
        val eqConstraints = genEqs(names, rvars)
        rconstraints ++ eqConstraints
      case Fun.Yield(exp) =>
        val (vars, constraints) = transExp(exp.ensureCore)
        constraints ++ genEqs(vars, outVars)
      case Fun.Fail =>
        throw BodyMustFail
    }

    def transCond(cond: Fun.CoreCond): Seq[GP.Constraint] = cond match {
      case Fun.Eq(lhs, rhs) =>
        val (lvars, lconstraints) = transExp(lhs.ensureCore)
        val (rvars, rconstraints) = transExp(rhs.ensureCore)
        val eqConstraints = genEqs(lvars, rvars)
        lconstraints ++ rconstraints ++ eqConstraints
      case Fun.Neq(lhs, rhs) =>
        val (lvars, lconstraints) = transExp(lhs.ensureCore)
        val (rvars, rconstraints) = transExp(rhs.ensureCore)
        val eqConstraints = genNeqs(lvars, rvars)
        lconstraints ++ rconstraints ++ eqConstraints
      case Fun.InstanceOf(exp, typ) =>
        val (vars, constraints) = transExp(exp.ensureCore)
        if (vars.size != 1) throw new IllegalArgumentException("Number of variables of exp of instance of need to be 1")
        constraints :+ GP.HasType(GP.Var(vars.head), transType(typ))

      case ninst@Fun.NotInstanceOf(exp, typ) => (Seq(), Seq())
        val (vars, constraints) = transExp(exp.ensureCore)
        val notInstanceOfHelper = nameOfNotInstanceOfHelper(ninst)
        val composition = GP.Call(notInstanceOfHelper, Seq(GP.Var(vars.head)), transitive = false, neg = true)
        constraints :+ composition
      case Fun.Def(exp) =>
        exp match {
          case Fun.Call(name, args, transitive, _) =>
            genDefCallConstraint(name, args, transitive, funs, neg = false)
          case pa: Fun.PathAccess =>
            val tmp = gensym.fresh("tmp")
            transPathAccess(pa, GP.Var(tmp))
          case _ => throw new IllegalArgumentException(s"Cannot support Def($exp)")
        }
      case Fun.Undef(exp) =>
        exp match {
          case Fun.Call(name, args, transitive, count) =>
            genDefCallConstraint(name, args, transitive, funs, neg = true)
          case path@Fun.PathAccess(exp, _) =>
            val pathHelper = nameOfUndefPathHelper(path)
            val (vars, constraints) = transExp(exp.ensureCore)
            val args = path.usedvars.toSeq.map(v => GP.Var(v._1))
            val compositionConstraint = GP.Call(pathHelper, args, transitive = false, neg = true)
            constraints :+ compositionConstraint
          case _ => throw new IllegalArgumentException("Cannot support in Undef " + exp)
        }
      case Fun.BooleanCond(v) =>
        if (v)
          Seq()
        else
          throw BodyMustFail
    }

    def genDefCallConstraint(name: Fun.Name, args: Seq[Fun.Exp], transitive: Boolean, funs: Map[String, Fun.PatternFunction], neg: Boolean): Seq[GP.Constraint] = {
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

    def transExp(exp: Fun.CoreExp): Res = exp match {
      case Fun.Var(name) => (Seq(name), Seq())
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

    }

    def transPathAccess(pathAccess: Fun.PathAccess, trg: GP.Term): Seq[GP.Constraint] = {
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

    val bodies = fun.bodies.flatMap(transBody)
    GP.Pattern(vis, fun.name, params ++ outParams, bodies)
  }

  // TODO fullname
  def nameOfUndefPathHelper(access: Fun.PathAccess): String = "generated_helper_undefpath_" + access.link

  def genUndefPathHelper(access: Fun.PathAccess): Fun.PatternFunction = {
    if (access.receiver.typ.isEmpty)
      throw new IllegalArgumentException(s"Cannot support undef condition for untyped receiver of path access $access")

    val params = access.usedvars.toSeq.map{ case (v,t) => Fun.Param(v, t) }

    Fun.PatternFunction(
      Some(Fun.Private),
      nameOfUndefPathHelper(access),
      params,
      List(),
      List(Fun.Body(List(Fun.Assert(Fun.Def(access))))))
  }

  def nameOfNotInstanceOfHelper(ninst: Fun.NotInstanceOf): String = "generated_helper_notinstanceof_" + ninst.typ.javastring

  def genNotInstanceOfHelper(ninst: Fun.NotInstanceOf): Fun.PatternFunction =
    Fun.PatternFunction(
      Some(Fun.Private),
      nameOfNotInstanceOfHelper(ninst),
      List(Fun.Param("in", Some(ninst.typ))),
      List(),
      // body is empty because relation is only applicable if c is actually of type ninst.typ
      List(Fun.Body(Seq())))
}
