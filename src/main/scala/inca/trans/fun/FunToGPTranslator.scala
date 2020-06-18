package inca.trans.fun

import inca.MetaElements.{DefinedNodeLink, Link}
import inca.lang.{FunLang => Fun, GraphPatternLang => GP}
import inca.util.Gensym

object FunToGPTranslator {

  def transformModule(module: Fun.Module): GP.Module = {
    val undefs = module.funs.flatMap(collectPathInUndef)
    val ninsts = module.funs.flatMap(collectNotInstanceOf)
    // generate helpers
    val helpers = undefs.map(genUndefPathHelper) ++ ninsts.map(genNotInstanceOfHelper)
    // construct map Name => Fun
    val funs = module.funs.map { fun => fun.name -> fun}.toMap
    val patterns = (module.funs ++ helpers).map { fun => transform(fun, funs) }
    GP.Module(module.name, module.imports, patterns)
  }

  def collectNotInstanceOf(fun: Fun.PatternFunction): Seq[Fun.NotInstanceOf] = fun.bodies.flatMap {
    case Fun.Alternative(stmts) => stmts.collect {
      case Fun.Assert(cond) => cond match {
        case ninst@Fun.NotInstanceOf(_, _) => Seq(ninst)
        case _ => Nil
      }
    }.flatten
  }

  def collectPathInUndef(fun: Fun.PatternFunction): Seq[Fun.PathAccess] = fun.bodies.flatMap {
    case Fun.Alternative(stmts) => stmts.collect {
      case Fun.Assert(cond) => cond match {
        case Fun.Undef(cond: Fun.PathAccess) => Seq(cond)
        case _ => Nil
      }
    }.flatten
  }

  def transform(fun: Fun.PatternFunction, funs: Map[String, Fun.PatternFunction]): GP.GraphPattern = {
    // TODO meta analysis negation in recusion
    rewriteFunction(fun, funs)
  }

  def rewriteFunction(fun: Fun.PatternFunction, funs: Map[String, Fun.PatternFunction]): GP.GraphPattern = {
    val gensym = new Gensym(FunVars(fun).toSet)
    val vis = fun.vis.map {
      case Fun.Private => GP.Private
      case Fun.Public => GP.Public
    }

    def transType(typ: Fun.Type): GP.Type = typ match {
      case Fun.TType(wrapped) => GP.TType(wrapped)
      case Fun.TBool => GP.TBool
      case Fun.TInt => GP.TInt
      case Fun.TLong => GP.TLong
      case Fun.TDouble => GP.TDouble
      case Fun.TString => GP.TString
    }


    val params = fun.params.map { param => GP.Param(param.name, if (param.typ.isDefined) Some(transType(param.typ.get)) else None) }

    val outParams = fun.outParams.map { param =>
      val name =
        if (param.name.isDefined) param.name.get
        else gensym.fresh("out")
      GP.Param(name, Some(transType(param.typ)))
    }
    val outVars = outParams.map(_.name)

    def generateCompareConstraints(comp: GP.Comparator)(lhs: Seq[String], rhs: Seq[String]): Seq[GP.Constraint] = {
      if (lhs.size != rhs.size) throw new IllegalStateException("Cannot create equalites for different sized variable lists")
      for (i <- lhs.indices) yield {
        GP.Compare(comp, GP.Var(lhs(i)), GP.Var(rhs(i)))
      }
    }

    val genEqs: (Seq[String], Seq[String]) => Seq[GP.Constraint] = generateCompareConstraints(GP.EqComparator)
    val genNeqs: (Seq[String], Seq[String]) => Seq[GP.Constraint] = generateCompareConstraints(GP.NeqComparator)

    type Res = (Seq[String], Seq[GP.Constraint])

    def transAlternative(alt: Fun.Alternative): Res = (Seq(), alt.stmts.map(transStatement).flatMap(_._2))

    def transStatement(stmt: Fun.Statement): Res = stmt match {
      case Fun.Return(exp) =>
        val (vars, constraints) = transExp(exp)
        (Seq(), constraints ++ genEqs(vars, outVars))
      case Fun.Assignment(names, exp) =>
        // TODO difference in encoding, in the paper lhs is a list of names, actual implementation one expression (which can be a tuple thus multiple names)
//        val (lvars, lconstraints) = transExp()
        val (rvars, rconstraints) = transExp(exp)
        val eqConstraints = genEqs(names, rvars)
        (Seq(), rconstraints ++ eqConstraints)
      case Fun.Assert(cond) => transCond(cond)
    }

    def transCond(cond: Fun.Cond): Res = cond match {
      case Fun.Eq(lhs, rhs) =>
        val (lvars, lconstraints) = transExp(lhs)
        val (rvars, rconstraints) = transExp(rhs)
        val eqConstraints = genEqs(lvars, rvars)
        (Seq(), lconstraints ++ rconstraints ++ eqConstraints)
      case Fun.Neq(lhs, rhs) =>
        val (lvars, lconstraints) = transExp(lhs)
        val (rvars, rconstraints) = transExp(rhs)
        val eqConstraints = genNeqs(lvars, rvars)
        (Seq(), lconstraints ++ rconstraints ++ eqConstraints)
      case Fun.InstanceOf(exp, typ) =>
        val (vars, constraints) = transExp(exp)
        if (vars.size != 1) throw new IllegalStateException("Number of variables of exp of instance of need to be 1")
        (Seq(), constraints :+ GP.Concept(GP.Var(vars.head), transType(typ)))

      case ninst@Fun.NotInstanceOf(exp, typ) => (Seq(), Seq())
        val (vars, constraints) = transExp(exp)
        val notInstanceOfHelper = nameOfNotInstanceOfHelper(ninst)
        val composition = GP.Composition(GP.PatternCall(notInstanceOfHelper, Seq(GP.Var(vars.head)), transitive = false), neg = true)
        (Seq(), constraints :+ composition)
      case Fun.Def(exp) =>
        exp match {
          case Fun.Call(call, count) =>
            genDefCallConstraint(call, funs, neg = false)
          case Fun.PathAccess(exp, paths) =>
            // TODO is this correct?
            val (vars, pathConstraints) = genPathConstraints(exp, paths.dropRight(1))
            val trueLit = GP.BooleanLiteral(true)
            val definedLink = DefinedNodeLink(paths.last.typ, paths.last.field)
            val trg = gensym.fresh("trg")
            val definedConstraint = GP.Path(GP.Var(vars.head), GP.Var(trg), definedLink, definedLink.typ)
            val compareConstraint = GP.Compare(GP.EqComparator, GP.Var(trg), GP.Constant(trueLit))
            (Seq(), pathConstraints ++ Seq(definedConstraint, compareConstraint))
          case _ => throw new IllegalArgumentException("Cannot support in Def " + exp)
        }
      case Fun.Undef(exp) =>
        exp match {
          case Fun.Call(call, count) =>
            genDefCallConstraint(call, funs, neg = true)
          case path@Fun.PathAccess(exp, _) =>
            val pathHelper = nameOfUndefPathHelper(path)
            val (vars, constraints) = transExp(exp)
            val compositionConstraint = GP.Composition(GP.PatternCall(pathHelper, Seq(GP.Var(vars.head)), transitive = false),neg = true)
            (Seq(), constraints :+ compositionConstraint)
          case _ => throw new IllegalArgumentException("Cannot support in Undef " + exp)
        }
    }

    def genDefCallConstraint(call: Fun.PatternCall, funs: Map[String, Fun.PatternFunction], neg: Boolean): Res = {
      val (vars, constraint) = call.args.map {
        case arg@Fun.Var(name) => (Seq(name), Seq())
        case arg =>
          val (vars, constraints) = transExp(arg)
          if (vars.size > 1)
            throw new IllegalStateException("More than one result variable for one argument " + arg)
          (vars, constraints)
      }.unzip

      val stillReq = (funs(call.name).params ++ funs(call.name).outParams).size - vars.flatten.size
      val tempVars = for (i <- 0 until stillReq) yield {
        GP.Var(gensym.fresh("arg"))
      }
      val patternCall = GP.PatternCall(call.name, vars.flatten.map(GP.Var) ++ tempVars, call.transitive)
      val compositionConstraint = GP.Composition(patternCall, neg)
      (Seq(), constraint.flatten :+ compositionConstraint)
    }

    def genPathConstraints(exp: Fun.Exp, paths: Seq[Link]): Res = {
      val (evars, econstraints) = transExp(exp)
      var src = evars.head
      val pathConstraints = paths.map { path =>
        val trg = gensym.fresh("trg")
        val trgVar = GP.Var(trg)
        val srcVar = GP.Var(src)
        src = trg
        GP.Path(srcVar, trgVar, path, path.typ)
      }
      (Seq(src), econstraints ++ pathConstraints)
    }

    def transExp(exp: Fun.Exp): Res = exp match {
      case Fun.Var(name) => (Seq(name), Seq())
      case Fun.Constant(lit) =>
        val tmpVar = gensym.fresh("tmp")
        val compare = GP.Compare(GP.EqComparator, GP.Var(tmpVar), GP.Constant(transLiteral(lit)))
        (Seq(tmpVar), Seq(compare))
      case Fun.Tuple(exps) =>
        val (vars, constraints) = exps.map(transExp).unzip
        val eqConstraints = genEqs(vars.flatten, outVars)
        (Seq(), constraints.flatten ++ eqConstraints)
      case Fun.PathAccess(exp, paths) => genPathConstraints(exp, paths)
      case Fun.Call(call, count) =>
        // TODO why is there a distinction between exp and non exp args in MPS impl?
        val (vars, constraints) = call.args.map(transExp).unzip
        val outVars = funs(call.name).outParams.map { _ =>
            val argVar = gensym.fresh("arg")
            GP.Var(argVar)
        }
        val patternCall = GP.PatternCall(call.name, vars.flatten.map(GP.Var) ++ outVars, call.transitive)
        if (count) {
          throw new IllegalArgumentException("TODO cannot support count aggregation")
        } else {
          val compositionConstraint = GP.Composition(patternCall, neg = false)
          (outVars.map(_.name), constraints.flatten :+ compositionConstraint)
        }
    }

    def transLiteral(lit: Fun.Literal): GP.Literal = lit match {
      case Fun.IntLiteral(v) => GP.IntLiteral(v)
      case Fun.LongLiteral(v) => GP.LongLiteral(v)
      case Fun.DoubleLiteral(v) => GP.DoubleLiteral(v)
      case Fun.StringLiteral(v) => GP.StringLiteral(v)
      case Fun.BooleanLiteral(v) => GP.BooleanLiteral(v)
    }

    val bodies = fun.bodies.map(transAlternative).map(c => GP.Alternative(c._2))
    GP.GraphPattern(vis, fun.name, params ++ outParams, bodies)
  }

  // TODO fullname
  def nameOfUndefPathHelper(access: Fun.PathAccess): String = "generated_helper_undefpath_" + access.path.map(_.toString).mkString(".")

  def genUndefPathHelper(path: Fun.PathAccess): Fun.PatternFunction = {
    Fun.PatternFunction(
      Some(Fun.Private),
      nameOfUndefPathHelper(path),
      // TODO figure out type of src
      List(Fun.Param("in", Some(path.path.head.typ))),
      List(),
      List(Fun.Alternative(List(Fun.Assert(Fun.Def(path))))))
  }

  def nameOfNotInstanceOfHelper(ninst: Fun.NotInstanceOf): String = "generated_helper_notinstanceof_" + nameOfType(ninst.typ)

  def nameOfType(typ: Fun.Type): String = typ match {
    case Fun.TType(wrapped) => wrapped.name.replace(".", "_")
    case Fun.TBool => "TBool"
    case Fun.TInt => "TInt"
    case Fun.TLong => "TLong"
    case Fun.TDouble => "TDouble"
    case Fun.TString => "TString"
  }

  def genNotInstanceOfHelper(ninst: Fun.NotInstanceOf): Fun.PatternFunction =
    Fun.PatternFunction(
      Some(Fun.Private),
      nameOfNotInstanceOfHelper(ninst),
      List(Fun.Param("in", Some(ninst.typ))),
      List(),
      // body is empty because relation is only applicable if c is actually of type ninst.typ
      List())
}
