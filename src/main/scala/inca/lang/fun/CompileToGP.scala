package inca.lang.fun

import inca.lang.gp.GP
import inca.util.Gensym

object CompileToGP {

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

  def transform(fun: Fun.PatternFunction, funs: Map[String, Fun.PatternFunction]): GP.Rule = {
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



  def rewriteFunction(fun: Fun.PatternFunction, funs: Map[String, Fun.PatternFunction]): GP.Rule = {
    val gensym = new Gensym(CollectVars(fun).toSet)
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

    def generateCompareConstraints(comp: GP.Comparator)(lhs: Seq[String], rhs: Seq[String]): Seq[GP.Atom] = {
      if (lhs.size != rhs.size) throw new IllegalStateException("Cannot create equalites for different sized variable lists")
      for (i <- lhs.indices) yield {
        GP.Compare(comp, GP.Var(lhs(i)), GP.Var(rhs(i)))
      }
    }

    val genEqs: (Seq[String], Seq[String]) => Seq[GP.Atom] = generateCompareConstraints(GP.EqComparator)
    val genNeqs: (Seq[String], Seq[String]) => Seq[GP.Atom] = generateCompareConstraints(GP.NeqComparator)

    type Res = (Seq[String], Seq[GP.Atom])

    def transAlternative(alt: Fun.Body): Res = (Seq(), alt.stmts.map(transStatement).flatMap(_._2))

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
        (Seq(), constraints :+ GP.HasType(GP.Var(vars.head), transType(typ)))

      case ninst@Fun.NotInstanceOf(exp, typ) => (Seq(), Seq())
        val (vars, constraints) = transExp(exp)
        val notInstanceOfHelper = nameOfNotInstanceOfHelper(ninst)
        val composition = GP.Call(notInstanceOfHelper, Seq(GP.Var(vars.head)), transitive = false, neg = true)
        (Seq(), constraints :+ composition)
      case Fun.Def(exp) =>
        exp match {
          case Fun.Call(name, args, transitive, _) =>
            genDefCallConstraint(name, args, transitive, funs, neg = false)
          case pa: Fun.PathAccess =>
            val tmp = gensym.fresh("tmp")
            (Seq(), transPathAccess(pa, GP.Var(tmp)))
          case _ => throw new IllegalArgumentException(s"Cannot support Def($exp)")
        }
      case Fun.Undef(exp) =>
        exp match {
          case Fun.Call(name, args, transitive, count) =>
            genDefCallConstraint(name, args, transitive, funs, neg = true)
          case path@Fun.PathAccess(exp, _) =>
            val pathHelper = nameOfUndefPathHelper(path)
            val (vars, constraints) = transExp(exp)
            val compositionConstraint = GP.Call(pathHelper, Seq(GP.Var(vars.head)), transitive = false, neg = true)
            (Seq(), constraints :+ compositionConstraint)
          case _ => throw new IllegalArgumentException("Cannot support in Undef " + exp)
        }
    }

    def genDefCallConstraint(name: Fun.Name, args: Seq[Fun.Exp], transitive: Boolean, funs: Map[String, Fun.PatternFunction], neg: Boolean): Res = {
      val (vars, constraint) = args.map {
        case arg@Fun.Var(name) => (Seq(name), Seq())
        case arg =>
          val (vars, constraints) = transExp(arg)
          if (vars.size > 1)
            throw new IllegalStateException("More than one result variable for one argument " + arg)
          (vars, constraints)
      }.unzip

      val stillReq = (funs(name).params ++ funs(name).outParams).size - vars.flatten.size
      val tempVars = for (i <- 0 until stillReq) yield {
        GP.Var(gensym.fresh("arg"))
      }
      val compositionConstraint = GP.Call(name, vars.flatten.map(GP.Var) ++ tempVars, transitive, neg)
      (Seq(), constraint.flatten :+ compositionConstraint)
    }

    def transExp(exp: Fun.Exp): Res = exp match {
      case Fun.Var(name) => (Seq(name), Seq())
      case Fun.Constant(lit) =>
        val tmpVar = gensym.fresh("tmp")
        val compare = GP.Compare(GP.EqComparator, GP.Var(tmpVar), GP.Constant(transLiteral(lit)))
        (Seq(tmpVar), Seq(compare))
      case Fun.Tuple(exps) =>
        val (vars, constraints) = exps.map(transExp).unzip
        (vars.flatten, constraints.flatten)
      case pa: Fun.PathAccess =>
        val trg = gensym.fresh("trg")
        (Seq(trg), transPathAccess(pa, GP.Var(trg)))
      case Fun.Call(name, args, transitive, count) =>
        // TODO why is there a distinction between exp and non exp args in MPS impl?
        val (vars, constraints) = args.map(transExp).unzip
        val outVars = funs(name).outParams.map { _ =>
            val argVar = gensym.fresh("arg")
            GP.Var(argVar)
        }
        if (count) {
          throw new IllegalArgumentException("TODO cannot support count aggregation")
        } else {
          val compositionConstraint = GP.Call(name, vars.flatten.map(GP.Var) ++ outVars, transitive, neg = false)
          (outVars.map(_.name), constraints.flatten :+ compositionConstraint)
        }
    }

    def transPathAccess(pathAccess: Fun.PathAccess, trg: GP.Term): Seq[GP.Atom] = {
      val receiver = pathAccess.receiver
      val (Seq(src), econstraints) = transExp(receiver)
      if (pathAccess.typ == null)
        throw new IllegalArgumentException(s"Cannot compile untyped $pathAccess")
      val ty = transType(pathAccess.typ)
      val path = pathAccess.link match {
        case Fun.ParentLink =>
          GP.Path(GP.Var(src), trg, GP.ParentLink, ty)
        case Fun.ChildrenLink =>
          GP.Path(trg, GP.Var(src), GP.ParentLink, ty)
        case Fun.NextLink =>
          GP.Path(GP.Var(src), trg, GP.NextLink, ty)
        case Fun.PreviousLink =>
          GP.Path(trg, GP.Var(src), GP.NextLink, ty)
        case Fun.NamedLink(node, field) =>
          GP.Path(GP.Var(src), trg, GP.NamedLink(GP.TNode(node.name), field), ty)
      }
      econstraints :+ path
    }

    def transLiteral(lit: Fun.Literal): GP.Literal = lit match {
      case Fun.IntLiteral(v) => GP.IntLiteral(v)
      case Fun.LongLiteral(v) => GP.LongLiteral(v)
      case Fun.DoubleLiteral(v) => GP.DoubleLiteral(v)
      case Fun.StringLiteral(v) => GP.StringLiteral(v)
      case Fun.BooleanLiteral(v) => GP.BooleanLiteral(v)
    }

    val bodies = fun.bodies.map(transAlternative).map(c => GP.Body(c._2))
    GP.Rule(vis, fun.name, params ++ outParams, bodies)
  }

  // TODO fullname
  def nameOfUndefPathHelper(access: Fun.PathAccess): String = "generated_helper_undefpath_" + access.link

  def genUndefPathHelper(access: Fun.PathAccess): Fun.PatternFunction = {
    Fun.PatternFunction(
      Some(Fun.Private),
      nameOfUndefPathHelper(access),
      List(Fun.Param("in", Option(access.receiver.typ))),
      List(),
      List(Fun.Body(List(Fun.Assert(Fun.Def(access))))))
  }

  def nameOfNotInstanceOfHelper(ninst: Fun.NotInstanceOf): String = "generated_helper_notinstanceof_" + nameOfType(ninst.typ)

  def nameOfType(typ: Fun.TypeAnno): String = typ.toString.replace(".", "_")

  def genNotInstanceOfHelper(ninst: Fun.NotInstanceOf): Fun.PatternFunction =
    Fun.PatternFunction(
      Some(Fun.Private),
      nameOfNotInstanceOfHelper(ninst),
      List(Fun.Param("in", Some(ninst.typ))),
      List(),
      // body is empty because relation is only applicable if c is actually of type ninst.typ
      List())
}
