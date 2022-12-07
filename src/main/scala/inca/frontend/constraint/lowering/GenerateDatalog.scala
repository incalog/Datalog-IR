package inca.frontend.constraint.lowering

import inca.backend.hints.MagicSetHints
import inca.backend.ir.Datalog
import inca.frontend.constraint.core._
import inca.util.{Gensym, Scala}

import scala.collection.mutable.ListBuffer
import scala.meta.{Name => _, Type => _}

class GenerateDatalog {
  val gensym = new Gensym(Iterable.empty)

  private val generatedPatterns = ListBuffer[Datalog.Pattern]()

  def transformModule(module: Module): Datalog.Module = {
    // construct map Name => Fun
    val Module(name, langModel, imports, nodeImports, contents) = module
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
    Datalog.Module(name.name, imports.map(_.name.name), generatedPatterns.toList, scalaContent.map(Scala.apply))
  }

  def transform(fun: PatternFunction): Datalog.Pattern = {
    rewriteFunction(fun)
  }

  def transType(typ: Type): Datalog.Type = typ match {
    case TAny => Datalog.TAny
    case TLiteral(lit) => Datalog.TLiteral(lit)
    case TAnyLinked => Datalog.TAnyLinked
    case node: TNode => Datalog.TNode(getFqnNode(node).name)
    case TList(ty) => Datalog.TList(transType(ty).asInstanceOf[Datalog.TLinked])
    case TScala(ty) => Datalog.TScala(ty)
  }


  def rewriteFunction(fun: PatternFunction): Datalog.Pattern = {
    gensym.register(fun.freeVars.keys.map(_.name))
    gensym.register(fun.boundNames.map(_.name))

    val vis = fun.vis.map {
      case Private => Datalog.Private
    }

    val params = fun.params.map { param => Datalog.Param(param.name.name, transType(param.typ)) }
    val outParams = fun.outParams.map { out =>
      val name = gensym.fresh("out")
      Datalog.Param(name, transType(out))
    }
    val outVars = outParams.map(_.name)

    val bodies = fun.bodies.flatMap(b => transBody(b, outVars)(gensym))
    val pat = Datalog.Pattern(vis, fun.name.name, params ++ outParams, bodies)
    if (fun.hasAnnotation(MainFunctionAnno.key))
      pat.addHint(MagicSetHints.Main(params.map(_ => true) ++ outParams.map(_ => false)))
    pat
  }

  def generateCompareConstraints(comp: Datalog.Comparator)(lhs: Seq[String], rhs: Seq[String]): Seq[Datalog.Atom] = {
    if (lhs.size != rhs.size)
      throw new IllegalArgumentException("Cannot create equalites for different sized variable lists")
    (lhs zip rhs).map { case (l, r) => Datalog.Compare(comp, Datalog.Var(l), Datalog.Var(r)) }
  }

  val genEqs: (Seq[String], Seq[String]) => Seq[Datalog.Atom] = generateCompareConstraints(Datalog.EqComparator)
  val genNeqs: (Seq[String], Seq[String]) => Seq[Datalog.Atom] = generateCompareConstraints(Datalog.NeqComparator)

  type Res = (Seq[String], Seq[Datalog.Atom])

  def transBody(alt: Body, outVars: Seq[String])(implicit gensym: Gensym): Option[Datalog.Body] = {
    try {
      val constraints = alt.stmts.flatMap { s =>
        transStatement(s.ensureCore, outVars)
      }
      Some(Datalog.Body(constraints))
    } catch {
      case Datalog.BodyMustFail => None
    }
  }

  def transStatement(stmt: CoreStatement, outVars: Seq[String])(implicit gensym: Gensym): Seq[Datalog.Atom] = stmt match {
    case Values(name, typ) =>
      (Seq(Datalog.HasType(Datalog.Var(name.name), transType(typ))))

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
      else throw Datalog.BodyMustFail
    case Assert(cond) => transExp(cond.ensureCore) match {
      case (Nil, cons) =>
        cons
      case (Seq(v), cons) =>
        cons :+ Datalog.Compare(Datalog.EqComparator, Datalog.Var(v), Datalog.Constant(Datalog.BooleanLiteral(true)))
    }

    case Yield(exp) =>
      val (vars, constraints) = transExp(exp.ensureCore)
      constraints ++ genEqs(vars, outVars)

    case FailStatement =>
      throw Datalog.BodyMustFail
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
      (Seq(), constraints :+ Datalog.HasType(Datalog.Var(vars.head), transType(typ)))

    case NotInstanceOf(exp, typ) =>
      val (vars, constraints) = transExp(exp.ensureCore)
      if (vars.size != 1) throw new IllegalArgumentException("Number of variables of exp of instance of need to be 1")
      (Seq(), constraints :+ Datalog.NotHasType(Datalog.Var(vars.head), transType(typ)))

    case Cast(exp, typ) =>
      val (vars, constraints) = transExp(exp.ensureCore)
      if (vars.size != 1) throw new IllegalArgumentException("Number of variables of exp of instance of need to be 1")
      val v = vars.head
      (Seq(v), constraints :+ Datalog.HasType(Datalog.Var(v), transType(typ)))

    case Def(exp) =>
      exp match {
        case call@Call(name, args, transitive) =>
          (Seq(), genDefCallConstraint(call, args, transitive, neg = false))
        case pa: PathAccess =>
          val tmp = gensym.fresh("_")
          (Seq(), transPathAccess(pa, Datalog.Var(tmp)))
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
              Datalog.NoPath(Datalog.Var(src), srcTy, Datalog.ParentLink, termIsSource = true)
            case ChildrenLink =>
              Datalog.NoPath(Datalog.Var(src), srcTy, Datalog.ParentLink, termIsSource = false)
            case NextLink =>
              Datalog.NoPath(Datalog.Var(src), srcTy, Datalog.NextLink, termIsSource = true)
            case PreviousLink =>
              Datalog.NoPath(Datalog.Var(src), srcTy, Datalog.NextLink, termIsSource = false)
            case SizeLink =>
              Datalog.NoPath(Datalog.Var(src), srcTy, Datalog.SizeLink, termIsSource = true)
            case NamedLink(field) =>
              val nodeType = receiver.typ match {
                case Some(node@TNode(_)) =>
                  Datalog.TNode(getFqnNode(node).name)
                case _ => throw new IllegalArgumentException(s"$receiver should have node type, but has ${receiver.typ}")
              }
              Datalog.NoPath(Datalog.Var(src), srcTy, Datalog.NamedLink(nodeType, field.name), termIsSource = true)
          }
          (Seq(), econstraints :+ path)

        case _ => throw new IllegalArgumentException("Cannot support in Undef " + exp)
      }

    case Constant(lit) =>
      transLiteral(lit) match {
        case None => (Seq(), Seq())
        case Some(gplit) =>
          val tmpVar = gensym.fresh("tmp")
          val compare = Datalog.Compare(Datalog.EqComparator, Datalog.Var(tmpVar), Datalog.Constant(gplit))
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
      (Seq(trg), transPathAccess(pa, Datalog.Var(trg)))

    case funcall@Call(name, args, transitive) =>
      val (inVars, outVars, constraints) = transCallArgs(funcall, args)
      val allvars = (inVars ++ outVars).map(Datalog.Var)
      val call = Datalog.Call(name.name, allvars, transitive, neg = false)
      (outVars, constraints :+ call)

    case Count(funcall) =>
      val (inVars, outVars, constraints) = transCallArgs(funcall, funcall.args)
      val countVar = gensym.fresh("count")
      val allvars = (inVars ++ outVars).map(Datalog.Var)
      val countConstraint = Datalog.Computed(Datalog.Var(countVar), Datalog.CountAggregation(funcall.name.name, allvars))
      (Seq(countVar), constraints :+ countConstraint)

    case eval@Eval(code) =>
      import scala.meta._
      val params = eval.params.getOrElse(Seq())

      val evalVar = gensym.fresh("eval")
      val argConstraints = ListBuffer[Datalog.Atom]()
      val paramsTyped = params.map { param =>
        param"${Term.Name(param.name.name)}: ${param.typ.get.asScala}"
      }.toList
      val args = params.map { param =>
        param.target.getOrElse(throw new IllegalArgumentException(s"Unbound eval parameter $param")) match {
          case assign@Assign(Seq(_), exp) if shouldInlineAssign(assign) =>
            // inline exp
            val (Seq(arg), cons) = transExp(exp.ensureCore)
            argConstraints ++= cons
            (Datalog.Var(arg), transType(exp.typ.get))
          case _: Param | _: Values | _: Assign => (Datalog.Var(param.name.name), transType(param.typ.get))
          case target => throw new IllegalArgumentException(s"Unknown eval param target $target for $param")
        }
      }
      val funCode = q"(..$paramsTyped) => {${code.tree}}"
      val resType = eval.typ.getOrElse(throw new IllegalStateException("untyped Eval"))
      val evalConstraint = Datalog.Computed(Datalog.Var(evalVar), Datalog.Evaluation(args, transType(resType), Scala(funCode)))
      (Seq(evalVar), (argConstraints :+ evalConstraint).toSeq)

    case Aggregate(agg, bodies) =>
      val aggCode = tryInlineVar(agg) match {
        case Eval(code) => code
        case _ => throw new IllegalArgumentException(s"Requires aggregation code, but got $agg")
      }

      val funname = gensym.freshGlobal("AggregateCollection")

      val inVars = bodies.flatMap(_.allVars).toMap
      val params = inVars.map(kv => Param(kv._1, kv._2.getOrElse(throw new IllegalArgumentException(s"untyped var ${kv._1} in $exp")))).toSeq
      val allvars = params.map(p => Datalog.Var(p.name.name)) :+ Datalog.Var(gensym.fresh("aggregand"))

      val resultType = exp.typ.getOrElse(throw new IllegalArgumentException("untyped aggregate"))
      val fun = PatternFunction(Seq(), None, Name(funname), params, resultType, bodies)
      generatedPatterns += transform(fun)

      val aggregation = Datalog.CustomAggregation(transType(resultType), None, aggCode, funname, allvars, allvars.size - 1)
      val resultVar = gensym.fresh("tmp")
      val compare = Datalog.Computed(Datalog.Var(resultVar), aggregation)
      (Seq(resultVar), Seq(compare))
  }

  private def shouldInlineAssign(assign: Assign): Boolean =
    assign.names.size == 1 && (assign.exp.typ.contains(TScalaBoolean) || assign.exp.typ.contains(TLiteral.Bool))

  def transCallArgs(call: Call, args: Seq[Expression])(implicit gensym: Gensym): (Seq[String], Seq[String], Seq[Datalog.Atom]) = {
    val (vars, constraints) = args.map(e => transExp(e.ensureCore)).unzip
    val outVars = call.target match {
      case Some(fun@PatternFunction(_, _, _, _, _, _)) => fun.outParams.map { _ =>
        val argVar = gensym.fresh("arg")
        argVar
      }
      case target => throw new IllegalArgumentException(s"Unknown call target $target for $call")
    }
    (vars.flatten, outVars, constraints.flatten)
  }

  def genDefCallConstraint(funcall: Call, args: Seq[Expression], transitive: Boolean, neg: Boolean)(implicit gensym: Gensym): Seq[Datalog.Atom] = {
    val (inVars, outVars, constraints) = transCallArgs(funcall, args)
    val allvars = (inVars ++ outVars).map(Datalog.Var)
    val call = Datalog.Call(funcall.name.name, allvars, transitive, neg)
    constraints :+ call
  }

  def transPathAccess(pathAccess: PathAccess, trg: Datalog.Term)(implicit gensym: Gensym): Seq[Datalog.Atom] = {
    val receiver = pathAccess.receiver
    val (Seq(src), econstraints) = transExp(receiver.ensureCore)
    val srcTy = transType(receiver.typ.getOrElse(throw new IllegalArgumentException(s"Cannot compile path access with untyped receiver $receiver")))
    val trgTy = transType(pathAccess.typ.getOrElse(throw new IllegalArgumentException(s"Cannot compile untyped $pathAccess")))
    val path = pathAccess.link match {
      case ParentLink =>
        Datalog.Path(Datalog.Var(src), srcTy, Datalog.ParentLink, trg, trgTy)
      case ChildrenLink =>
        Datalog.Path(trg, trgTy, Datalog.ParentLink, Datalog.Var(src), srcTy)
      case NextLink =>
        Datalog.Path(Datalog.Var(src), srcTy, Datalog.NextLink, trg, trgTy)
      case PreviousLink =>
        Datalog.Path(trg, trgTy, Datalog.NextLink, Datalog.Var(src), srcTy)
      case SizeLink =>
        Datalog.Path(Datalog.Var(src), srcTy, Datalog.SizeLink, trg, trgTy)
      case NamedLink(field) =>
        val nodeType = receiver.typ match {
          case Some(node@TNode(_)) => Datalog.TNode(getFqnNode(node).name)
          case _ => throw new IllegalArgumentException(s"$receiver should have node type, but has ${receiver.typ}")
        }
        Datalog.Path(Datalog.Var(src), srcTy, Datalog.NamedLink(nodeType, field.name), trg, trgTy)
    }
    econstraints :+ path
  }

  def transLiteral(lit: Literal): Option[Datalog.Literal] = lit match {
    case UnitLiteral => None
    case IntLiteral(v) => Some(Datalog.IntLiteral(v))
    case LongLiteral(v) => Some(Datalog.LongLiteral(v))
    case DoubleLiteral(v) => Some(Datalog.DoubleLiteral(v))
    case StringLiteral(v) => Some(Datalog.StringLiteral(v))
    case BooleanLiteral(v) => Some(Datalog.BooleanLiteral(v))
  }

  def getFqnNode(node: TNode): TNode =
    node.target.getOrElse(throw new IllegalArgumentException(s"Could not resolve fully qualified name of $node"))
}
