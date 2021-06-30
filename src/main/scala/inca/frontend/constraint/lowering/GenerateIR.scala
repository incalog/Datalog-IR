package inca.frontend.constraint.lowering

import inca.backend.hints.MagicSetHints
import inca.backend.ir.IR
import inca.frontend.constraint.core._
import inca.util.{Gensym, Scala}

import scala.collection.mutable.ListBuffer
import scala.meta.{Name => _, Type => _}

class GenerateIR {
  val gensym = new Gensym(Iterable.empty)

  private val generatedPatterns = ListBuffer[IR.Pattern]()

  def transformModule(module: Module): IR.Module = {
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
    IR.Module(name.name, imports.map(_.name.name), generatedPatterns.toList, scalaContent.map(Scala.apply))
  }

  def transform(fun: PatternFunction): IR.Pattern = {
    rewriteFunction(fun)
  }

  def transType(typ: Type): IR.Type = typ match {
    case TAny => IR.TAny
    case TLiteral(lit) => IR.TLiteral(lit)
    case TAnyLinked => IR.TAnyLinked
    case node: TNode => IR.TNode(getFqnNode(node).name)
    case TList(ty) => IR.TList(transType(ty).asInstanceOf[IR.TLinked])
    case TScala(ty) => IR.TScala(ty)
  }


  def rewriteFunction(fun: PatternFunction): IR.Pattern = {
    gensym.register(fun.freeVars.keys.map(_.name))
    gensym.register(fun.boundNames.map(_.name))

    val vis = fun.vis.map {
      case Private => IR.Private
    }

    val params = fun.params.map { param => IR.Param(param.name.name, transType(param.typ)) }
    val outParams = fun.outParams.map { out =>
      val name = gensym.fresh("out")
      IR.Param(name, transType(out))
    }
    val outVars = outParams.map(_.name)

    val bodies = fun.bodies.flatMap(b => transBody(b, outVars)(gensym))
    val pat = IR.Pattern(vis, fun.name.name, params ++ outParams, bodies)
    if (fun.hasAnnotation(MainFunctionAnno.key))
      pat.addHint(MagicSetHints.Main(params.map(_ => true) ++ outParams.map(_ => false)))
    pat
  }

  def generateCompareConstraints(comp: IR.Comparator)(lhs: Seq[String], rhs: Seq[String]): Seq[IR.Atom] = {
    if (lhs.size != rhs.size)
      throw new IllegalArgumentException("Cannot create equalites for different sized variable lists")
    (lhs zip rhs).map { case (l, r) => IR.Compare(comp, IR.Var(l), IR.Var(r)) }
  }

  val genEqs: (Seq[String], Seq[String]) => Seq[IR.Atom] = generateCompareConstraints(IR.EqComparator)
  val genNeqs: (Seq[String], Seq[String]) => Seq[IR.Atom] = generateCompareConstraints(IR.NeqComparator)

  type Res = (Seq[String], Seq[IR.Atom])

  def transBody(alt: Body, outVars: Seq[String])(implicit gensym: Gensym): Option[IR.Body] = {
    try {
      val constraints = alt.stmts.flatMap { s =>
        transStatement(s.ensureCore, outVars)
      }
      Some(IR.Body(constraints))
    } catch {
      case IR.BodyMustFail => None
    }
  }

  def transStatement(stmt: CoreStatement, outVars: Seq[String])(implicit gensym: Gensym): Seq[IR.Atom] = stmt match {
    case Values(name, typ) =>
      (Seq(IR.HasType(IR.Var(name.name), transType(typ))))

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
      else throw IR.BodyMustFail
    case Assert(cond) => transExp(cond.ensureCore) match {
      case (Nil, cons) =>
        cons
      case (Seq(v), cons) =>
        cons :+ IR.Compare(IR.EqComparator, IR.Var(v), IR.Constant(IR.BooleanLiteral(true)))
    }

    case Yield(exp) =>
      val (vars, constraints) = transExp(exp.ensureCore)
      constraints ++ genEqs(vars, outVars)

    case FailStatement =>
      throw IR.BodyMustFail
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
      (Seq(), constraints :+ IR.HasType(IR.Var(vars.head), transType(typ)))

    case NotInstanceOf(exp, typ) =>
      val (vars, constraints) = transExp(exp.ensureCore)
      if (vars.size != 1) throw new IllegalArgumentException("Number of variables of exp of instance of need to be 1")
      (Seq(), constraints :+ IR.NotHasType(IR.Var(vars.head), transType(typ)))

    case Cast(exp, typ) =>
      val (vars, constraints) = transExp(exp.ensureCore)
      if (vars.size != 1) throw new IllegalArgumentException("Number of variables of exp of instance of need to be 1")
      val v = vars.head
      (Seq(v), constraints :+ IR.HasType(IR.Var(v), transType(typ)))

    case Def(exp) =>
      exp match {
        case call@Call(name, args, transitive) =>
          (Seq(), genDefCallConstraint(call, args, transitive, neg = false))
        case pa: PathAccess =>
          val tmp = gensym.fresh("_")
          (Seq(), transPathAccess(pa, IR.Var(tmp)))
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
              IR.NoPath(IR.Var(src), srcTy, IR.ParentLink, termIsSource = true)
            case ChildrenLink =>
              IR.NoPath(IR.Var(src), srcTy, IR.ParentLink, termIsSource = false)
            case NextLink =>
              IR.NoPath(IR.Var(src), srcTy, IR.NextLink, termIsSource = true)
            case PreviousLink =>
              IR.NoPath(IR.Var(src), srcTy, IR.NextLink, termIsSource = false)
            case SizeLink =>
              IR.NoPath(IR.Var(src), srcTy, IR.SizeLink, termIsSource = true)
            case NamedLink(field) =>
              val nodeType = receiver.typ match {
                case Some(node@TNode(_)) =>
                  IR.TNode(getFqnNode(node).name)
                case _ => throw new IllegalArgumentException(s"$receiver should have node type, but has ${receiver.typ}")
              }
              IR.NoPath(IR.Var(src), srcTy, IR.NamedLink(nodeType, field.name), termIsSource = true)
          }
          (Seq(), econstraints :+ path)

        case _ => throw new IllegalArgumentException("Cannot support in Undef " + exp)
      }

    case Constant(lit) =>
      transLiteral(lit) match {
        case None => (Seq(), Seq())
        case Some(gplit) =>
          val tmpVar = gensym.fresh("tmp")
          val compare = IR.Compare(IR.EqComparator, IR.Var(tmpVar), IR.Constant(gplit))
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
      (Seq(trg), transPathAccess(pa, IR.Var(trg)))

    case funcall@Call(name, args, transitive) =>
      val (inVars, outVars, constraints) = transCallArgs(funcall, args)
      val allvars = (inVars ++ outVars).map(IR.Var)
      val call = IR.Call(name.name, allvars, transitive, neg = false)
      (outVars, constraints :+ call)

    case Count(funcall) =>
      val (inVars, outVars, constraints) = transCallArgs(funcall, funcall.args)
      val countVar = gensym.fresh("count")
      val allvars = (inVars ++ outVars).map(IR.Var)
      val countConstraint = IR.Computed(IR.Var(countVar), IR.CountAggregation(funcall.name.name, allvars))
      (Seq(countVar), constraints :+ countConstraint)

    case eval@Eval(code) =>
      import scala.meta._
      val params = eval.params.getOrElse(Seq())

      val evalVar = gensym.fresh("eval")
      val argConstraints = ListBuffer[IR.Atom]()
      val paramsTyped = params.map { param =>
        param"${Term.Name(param.name.name)}: ${param.typ.get.asScala}"
      }.toList
      val args = params.map { param =>
        param.target.getOrElse(throw new IllegalArgumentException(s"Unbound eval parameter $param")) match {
          case assign@Assign(Seq(_), exp) if shouldInlineAssign(assign) =>
            // inline exp
            val (Seq(arg), cons) = transExp(exp.ensureCore)
            argConstraints ++= cons
            (IR.Var(arg), transType(exp.typ.get))
          case _: Param | _: Values | _: Assign => (IR.Var(param.name.name), transType(param.typ.get))
          case target => throw new IllegalArgumentException(s"Unknown eval param target $target for $param")
        }
      }
      val funCode = q"(..$paramsTyped) => {${code.tree}}"
      val resType = eval.typ.getOrElse(throw new IllegalStateException("untyped Eval"))
      val evalConstraint = IR.Computed(IR.Var(evalVar), IR.Evaluation(args, transType(resType), Scala(funCode)))
      (Seq(evalVar), (argConstraints :+ evalConstraint).toSeq)

    case Aggregate(agg, bodies) =>
      val aggCode = tryInlineVar(agg) match {
        case Eval(code) => code
        case _ => throw new IllegalArgumentException(s"Requires aggregation code, but got $agg")
      }

      val funname = gensym.freshGlobal("AggregateCollection")

      val inVars = bodies.flatMap(_.freeVars).toMap
      val params = inVars.map(kv => Param(kv._1, kv._2.getOrElse(throw new IllegalArgumentException(s"untyped var ${kv._1} in $exp")))).toSeq
      val allvars = params.map(p => IR.Var(p.name.name)) :+ IR.Var(gensym.fresh("aggregand"))

      val resultType = exp.typ.getOrElse(throw new IllegalArgumentException("untyped aggregate"))
      val fun = PatternFunction(Seq(), None, Name(funname), params, resultType, bodies)
      generatedPatterns += transform(fun)

      val aggregation = IR.CustomAggregation(transType(resultType), None, aggCode, funname, allvars, allvars.size - 1)
      val resultVar = gensym.fresh("tmp")
      val compare = IR.Computed(IR.Var(resultVar), aggregation)
      (Seq(resultVar), Seq(compare))
  }

  private def shouldInlineAssign(assign: Assign): Boolean =
    assign.names.size == 1 && (assign.exp.typ.contains(TScalaBoolean) || assign.exp.typ.contains(TLiteral.Bool))

  def transCallArgs(call: Call, args: Seq[Expression])(implicit gensym: Gensym): (Seq[String], Seq[String], Seq[IR.Atom]) = {
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

  def genDefCallConstraint(funcall: Call, args: Seq[Expression], transitive: Boolean, neg: Boolean)(implicit gensym: Gensym): Seq[IR.Atom] = {
    val (inVars, outVars, constraints) = transCallArgs(funcall, args)
    val allvars = (inVars ++ outVars).map(IR.Var)
    val call = IR.Call(funcall.name.name, allvars, transitive, neg)
    constraints :+ call
  }

  def transPathAccess(pathAccess: PathAccess, trg: IR.Term)(implicit gensym: Gensym): Seq[IR.Atom] = {
    val receiver = pathAccess.receiver
    val (Seq(src), econstraints) = transExp(receiver.ensureCore)
    val srcTy = transType(receiver.typ.getOrElse(throw new IllegalArgumentException(s"Cannot compile path access with untyped receiver $receiver")))
    val trgTy = transType(pathAccess.typ.getOrElse(throw new IllegalArgumentException(s"Cannot compile untyped $pathAccess")))
    val path = pathAccess.link match {
      case ParentLink =>
        IR.Path(IR.Var(src), srcTy, IR.ParentLink, trg, trgTy)
      case ChildrenLink =>
        IR.Path(trg, trgTy, IR.ParentLink, IR.Var(src), srcTy)
      case NextLink =>
        IR.Path(IR.Var(src), srcTy, IR.NextLink, trg, trgTy)
      case PreviousLink =>
        IR.Path(trg, trgTy, IR.NextLink, IR.Var(src), srcTy)
      case SizeLink =>
        IR.Path(IR.Var(src), srcTy, IR.SizeLink, trg, trgTy)
      case NamedLink(field) =>
        val nodeType = receiver.typ match {
          case Some(node@TNode(_)) => IR.TNode(getFqnNode(node).name)
          case _ => throw new IllegalArgumentException(s"$receiver should have node type, but has ${receiver.typ}")
        }
        IR.Path(IR.Var(src), srcTy, IR.NamedLink(nodeType, field.name), trg, trgTy)
    }
    econstraints :+ path
  }

  def transLiteral(lit: Literal): Option[IR.Literal] = lit match {
    case UnitLiteral => None
    case IntLiteral(v) => Some(IR.IntLiteral(v))
    case LongLiteral(v) => Some(IR.LongLiteral(v))
    case DoubleLiteral(v) => Some(IR.DoubleLiteral(v))
    case StringLiteral(v) => Some(IR.StringLiteral(v))
    case BooleanLiteral(v) => Some(IR.BooleanLiteral(v))
  }

  def getFqnNode(node: TNode): TNode =
    node.target.getOrElse(throw new IllegalArgumentException(s"Could not resolve fully qualified name of $node"))
}
