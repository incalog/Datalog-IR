package inca.frontend.core

import inca.backend.ir.GP
import inca.frontend.core.Core.DataOp
import inca.util.Gensym
import inca.util.Meta.Scala

import scala.collection.mutable.ListBuffer
import scala.meta.{Name => _, Type => _, _}

object CompileToGP {

  case object BodyMustFail extends Exception


  def transformModule(module: Module): GP.Module = {
    // construct map Name => Fun
    val Module(name, imports, contents) = module

    val patterns = ListBuffer[GP.Pattern]()
    val stats = ListBuffer[Scala[meta.Stat]]()
    contents.foreach {
      case fun: PatternFunction => patterns += transform(fun)
      case stat: ScalaStatement => stats += stat
    }

    GP.Module(name.name, imports.map(_.name.name), patterns.toList, stats.toList)
  }

  def transform(fun: PatternFunction): GP.Pattern = {
    // TODO meta analysis negation in recusion
    rewriteFunction(fun)
  }

  def transType(typ: Type): GP.Type = typ match {
    case TAny => GP.TAny
    case TBool => GP.TBool
    case TInt => GP.TInt
    case TLong => GP.TLong
    case TDouble => GP.TDouble
    case TString => GP.TString
    case TAnyLinked => GP.TAnyLinked
    case TNode(name) => GP.TNode(name)
    case TList(ty) => GP.TList(transType(ty).asInstanceOf[GP.TLinked])
//    case dt: DataType => GP.TDataType(resolveDataType(dt))
  }


  def rewriteFunction(fun: PatternFunction): GP.Pattern = {
    val gensym = new Gensym(fun.freeVars.keys.map(_.name))
    gensym.register(fun.boundNames.map(_.name))

    val vis = fun.vis.map {
      case Private => GP.Private
      case Public => GP.Public
    }

    val params = fun.params.map { param => GP.Param(param.name.name, transType(param.typ)) }
    val outParams = fun.outParams.map { param =>
      val name =
        if (param.name.isDefined) param.name.get.name
        else gensym.fresh("out")
      GP.Param(name, transType(param.typ))
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

  case class Binding(typ: Type, exp: Option[CoreExpression], index: Option[Int]) {
    def shouldInline: Boolean = typ match {
      case TBool => exp.nonEmpty && index.isEmpty
      case _ => false
    }
  }

  def transBody(alt: Body, outVars: Seq[String])(implicit gensym: Gensym): Option[GP.Body] = gensym.scoped {
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

    case Assign(names, exp) =>
      if (exp.typ.isEmpty)
        throw new IllegalArgumentException(s"Cannot compile untyped assignment $stmt")

      gensym.register(names.map(_.name))
      val expTy = exp.typ.get
      val bindings = names match {
        case Nil => Seq()
        case Seq(_) => Seq(Binding(expTy, Some(exp.ensureCore), None))
        case ns => ns.zipWithIndex.map { case (n, i) =>
          val ty = expTy match {
            case tup@TTuple(ts) =>
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
      if (shouldInline) {
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


  def transExp(cond: CoreExpression)(implicit gensym: Gensym): Res = cond match {
    case v@Var(name) =>
      v.target.getOrElse(throw new IllegalArgumentException(s"Unbound variable $name")) match {
        case assign@Assign(Seq(_), exp) if shouldInlineAssign(assign) =>
          // inline exp
          transExp(exp.ensureCore)
        case _: Param | _: Values | _: Assign => (Seq(name.name), Seq())
        case target => throw new IllegalArgumentException(s"Unknown variable target $target for $v")
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
      // TODO why is there a distinction between exp and non exp args in MPS impl?
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

    case eval@Eval(params, code) =>
      import scala.meta._

      val evalVar = gensym.fresh("eval")
      val argConstraints = ListBuffer[GP.Constraint]()
      val paramsTyped = params.map { param =>
        param"${Term.Name(param.name.name)}: ${scalaType(param.typ.get)}"
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
      val evalConstraint = GP.Computed(GP.Var(evalVar), GP.Evaluation(args, transType(resType), funCode))
      (Seq(evalVar), (argConstraints :+ evalConstraint).toSeq)

    case Aggregate(init, join, unjoin, funcall) =>
      if (!join.isAssociative || !join.isCommutative)
        throw new IllegalArgumentException(s"Can only compile aggregations with join operators that are associative and commutative")

      val resultType = funcall.typ.getOrElse(throw new IllegalArgumentException(s"Cannot compile aggregation over untyped $funcall"))
      resultType match {
        case TTuple(ts) if ts.isEmpty => throw new IllegalArgumentException(s"Cannot aggregate over functions with Unit result type")
        case TTuple(ts) if ts.size > 1 => throw new IllegalArgumentException(s"Cannot aggregate over functions with multiple results $ts")
        case _ => // nothing
      }

      val (inVars, Seq(outVar), constraints) = transCallArgs(funcall, funcall.args)
      val allvars = (inVars :+ outVar).map(GP.Var)
      val initOp = resolveDataOp(init)
      val joinOp = resolveDataOp(join)
      val invOp = unjoin.map(resolveDataOp)
      val aggregation = GP.CustomAggregation(transType(resultType), initOp, joinOp, invOp, funcall.name.name, allvars, allvars.size - 1)

      val resultVar = gensym.fresh("tmp")
      val compare = GP.Computed(GP.Var(resultVar), aggregation)
      (Seq(resultVar), constraints :+ compare)
  }

  private def shouldInlineAssign(assign: Assign): Boolean =
    assign.names.size == 1 && assign.exp.typ.contains(TBool)

  def transCallArgs(call: Call, args: Seq[Expression])(implicit gensym: Gensym): (Seq[String], Seq[String], Seq[GP.Constraint]) = {
    val (vars, constraints) = args.map(e => transExp(e.ensureCore)).unzip
    val outVars = call.target match {
      case Some(PatternFunction(_, _, _, outParams, _)) => outParams.map { _ =>
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

  def scalaAnnoString(typ: Type): String = typ match {
    case TAny => "Any"
    case TBool => "Boolean"
    case TInt => "Int"
    case TLong => "Long"
    case TDouble => "Double"
    case TString => "String"
//    case dt: DataType => resolveDataType(dt)
    case _: TLinked => "truechange.URI"
  }

  def scalaType(typ: Type): meta.Type = typ match {
    case TAny => t"Any"
    case TBool => t"Boolean"
    case TInt => t"Int"
    case TLong => t"Long"
    case TDouble => t"Double"
    case TString => t"String"
//    case dt: DataType => t"${resolveDataType(dt)}"
    case _: TLinked => t"truechange.URI"
  }

  def resolveDataOp(op: DataOp): String = op.qualifier match {
    case Some(Name(q)) if q.isEmpty => op.operation.name
    case Some(q) => s"$q.${op.operation}"
    case None => throw new IllegalArgumentException(s"TODO resolve unqualified data op calls")
  }

//  def resolveDataType(dt: DataType): String = dt.qualifier match {
//    case Some(Name(q)) if q.isEmpty => dt.name.name
//    case Some(q) => s"$q.${dt.name}"
//    case None => throw new IllegalArgumentException(s"TODO resolve unqualified data types")
//  }
}
