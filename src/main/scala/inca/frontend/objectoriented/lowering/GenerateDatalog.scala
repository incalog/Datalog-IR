package inca.frontend.objectoriented.lowering

import inca.backend.hints.{DataHints, MagicSetHints}
import inca.backend.hints.MagicSetHints.{FixedAdornment, IgnoreCall, NoInputRelation}
import inca.backend.ir.Datalog
import inca.frontend.objectoriented.core._
import inca.runtime.data.MockURI
import inca.util.Scala.{symbolOf, typeOf}
import inca.util.{Gensym, Scala, TupleOps}

import scala.:+
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.meta.Term
import scala.meta.quasiquotes._

object GenerateDatalog {
  def transformModule(module: Module): Datalog.Module =
    new GenerateDatalog(module).transModule()

  def transformModules(modules: Seq[Module]): Seq[Datalog.Module] =
    modules.map(transformModule)
}

class GenerateDatalog(module: Module) {
  private val gensym: Gensym = new Gensym(Iterable.empty)

  private val generatedPatterns = ListBuffer[Datalog.Pattern]()

  def transModule(): Datalog.Module = {
    val Module(name, imports, classes) = module
    gensym.register(module.usedModuleNames.map(_.raw))
    gensym.register(module.usedClassNames.map(_.raw))

    classes.foreach(c => generatedPatterns ++= transClass(c))

    Datalog.Module(
      name.raw,
      imports.map(_.name.raw),
      generatedPatterns.toList,
      Seq()
    )
  }

  private def transClass(classDef: ClassDef): Seq[Datalog.Pattern] = {
    val className = classDef.name.raw

    val fieldParams = classDef.fields.map(f => Datalog.Param(f.name.raw, transType(f.typ)))
    val thisParam = Datalog.Param("this", transType(classDef.typ))
    val bodies = Seq(transDefaultConstructor(classDef))

    val objectPat = Datalog.Pattern(transVis(classDef.vis), className, thisParam +: fieldParams, bodies)
    val methodPats = classDef.methods.map(m => transMethod(classDef, m))
    val castPat = Datalog.Pattern(None, "cast$_" + className, Seq(thisParam), Seq(transCast(classDef)))
      .addHint(MagicSetHints.NoInputRelation)

    val hasTypeOutParam = Datalog.Param("hasType", Datalog.TScalaBoolean)
    val instanceOfBodies = transInstanceOf(classDef)
    val instanceOfParams = Seq(thisParam, hasTypeOutParam)
    val instanceOfPat = Datalog.Pattern(None, "instanceOf$_" + className, instanceOfParams, instanceOfBodies)

    objectPat +: methodPats :+ castPat :+ instanceOfPat
  }

  val oMockURI: meta.Term = symbolOf(MockURI)
  val tyMockURI: meta.Type = typeOf[MockURI]

  private def transInstanceOf(classDef: ClassDef): Seq[Datalog.Body] = gensym.scoped {
    val thisVar = Datalog.Var("this")
    val outVar = Datalog.Var("hasType")

    val fieldVars = classDef.fields.map(_ => Datalog.Var("_"))
    val extObject = Datalog.Call(classDef.name.raw, thisVar +: fieldVars)
      .addHint(MagicSetHints.IgnoreCall)
      .addHint(MagicSetHints.FixedAdornment(false +: fieldVars.map(_ => true)))
    val notExtObject = Datalog.Call(classDef.name.raw, thisVar +: fieldVars, neg = true)
      .addHint(MagicSetHints.IgnoreCall)
      .addHint(MagicSetHints.FixedAdornment(false +: fieldVars.map(_ => true)))

    Seq(
      Datalog.Body(Seq(extObject, Datalog.Eq(outVar, Datalog.True))),
      Datalog.Body(Seq(notExtObject, Datalog.Eq(outVar, Datalog.False)))
    )
  }

  private def transCast(classDef: ClassDef): Datalog.Body = gensym.scoped {
    val thisVar = Datalog.Var("this")
    val fieldVars = classDef.fields.map(_ => Datalog.Var("_"))
    val readObject = Datalog.Call(classDef.name.raw, thisVar +: fieldVars)
      .addHint(MagicSetHints.IgnoreCall)
      .addHint(MagicSetHints.FixedAdornment(false +: fieldVars.map(_ => true)))
    Datalog.Body(Seq(readObject))
  }

  private def transDefaultConstructor(classDef: ClassDef): Datalog.Body = gensym.scoped {
    val params = classDef.fields.map(f => Datalog.Param(f.name.raw, transType(f.typ)))
    val constrScalaFun = Term.Function(
      classDef.fields.map(p => Term.Param(Nil, Term.Name(p.name.raw), Some(p.typ.asScala), None)).toList,
      q"""$oMockURI(${classDef.name.raw}, ..${params.map(p => Term.Name(p.name)).toList})"""
    )
    val thisVar = Datalog.Var("this")

    val thisCons =
      Datalog.Computed(
        thisVar,
        Datalog.Evaluation(
          classDef.fields.map(p => Datalog.Var(p.name.raw) -> transType(p.typ)),
          transType(classDef.typ),
          Scala(constrScalaFun)))

    // TODO: pass uid to super constructor... how ?
    /*val superCons = classDef.parentClassRefs.map { ref =>
      val superClassDef = ref.target.get
      val args = superClassDef.fields.map(_ => Datalog.Var("_"))
      Datalog.Call(superClassDef.name.raw, thisVar +: args)
    }*/

    //Datalog.Body(thisCons +: superCons)
    Datalog.Body(Seq(thisCons))
  }

  // TODO: Implement this function when mutability is available + Move all fields to their own tables ?
  /*private def transConstructor(classDef: ClassDef, constructorDef: MethodDef): Datalog.Pattern = gensym.scoped {
    gensym.register(constructorDef.vars.keys.map(_.raw) + "this")

    val params = constructorDef.params.map(f => Datalog.Param(f.name.raw, transType(f.typ)))
    val constrScalaFun = Term.Function(
      constructorDef.params.map(p => Term.Param(Nil, Term.Name(p.name.raw), Some(p.typ.asScala), None)).toList,
      q"""$oMockURI(${classDef.name.raw}, ..${params.map(p => Term.Name(p.name)).toList})"""
    )
    val thisVar = Datalog.Var("this")

    val thisCons =
      Datalog.Computed(
        thisVar,
        Datalog.Evaluation(
          constructorDef.params.map(p => Datalog.Var(p.name.raw) -> transType(p.typ)),
          transType(classDef.typ),
          Scala(constrScalaFun)))

    val bodyRes = transStatements(constructorDef.body)
    val bodies = for ((optReturn, cons) <- bodyRes) yield {
      if (optReturn.isDefined)
        throw new IllegalStateException(s"Constructor of class ${classDef.name} may not call return")
          Datalog.Body(thisCons +: cons)
    }

    val argParams = constructorDef.params.map { case Param(name, typ) =>
      Datalog.Param(name.raw, transType(typ))
    }
    Datalog.Pattern(None, classDef.name.raw, argParams, bodies)
  }*/

  private def transMethod(classDef: ClassDef, methodDef: MethodDef): Datalog.Pattern = gensym.scoped {
    gensym.register(methodDef.vars.keys.map(_.raw) + "this")

    val qualifiedName = classDef.name + "$" + methodDef.name.raw
    val isMain = methodDef.annos.contains(MainAnnotation)

    val thisParam = Datalog.Param("this", transType(classDef.typ))
    val argParams = methodDef.params.map { case Param(name, typ) =>
      Datalog.Param(name.raw, transType(typ))
    }
    val returnParams =
      if (methodDef.returnsUnit)
        Seq()
      else
        Seq(Datalog.Param(gensym.fresh("return"), transType(methodDef.outType)))

    val bodyRes = transStatements(methodDef.body)
    val bodies = for ((optReturn, cons) <- bodyRes) yield {
      if (!methodDef.returnsUnit && optReturn.isEmpty)
        throw new IllegalStateException(s"Method ${classDef.name}.${methodDef.name} must call return")
      val returnCons = returnParams.zip(optReturn.getOrElse(Seq())).map { case (p, t) =>
        Datalog.Eq(Datalog.Var(p.name), t)
      }
      Datalog.Body(cons ++ returnCons)
    }

    if (isMain)
      Datalog.Pattern(transVis(methodDef.vis), qualifiedName, argParams ++ returnParams, bodies)
        .addHint(MagicSetHints.Main(argParams.map(_ => true) ++ returnParams.map(_ => false)))
    else
      Datalog.Pattern(transVis(methodDef.vis), qualifiedName, thisParam +: (argParams ++ returnParams), bodies)
  }

  type Constraints = Seq[Datalog.Atom]
  type Tuple = Seq[Datalog.Term]
  type Alternatives[A] = Seq[A]

  type ExpRes = Alternatives[(Tuple, Constraints)]
  type StmRes = Alternatives[(Option[Tuple], Constraints)]

  private def transStatements(stmts: Seq[Statement]): StmRes = stmts match {
    case Nil => Seq((None, Seq()))
    case s::rest =>
      val alternatives = for ((sReturn, sConstraints) <- transStatement(s)) yield {
        if (sReturn.isDefined)
          Seq((sReturn, sConstraints))
        else
          transStatements(rest).map(res => (res._1, sConstraints ++ res._2))
      }
      alternatives.flatten
  }

  private def transStatement(stmt: Statement): StmRes = stmt match {
    case ExprStmt(expression) =>
      for ((_, cons) <- transExpression(expression))
        yield (None, cons)
    case ReturnStmt(expression) =>
      for ((tup, cons) <- transExpression(expression))
        yield (Some(tup), cons)
    case VarDeclareStmt(name, typ, Some(expression), true) =>
      for ((Seq(term), cons) <- transExpression(expression))
        yield (None, cons :+ Datalog.Eq(Datalog.Var(name.raw), term))
    case IfStmt(cnd, thn, els) =>
      val cndTrans = transExpression(cnd)
      val thnTrans = transStatements(thn)
      val elsTrans = transStatements(els)
      val thnRes: StmRes =
        for ((Seq(cndTerm), cndCons) <- cndTrans;
             (thnTerm, thnCons) <- thnTrans)
        yield (thnTerm, cndCons ++ Seq(Datalog.Eq(cndTerm, Datalog.True)) ++ thnCons)
      val elsRes: StmRes =
        for ((Seq(cndTerm), cndCons) <- cndTrans;
             (elsTerm, elsCons) <- elsTrans)
        yield (elsTerm, cndCons ++ Seq(Datalog.Eq(cndTerm, Datalog.False)) ++ elsCons)
      thnRes ++ elsRes
    /*case FieldAssignStmt(recv, name, expression) => ???
    case VarAssignStmt(targetName, expression) => ???
    */
    case _ =>
      throw new RuntimeException("Mutability is no yet supported")
  }

  private def transExpression(expression: Expression): ExpRes = expression match {
    case VarReadExpr(name) =>
      val expTyp = expression.typ.getOrElse(throw new IllegalArgumentException(s"Untyped expression $expression"))
      Seq((flattenVars(name, expTyp).map(_._1), Seq()))

    case FieldReadExpr(recv, targetName) =>
      // TODO: We might use the fieldDef target here instead to allow inheritance of attributes
      val classType = recv.typ.getOrElse(throw new IllegalArgumentException(s"Untyped expression $expression")) match {
        case t: TClass => t
        case _ => throw new IllegalArgumentException(s"Illegal field lookup on expression $expression")
      }
      val classDef = classType.ref.target.getOrElse(throw new IllegalArgumentException(s"Unresolved class $classType"))

      for ((Seq(term), cons) <- transExpression(recv)) yield {
        val fieldReadVar = Datalog.Var(gensym.fresh("fieldRead"))
        val fieldVars = classDef.fields.map(f =>
          if (f.name == targetName)
            fieldReadVar
          else {
            // TODO: Why is _ a problem ?
            //Datalog.Var(gensym.fresh("_"))
            Datalog.Var("_")
          }
        )
        val fieldReadCall = Datalog.Call(classType.ref.name.raw, term +: fieldVars)
          .addHint(MagicSetHints.IgnoreCall)
          .addHint(MagicSetHints.FixedAdornment(false +: fieldVars.map(_ => true)))
        (Seq(fieldReadVar), cons :+ fieldReadCall)
      }

    case ConstructorExpr(classRef, args) =>
      val constructedVar = Datalog.Var(gensym.fresh("new"))
      val argRes = args.map(e => transExpression(e))

      // create single call constraint when no arguments passed
      if (argRes.isEmpty)
        return Seq((Seq(constructedVar), Seq(Datalog.Call(classRef.name.raw, Seq(constructedVar)))))

      for (tups <- TupleOps.cartesianProduct(argRes)) yield {
        val (argTerms, argCons) = tups.unzip
        (Seq(constructedVar), argCons.flatten ++ Seq(Datalog.Call(classRef.name.raw, constructedVar +: argTerms.flatten)))
      }

    case methodCallExp@MethodCallExpr(recv, fun, args) =>
      val outVar = Datalog.Var(gensym.fresh("methodCall"))
      val argRes = args.map(e => transExpression(e))

      // method might be implemented inside the parent, that means use the classDef of the resolved methodDef
      val methodDef = methodCallExp.target.getOrElse(throw new IllegalArgumentException(s"Unresolved method $methodCallExp"))
      val classDef = methodDef.classDef.getOrElse(throw new IllegalArgumentException(s"Unresolved classDef for method $methodDef"))
      val qualifiedName = classDef.name.raw + "$" + fun

      val transRecv = for ((Seq(term), cons) <- transExpression(recv)) yield {
        if (argRes.isEmpty)
          return Seq((Seq(outVar), cons :+ Datalog.Call(qualifiedName, term +: Seq(outVar))))
        for (tups <- TupleOps.cartesianProduct(argRes)) yield {
          val (argTerms, argCons) = tups.unzip
          (Seq(outVar), cons ++ argCons.flatten ++ Seq(Datalog.Call(qualifiedName, term +: argTerms.flatten :+ outVar)))
        }
      }
      transRecv.flatten

    case TypeCastExpr(recv, toTyp) =>
      for ((Seq(eTerm), eCons) <- transExpression(recv)) yield {
        val instanceCall = Datalog.Call("cast$_" + toTyp.toString, Seq(eTerm))
        (Seq(eTerm), eCons :+ instanceCall)
      }

    case InstanceOfExpr(recv, ofTyp) =>
      for ((Seq(eTerm), eCons) <- transExpression(recv)) yield {
        val outVar = Datalog.Var(gensym.fresh("instanceOf"))
        val instanceOfCall = Datalog.Call("instanceOf$_" + ofTyp.toString, Seq(eTerm, outVar))
        (Seq(outVar), eCons :+ instanceOfCall)
      }

    /*
    case SuperExpr(args) => ???
    case NullExpr() => ???
    case TupleExpr(exps) => ???*/
    case BaseLitExpr(code) =>
      import scala.meta._
      val evalOut = Datalog.Var(gensym.fresh("lit"))
      val resType = expression.typ.getOrElse(throw new IllegalStateException("cannot compile untyped Eval"))
      val funCode = q"() => ${code.tree}"
      val evalConstraint = Datalog.Computed(evalOut, Datalog.Evaluation(Seq(), transType(resType), Scala(funCode)))
      Seq((Seq(evalOut), Seq(evalConstraint)))

    case BaseApplyUnaryExpr(op, exp) =>
      import scala.meta.quasiquotes._
      val expParam = {
        val typ = exp.typ.getOrElse(throw new IllegalStateException(s"Cannot compile call to $op with untyped argument $exp"))
        param"exp: ${typ.asScala}"
      }

      val unary = meta.Term.ApplyUnary(op.tree, meta.Term.Name("exp"))
      val funCode = q"($expParam) => $unary"
      val resType = exp.typ.getOrElse(throw new IllegalStateException("cannot compile untyped base infix application"))

      val expRes = transExpression(exp)
      val evalOut = Datalog.Var(gensym.fresh("eval"))
      for ((Seq(expTerm), expCons) <- expRes) yield {
        val evalConstraint = Datalog.Computed(evalOut,
          Datalog.Evaluation(Seq(expTerm -> transType(exp.typ.get)),
            transType(resType), Scala(funCode)))
        (Seq(evalOut), expCons ++ Seq(evalConstraint))
      }

    case BaseApplyMethodExpr(recv, method, args) =>
      import scala.meta._

      val paramsTyped = (recv +: args.getOrElse(Seq())).zipWithIndex.map { case (arg, ix) =>
        val argTyp = arg.typ.getOrElse(throw new IllegalStateException(s"Cannot compile call of ${recv.prettyprint("")}.$method with untyped argument/reciever $arg"))
        val paramName = gensym.fresh(s"arg$ix")
        param"${Term.Name(paramName)}: ${argTyp.asScala}"
      }.toList
      val scalaArgs = paramsTyped.map(p => Term.Name(p.name.value))
      val methodName = Term.Name(method.raw)
      val funCode =
        if (args.isEmpty)
          q"(..$paramsTyped) => ${scalaArgs.head}.${methodName}"
        else
          q"(..$paramsTyped) => ${scalaArgs.head}.${methodName}(..${scalaArgs.tail})"
      val resType = expression.typ.getOrElse(throw new IllegalStateException("cannot compile untyped Eval"))

      val recvRes = transExpression(recv)
      val argRes = args.getOrElse(Seq()).map(e => transExpression(e))
      val evalOut = Datalog.Var(gensym.fresh("eval"))
      for (tups <- TupleOps.cartesianProduct(recvRes +: argRes)) yield {
        val (argTermss, argCons) = tups.unzip
        val flatArgTerms = argTermss.zip(recv +: args.getOrElse(Nil)).map {
          case (t :: Nil, arg) => (t, transType(arg.typ.get))
          case (_, arg) => throw new IllegalArgumentException(s"Cannot pass tuple argument $arg to ${recv.prettyprint("")}.$method")
        }
        val evalConstraint = Datalog.Computed(evalOut, Datalog.Evaluation(flatArgTerms, transType(resType), Scala(funCode)))
        (Seq(evalOut), argCons.flatten :+ evalConstraint)
      }

    case BaseApplyExpr(fun, args) =>
      import scala.meta._
      val paramsTyped = args.zipWithIndex.map { case (arg, ix) =>
        val argTyp = arg.typ.getOrElse(throw new IllegalStateException(s"Cannot compile call to $fun with untyped argument $arg"))
        val paramName = gensym.fresh(s"arg$ix")
        param"${Term.Name(paramName)}: ${argTyp.asScala}"
      }.toList
      val scalaArgs: List[meta.Term] = paramsTyped.map(p => Term.Name(p.name.value))
      val funCode = q"(..$paramsTyped) => ${fun.tree}(..$scalaArgs)"
      val resType = expression.typ.getOrElse(throw new IllegalStateException("cannot compile untyped Eval"))

      val argRes = args.map(e => transExpression(e))
      val evalOut = Datalog.Var(gensym.fresh("eval"))
      for (tups <- TupleOps.cartesianProduct(argRes)) yield {
        val (argTermss, argCons) = tups.unzip
        val flatArgTerms = argTermss.zip(args).map {
          case (Nil, arg) => throw new IllegalArgumentException(s"Cannot pass empty argument $arg to $fun")
          case (t :: Nil, arg) => (t, transType(arg.typ.get))
          case (_, arg) => throw new IllegalArgumentException(s"Cannot pass tuple argument $arg to $fun")
        }
        val evalConstraint = Datalog.Computed(evalOut, Datalog.Evaluation(flatArgTerms, transType(resType), Scala(funCode)))
        (Seq(evalOut), argCons.flatten :+ evalConstraint)
      }

    /*case BaseApplyInfixExpr(left, op, right)
      if op.tree.value == "++" && left.typ.exists(_.isInstanceOf[TSet]) && right.typ.exists(_.isInstanceOf[TSet]) =>
      transExpression(left) ++ transExpression(right)*/

    /*case BaseApplyInfixExpr(left, op, right)
      if op.tree.value == "&" && left.typ.exists(_.isInstanceOf[TSet]) && right.typ.exists(_.isInstanceOf[TSet]) =>
      val transLeft = transExpression(left)
      val transRight = transExpression(right)

      // create substitution: replace every bound variable in right with freshly generated variable to avoid unwanted nameclashes after merging constraints from left and right
      val boundNamesInRight = right.vars.keys.map(_.name).toSet -- right.freevars.map(_.name.name)
      val freshVarsInRight = boundNamesInRight.map { n => Datalog.Var(gensym.fresh(n)) }
      val boundVarsInRight = boundNamesInRight.map(Datalog.Var)
      val subst = Substitute.fromMap(boundVarsInRight.zip(freshVarsInRight).toMap)

      for ((leftTerms, leftCons) <- transLeft;
           (rightTerms, rightCons) <- transRight) yield {
        // apply substitution created above
        val renamedRightTerms = rightTerms.map(subst.substTerm)
        val renamedRightCons = rightCons.map(subst.substAtom)

        // generate equality constraints to force that constraints of left and right have to hold (X intersect Y implemented as X AND Y)
        val eqTerms = leftTerms.zip(renamedRightTerms).map { case (l, r) => Datalog.Eq(l, r) }
        (leftTerms, leftCons ++ renamedRightCons ++ eqTerms)
      }*/

    case BaseApplyInfixExpr(left, op, right) =>
      import scala.meta.quasiquotes._
      val leftParam = {
        val typ = left.typ.getOrElse(throw new IllegalStateException(s"Cannot compile call to $op with untyped argument $left"))
        param"left: ${typ.asScala}"
      }
      val rightParam = {
        val typ = right.typ.getOrElse(throw new IllegalStateException(s"Cannot compile call to $op with untyped argument $right"))
        param"right: ${typ.asScala}"
      }
      val funCode = q"($leftParam, $rightParam) => left ${op.tree} right"
      val resType = expression.typ.getOrElse(throw new IllegalStateException("cannot compile untyped base infix application"))

      val leftRes = transExpression(left)
      val rightRes = transExpression(right)
      val evalOut = Datalog.Var(gensym.fresh("eval"))
      for ((Seq(leftTerm), leftCons) <- leftRes;
           (Seq(rightTerm), rightCons) <- rightRes) yield {
        val evalConstraint = Datalog.Computed(evalOut,
          Datalog.Evaluation(Seq(leftTerm -> transType(left.typ.get), rightTerm -> transType(right.typ.get)),
            transType(resType), Scala(funCode)))
        (Seq(evalOut), leftCons ++ rightCons ++ Seq(evalConstraint))
      }

    case stmt =>
      throw new RuntimeException(s"Statement not supported $stmt")
  }

  private def flattenParam(name: String, typ: Type, genFresh: Boolean): Seq[Datalog.Param] = typ match {
    case TTuple(tys) => tys.zipWithIndex.flatMap { case (ty, ix) =>
      flattenParam(name + "_" + ix, ty, genFresh = true)
    }
    case _ =>
      val v = if (genFresh) gensym.fresh(name) else name
      Seq(Datalog.Param(v, transType(typ)))
  }

  private def flattenVars(x: Name, ty: Type): Seq[(Datalog.Var, Datalog.Type)] = ty match {
    /*case TTuple(ts) =>
      ts.zipWithIndex.map { case (ty, ix) => Datalog.Var(x.name + "$_" + ix) -> transType(ty) }
      tupleParams.get(x.name) match {
        case Some(vars) =>
          ts.zip(vars).map { case (ty, v) => Datalog.Var(v) -> transType(ty) }
        case None =>
          ts.zipWithIndex.map { case (ty, ix) => Datalog.Var(x.name + "$_" + ix) -> transType(ty) }
      }*/
    case ty =>
      Seq(Datalog.Var(x.raw) -> transType(ty))
  }

  def GP_URI: Datalog.TScala = Datalog.TScala(Scala(typeOf[truechange.URI]))

  private def transVis(vis: Option[Visibility]): Option[Datalog.Visibility] =
    vis.map { case Private => Datalog.Private }

  //@tailrec
  private def transType(typ: Type): Datalog.Type = typ match {
    case TAny => Datalog.TAny
    case TClass(ref) => GP_URI
    case TScala(ty) => Datalog.TScala(ty)
    case _ => throw new IllegalArgumentException(s"Cannot translate $typ to Datalog")
  }
}