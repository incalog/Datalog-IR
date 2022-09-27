package inca.frontend.objectoriented.lowering

import inca.backend.hints.{MagicSetHints, ObjectHints, OptimizationHints}
import inca.backend.ir.Datalog
import inca.frontend.objectoriented.core.{TNull, _}
import inca.frontend.objectoriented.lowering.GenerateDatalog._
import inca.runtime.aggregate.Aggregation
import inca.runtime.data.ObjectID
import inca.util.Scala.{symbolOf, typeOf}
import inca.util.{Gensym, Scala, TupleOps}

import scala.{+:, :+}
import scala.annotation.tailrec
import scala.collection.immutable.MultiDict
import scala.collection.mutable.ListBuffer
import scala.meta.Term
import scala.meta.quasiquotes._

object GenerateDatalog {
  private val sep: String = "$"
  private val internalPrefix: String = "_" + sep

  val castPatName: String       = internalPrefix + "cast"
  val equalsPatName: String     = internalPrefix + "equals"
  val instanceOfPatName: String = internalPrefix + "instanceOf"

  def dispatchPatName(methodNameWithSignature: String): String = s"${internalPrefix}dispatch_${methodNameWithSignature}"
  def constructorPatName(className: String): String = className
  def fieldPatName(className: String, fieldName: String): String = className + sep + sep + fieldName

  def transformModule(module: Module): Datalog.Module =
    new GenerateDatalog(module).transModule()

  def transformModules(modules: Seq[Module]): Seq[Datalog.Module] =
    modules.map(transformModule)
}


class GenerateDatalog(module: Module) {

  private val gensym: Gensym = new Gensym(Iterable.empty)

  private val generatedPatterns = ListBuffer[Datalog.Pattern]()

  val oOID: meta.Term = symbolOf(ObjectID)
  val tyOID: meta.Type = typeOf[ObjectID]

  def transModule(): Datalog.Module = {
    val Module(name, imports, classes) = module
    gensym.register(module.usedModuleNames.map(_.raw))
    gensym.register(module.usedClassNames.map(_.raw))

    generatedPatterns += transNull()
    generatedPatterns += transInstanceOf()
    generatedPatterns += transCast()
    generatedPatterns += transEquals()
    generatedPatterns ++= transDynamicDispatch(classes)
    generatedPatterns ++= classes.flatMap(transClass)

    Datalog.Module(
      name.raw,
      imports.map(_.name.raw),
      generatedPatterns.toList,
      Seq()
    )
  }

  /**
   * Guard that ensures that an object of the class with the id specified in the var `this` exists.
   * @param classDef the class to check for
   * @param neg      negate the guard, that means no object with the id exists
   * @return         Datalog.Call to check the existence
   */
  private def guard(classDef: ClassDef, neg: Boolean = false): Datalog.Call = {
    val thisVar = Datalog.Var("this")
    Datalog.Call(constructorPatName(classDef.name.raw), Seq(thisVar), neg = neg)
      .addHint(MagicSetHints.IgnoreCall)
      .addHint(MagicSetHints.FixedAdornment(Seq(false)))
  }

  private def transDynamicDispatch(classes: Seq[ClassDef]): Seq[Datalog.Pattern] = {
    /*
     * Collect all methods implemented by a class. This function traverses all parent classes and stores
     * a mapping func.name${hash} -> (classDef, methodDef) where classDef is the class itself or the parent class
     * where the method is last overwritten.
     */
    def collectMethods(classDef: ClassDef): Map[String, (ClassDef, MethodDef)] = {
      val methods = classDef.content.flatMap {
        case m :MethodDef if !m.annos.contains(MainAnnotation) => Seq(m.name + sep + m.paramSignature -> (classDef, m))
        case _ => None
      }.toMap

      val parentMethods = classDef.parentClassRefs.flatMap { ref =>
        val parentClassDef = ref.target.getOrElse(throw new RuntimeException(s"Unresolved class ${ref.name.raw}"))
        collectMethods(parentClassDef)
      }.toMap
      parentMethods ++ methods
    }

    val params = classes.flatMap(collectMethods).map {
      case (sig, (c, m)) =>
        sig ->
          (Datalog.Param("this", transType(c.typ))
            +: m.params.map(p => Datalog.Param(p.name.raw, transType(p.typ)))
            :+ Datalog.Param("out", transType(m.outType)))
    }.toMap

    val bodies = MultiDict.from(classes.flatMap { cls =>
      collectMethods(cls).map {
        case (sig, (c, m)) =>
          val methodParams = params(sig).map(p => Datalog.Var(p.name))
          val methodCall = Datalog.Call(c.name.raw + sep + m.name, methodParams)
          sig -> Datalog.Body(Seq(guard(cls), methodCall))
      }.toSeq
    })

    params.map { case (sig, params) =>
      Datalog.Pattern(None, dispatchPatName(sig), params, bodies.get(sig).toSeq)
    }.toSeq
  }

  private def transNull(): Datalog.Pattern = gensym.scoped {
    val outParam = Datalog.Param("this", transType(TNull))
    val thisVar = Datalog.Var("this")
    val constrScalaFun = Term.Function(Nil, q"""$oOID("Null")""")
    val tmpCons = Datalog.Computed(thisVar, Datalog.Evaluation(Seq(), transType(TNull), Scala(constrScalaFun)))

    Datalog.Pattern(None, "Null", Seq(outParam), Seq(
      Datalog.Body(Seq(tmpCons))
    ))
  }

  private def getObjectAttribute(obj: Datalog.Var, attribute: String, outVar: Datalog.Var, outType: Datalog.Type): Datalog.Computed = {
    val compAttr = Term.Name(attribute)
    val compArg = Term.Name("obj")
    val compParam = Term.Param(Nil, compArg, Some(GP_URI.asScala), None)
    Datalog.Computed(
      outVar, Datalog.Evaluation(Seq(obj -> GP_URI), outType, Scala(q"($compParam) => $compArg.$compAttr")
      )
    )
  }

  private def getObjectTyp(obj: Datalog.Var, outVar: Datalog.Var): Datalog.Computed = {
    getObjectAttribute(obj, "typ", outVar, Datalog.TScalaString)
  }

  /*private def getObjectId(obj: Datalog.Var, outVar: Datalog.Var): Datalog.Computed = {
    getObjectAttribute(obj, "allocId", outVar, Datalog.TScalaInt)
  }*/

  private def transEquals(): Datalog.Pattern = gensym.scoped {
    val params = Seq(
      Datalog.Param("obj1", GP_URI),
      Datalog.Param("obj2", GP_URI),
      Datalog.Param("out", Datalog.TScalaBoolean)
    )

    val compArg1 = Term.Name("obj1")
    val compParam1 = Term.Param(Nil, compArg1, Some(GP_URI.asScala), None)
    val compArg2 = Term.Name("obj2")
    val compParam2 = Term.Param(Nil, compArg2, Some(GP_URI.asScala), None)

    val body = Datalog.Body(Seq(
      Datalog.Computed(
        Datalog.Var("out"),
        Datalog.Evaluation(
          Seq(Datalog.Var("obj1") -> GP_URI, Datalog.Var("obj2") -> GP_URI),
          Datalog.TScalaBoolean,
          Scala(q"($compParam1, $compParam2) => $compArg1 == $compArg2")
        )
      )
    ))

    Datalog.Pattern(None, equalsPatName, params, Seq(body))
  }

  private def transInstanceOf(): Datalog.Pattern = gensym.scoped {
    val params = Seq(
      Datalog.Param("this", GP_URI),
      Datalog.Param("t", Datalog.TScalaString),
      Datalog.Param("out", Datalog.TScalaBoolean)
    )

    val tyVar = Datalog.Var("ty")
    val tyComp = getObjectTyp(Datalog.Var("this"), tyVar)

    val outVar = Datalog.Var("out")
    val tyParamVar = Datalog.Var("t")
    val outTrue = Datalog.Eq(outVar, Datalog.True)
    val outFalse = Datalog.Eq(outVar, Datalog.False)

    val isSubtype = Datalog.ExtensionalCall("subtype", Seq(tyVar, tyParamVar))
    // TODO: If negation of ExtensionalCall is implemented this can be changed to !isSubtype
    val notIsSubtype = Datalog.ExtensionalCall("not#subtype", Seq(tyVar, tyParamVar))

    val bodies = Seq(
      Datalog.Body(Seq(tyComp, isSubtype, outTrue)),
      Datalog.Body(Seq(tyComp, notIsSubtype, outFalse)),
    )
    Datalog.Pattern(None, instanceOfPatName, params, bodies)
  }

  private def transCast(): Datalog.Pattern = gensym.scoped {
    val params = Seq(
      Datalog.Param("this", GP_URI),
      Datalog.Param("t", Datalog.TScalaString)
    )

    val tyVar = Datalog.Var("ty")
    val tyComp = getObjectTyp(Datalog.Var("this"), tyVar)

    val tyParamVar = Datalog.Var("t")
    val isSubtype = Datalog.ExtensionalCall("subtype", Seq(tyVar, tyParamVar))

    val bodies = Seq(
      Datalog.Body(Seq(tyComp, isSubtype)),
    )
    Datalog.Pattern(None, castPatName, params, bodies)
      .addHint(OptimizationHints.NoInline, OptimizationHints.NoInlineInput)
  }

  private def transClass(classDef: ClassDef): Seq[Datalog.Pattern] = {
    classDef.content.map {
      case field: FieldDef => transField(classDef, field)
      case method: MethodDef => transMethod(classDef, method)
      case constructor: ConstructorDef => transConstructor(classDef, constructor)
    } :+ transDefaultConstructor(classDef)
  }

  private def transConstructor(classDef: ClassDef, constructorDef: ConstructorDef): Datalog.Pattern = {
    val qualifiedName = constructorPatName(classDef.name.raw) + sep + constructorDef.paramSignature

    val thisParam = Datalog.Param("this", transType(classDef.typ))
    val params = constructorDef.params.map(p => Datalog.Param(p.name.raw, transType(p.typ)))

    val bodyRes = transStatements(constructorDef.body)
    val bodies = for ((optReturn, cons) <- bodyRes) yield {
      if (optReturn.nonEmpty)
        throw new IllegalStateException(s"Constructor ${classDef.name} must not call return")
      Datalog.Body(
        Datalog.Call(constructorPatName(classDef.name.raw), Seq(Datalog.Var("this"))) +: cons
      )
    }

    Datalog.Pattern(None, qualifiedName, thisParam +: params, bodies)
  }

  private def transField(classDef: ClassDef, fieldDef: FieldDef): Datalog.Pattern = {
    val qualifiedName = fieldPatName(classDef.name.raw, fieldDef.name.raw)
    val params = Seq(
      Datalog.Param("this", transType(classDef.typ)),
      Datalog.Param(fieldDef.name.raw, transType(fieldDef.typ))
    )
    Datalog.Pattern(None, qualifiedName, params, Seq())
      .addHint(ObjectHints.Field)
  }

  private def transDefaultConstructor(classDef: ClassDef): Datalog.Pattern = gensym.scoped {
    val thisVar = Datalog.Var("this")
    val constrScalaFun = Term.Function(Nil, q"""$oOID(${classDef.name.raw})""")
    val tmpCons = Datalog.Computed(thisVar, Datalog.Evaluation(Seq(), transType(classDef.typ), Scala(constrScalaFun)))

    val thisParam = Datalog.Param("this", transType(classDef.typ))

    // TODO: Load initial values for all fields
    // set all the default values for each field
    /*val fieldSetter = classDef.fields.flatMap {
      case fieldDef if fieldDef.body.isDefined =>
        val fieldName = fieldPatName(classDef.name.raw, fieldDef.name.raw)

        for ((Seq(term), cons) <- transExpression(fieldDef.body.get)) yield {
          Seq(cons, Datalog.Call(fieldName, Seq(thisVar, term)).addHint(ObjectHints.FieldSet))
        }
      case _ => None
    }*/

    Datalog.Pattern(transVis(classDef.vis), constructorPatName(classDef.name.raw), Seq(thisParam), Seq(
      Datalog.Body(Seq(tmpCons))
    )).addHint(OptimizationHints.NoInline)
      .addHint(ObjectHints.Allocation)
  }

  private def transMethod(classDef: ClassDef, methodDef: MethodDef): Datalog.Pattern = gensym.scoped {
    gensym.register(methodDef.vars.keys.map(_.raw) + "this")

    val qualifiedName = classDef.name + sep + methodDef.name.raw

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

    if (methodDef.isMain)
      Datalog.Pattern(transVis(methodDef.vis), qualifiedName, argParams ++ returnParams,  bodies)
        .addHint(MagicSetHints.Main(argParams.map(_ => true) ++ returnParams.map(_ => false)))
        .addHint(ObjectHints.AllocationRoot)
        .addHint(ObjectHints.FieldRoot)
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

    case VarDeclareStmt(name, _, Some(expression), true) =>
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
    case FieldAssignStmt(recv, name, expression) =>
      // TODO: We might use the fieldDef target here instead to allow inheritance of attributes
      val classType = recv.typ.getOrElse(throw new IllegalArgumentException(s"Untyped expression $expression")) match {
        case t: TClass => t
        case _ => throw new IllegalArgumentException(s"Illegal field lookup on expression $expression")
      }
      val classDef = classType.ref.target.getOrElse(throw new IllegalArgumentException(s"Unresolved class $classType"))
      val qualifiedName = fieldPatName(classDef.name.raw, name.raw)

      val transRecv = for ((Seq(term), cons) <- transExpression(recv)) yield {
        for (tups <- transExpression(expression)) yield {
          val (argTerms, argCons) = tups
          (None, cons ++ argCons :+ Datalog.Call(qualifiedName, term +: argTerms).addHint(ObjectHints.FieldSet))
        }
      }
      transRecv.flatten

      /*for (tups <- TupleOps.cartesianProduct(exprRes)) yield {
        val (argTerms, argCons) = tups.unzip
        (None, argCons.flatten :+
          Datalog.Call(fieldPatName(classDef.name.raw, name.raw), argTerms.flatten)
        )
      }*/

    //case VarAssignStmt(targetName, expression) => ???
    case s =>
      throw new RuntimeException(s"Statement $s is not yet supported!")
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
        val fieldReadVar = Datalog.Var(gensym.fresh(targetName.raw))
        val fieldReadCall = Datalog.Call(fieldPatName(classDef.name.raw, targetName.raw), Seq(term, fieldReadVar))
          .addHint(MagicSetHints.FixedAdornment(Seq(true, true)))
          .addHint(ObjectHints.FieldGet)
        (Seq(fieldReadVar), cons :+ fieldReadCall)
      }

    case constrExpr@ConstructorExpr(classRef, args) =>
      // TODO: Figure out inheritance for constructors
      val constructedVar = Datalog.Var(gensym.fresh("new"))
      val argRes = args.map(e => transExpression(e))
      val constructorDef = constrExpr.target.getOrElse(throw new IllegalArgumentException(s"Unresolved constructor $constrExpr"))

      val constrName =
        if (constructorDef.params.nonEmpty)
          constructorPatName(classRef.name.raw) + sep + constructorDef.paramSignature
        else
          constructorPatName(classRef.name.raw)

      // create single call constraint when no arguments are passed
      if (argRes.isEmpty)
        return Seq((Seq(constructedVar), Seq(Datalog.Call(constrName, Seq(constructedVar)))))

      for (tups <- TupleOps.cartesianProduct(argRes)) yield {
        val (argTerms, argCons) = tups.unzip
        (Seq(constructedVar), argCons.flatten ++ Seq(Datalog.Call(constrName, constructedVar +: argTerms.flatten)))
      }

    case methodCallExp@MethodCallExpr(recv, fun, args) =>
      val outVar = Datalog.Var(gensym.fresh("methodCall"))
      val argRes = args.map(e => transExpression(e))

      val methodDef = methodCallExp.target.getOrElse(throw new IllegalArgumentException(s"Unresolved method $methodCallExp"))
      val qualifiedName = dispatchPatName(methodDef.name + sep + methodDef.paramSignature)

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
        val castCall = Datalog.Call(castPatName, Seq(eTerm, Datalog.StringConstant(toTyp.toString)))
        (Seq(eTerm), eCons :+ castCall)
      }

    case InstanceOfExpr(recv, ofTyp) =>
      for ((Seq(eTerm), eCons) <- transExpression(recv)) yield {
        val outVar = Datalog.Var(gensym.fresh("isInstance"))
        val instanceOfCall = Datalog.Call(instanceOfPatName, Seq(eTerm, Datalog.StringConstant(ofTyp.toString), outVar))
        (Seq(outVar), eCons :+ instanceOfCall)
      }

    case EqualsExpr(obj1, obj2) =>
      val transExps = Seq(transExpression(obj1), transExpression(obj2))
      for (tups <- TupleOps.cartesianProduct(transExps)) yield {
        val (eTerms, eCons) = tups.unzip
        val outVar = Datalog.Var(gensym.fresh("isEqual"))
        val equalsCall = Datalog.Call(equalsPatName, eTerms.flatten :+ outVar)
        (Seq(outVar), eCons.flatten ++ Seq(equalsCall))
      }

    case NullExpr() =>
      val nullVar = Datalog.Var(gensym.fresh("null"))
      val nullConstrCall = Datalog.Call("Null", Seq(nullVar))
      Seq((Seq(nullVar), Seq(nullConstrCall)))

    /*
    case SuperExpr(args) => ???
    case TupleExpr(exps) => ???*/
    case BaseLitExpr(code) =>
      import scala.meta._
      val evalOut = Datalog.Var(gensym.fresh("lit"))
      val resType = expression.typ.getOrElse(throw new IllegalStateException("Cannot compile untyped Eval"))
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
      val resType = exp.typ.getOrElse(throw new IllegalStateException("Cannot compile untyped base infix application"))

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
      val resType = expression.typ.getOrElse(throw new IllegalStateException("Cannot compile untyped Eval"))

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
      val resType = expression.typ.getOrElse(throw new IllegalStateException("Cannot compile untyped Eval"))

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
      val resType = expression.typ.getOrElse(throw new IllegalStateException("Cannot compile untyped base infix application"))

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

  def GP_URI: Datalog.TScala = Datalog.TScala(Scala(typeOf[ObjectID]))

  private def transVis(vis: Option[Visibility]): Option[Datalog.Visibility] =
    vis.map { case Private => Datalog.Private }

  //@tailrec
  private def transType(typ: Type): Datalog.Type = typ match {
    case TAny => Datalog.TAny
    case TNull => GP_URI
    case TClass(_) => GP_URI
    case TScala(ty) => Datalog.TScala(ty)
    case _ => throw new IllegalArgumentException(s"Cannot translate $typ to Datalog")
  }
}