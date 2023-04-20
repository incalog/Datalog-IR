package inca.frontend.objectoriented.lowering

import inca.backend.hints.{MagicSetHints, ObjectHints, OptimizationHints}
import inca.backend.ir.Datalog
import inca.backend.ir.Datalog.Computed
import inca.compiler.SourceObject
import inca.frontend.objectoriented.core._
import inca.frontend.objectoriented.lowering.GenerateDatalog._
import inca.util.TupleOps
import inca.runtime.data.ObjectID
import inca.util.Scala.{symbolOf, typeOf}
import inca.util.{Gensym, Scala}

import scala.collection.immutable.MultiDict
import scala.collection.mutable.ListBuffer
import scala.meta.{Stat, Term, Type => MetaType}
import scala.meta.quasiquotes._

object GenerateDatalog {
  private val sep: String = "$"
  private val internalPrefix: String = "_" + sep

  val castPatName: String = internalPrefix + "cast"
  val instanceOfPatName: String = internalPrefix + "instanceOf"

  def dispatchPatName(methodNameWithSignature: String): String = s"${internalPrefix}dispatch_${methodNameWithSignature}"
  def aggregatePatName(className: String, methodName: String): String = s"${internalPrefix}aggregate_$className${sep}$methodName"
  def coalescedPatName(className: String): String = s"${internalPrefix}coalesced_$className"
  def coalescedPatName(): String = s"${internalPrefix}coalesced"
  def uncoalescedPatName(className: String): String = s"${internalPrefix}uncoalesced_$className"
  def uncoalescedPatName(): String = s"${internalPrefix}uncoalesced"
  def constructorPatName(className: String): String = className + sep
  def constructorSuperPatName(className: String): String = s"${internalPrefix}super_$className"
  def fieldPatName(className: String, fieldName: String): String = s"$className$sep$sep$fieldName"
  def methodPatName(className: String, methodName: String): String = s"$className$sep$methodName"
  def monoMapPatName(mapName: String): String = mapName + "$1"

  def transformModule(typedModule: Module, coreModule: Module): Datalog.Module =
    new GenerateDatalog(typedModule, coreModule).transModule()

  def transformModules(modules: Seq[(Module, Module)]): Seq[Datalog.Module] =
    modules.map { case (typed, core) => transformModule(typed, core) }
}


class GenerateDatalog(typedModule: Module, coreModule: Module) {

  private val gensym: Gensym = new Gensym(Iterable.empty)
  private val genScala: GenerateScala = new GenerateScala

  private val generatedPatterns = ListBuffer[Datalog.Pattern]()
  private val generatedScala = ListBuffer[Scala[Stat]]()

  val oOID: meta.Term = symbolOf(ObjectID)
  val tyOID: meta.Type = typeOf[ObjectID]

  def GP_URI: Datalog.TScala = Datalog.TScala(Scala(tyOID))

  private def createObject(className: String, typ: Type): (Datalog.Var, Computed) = {
    val objVar = Datalog.Var(gensym.fresh("obj"))
    val constrScalaFun = Term.Function(Nil, q"$oOID($className)")
    val constrComp = Datalog.Computed(objVar, Datalog.Evaluation(
      Seq(), transType(typ), Scala(constrScalaFun))
    )
    (objVar, constrComp.addHint(ObjectHints.AllocationInit))
  }

  private def createNullObject(): (Datalog.Var, Computed) = {
    val objVar = Datalog.Var(gensym.fresh("null"))
    val constrScalaFun = Term.Function(Nil, q"""$oOID("Null")""")
    val constrComp = Datalog.Computed(objVar, Datalog.Evaluation(
      Seq(), transDataType(TNull), Scala(constrScalaFun))
    )
    (objVar, constrComp)
  }

  def transModule(): Datalog.Module = {
    val Module(name, imports, classes) = coreModule
    val concreteClasses = classes.filter(c => !c.isAbstract && !c.isMonotoneMapClass)

    gensym.register(coreModule.usedModuleNames.map(_.raw))
    gensym.register(concreteClasses.map(_.name.raw))

    generatedPatterns += transNull()
    generatedPatterns += transInstanceOf()
    generatedPatterns += transCast()
    generatedPatterns ++= transDynamicDispatch(concreteClasses)
    generatedPatterns ++= concreteClasses.flatMap(transClass)

    val scalaModule = genScala.genModule(typedModule)
    // TODO: Optimize: We only need to generate this if we use an aggregation. We keep it in for now, to spot errors
    //  in the scala code generation.
    generatedScala ++= scalaModule.classes.map(c => Scala(c))
    generatedScala ++= scalaModule.objects.map(c => Scala(c))

    Datalog.Module(
      name.raw,
      imports.map(_.name.raw),
      generatedPatterns.toList,
      generatedScala.toList
    )
  }

  private def transDynamicDispatch(classes: Seq[ClassDef]): Seq[Datalog.Pattern] = {
    /*
     * Collect all methods transitively implemented by a class. This function traverses all parent classes and stores
     * a mapping func.name${hash} -> (classDef, methodDef) where classDef is the class itself or the parent class
     * where the method is last overwritten.
     */
    def collectMethods(classDef: ClassDef): Map[String, (ClassDef, MethodDef)] = {
      val methods = classDef.content.flatMap {
        case m :MethodDef if !m.annos.contains(MainAnnotation) => Seq(m.name + sep + m.signature -> (classDef, m))
        case _ => None
      }.toMap

      val parentMethods = classDef.parentClassRefs.flatMap { ref =>
        val parentClassDef = ref.target.getOrElse(throw new IllegalArgumentException(s"Unresolved class ${ref.name.raw}"))
        collectMethods(parentClassDef)
      }.toMap
      parentMethods ++ methods
    }

    val params = classes.flatMap(collectMethods).map {
      case (sig, (c, m)) =>
        val isMonotoneAddMethod = c.isMonotoneClass && m.name.raw == AssignmentOp.AGG_ELEMENT.name.raw
        val outParms = if (isMonotoneAddMethod) {
          // TODO: support this for arbitrarily nested tuples
          val resType = m.outType match {
            case ty : TClass => transDataType(ty)
            case ty =>  transType(ty)
          }
          Seq(Datalog.Param(gensym.fresh("out"), resType))
        } else {
          flattenParam("out", m.outType, genFresh = false)
        }
        sig ->
          (Datalog.Param("this", transType(c.typ))
            +: (m.params.flatMap(p => flattenParam(p.name.raw, p.typ, genFresh = false)) ++ outParms))
    }.toMap

    val bodies = MultiDict.from(classes.flatMap { cls =>
      collectMethods(cls).map {
        case (sig, (c, m)) =>
          val methodParams = params(sig).map(p => Datalog.Var(p.name))
          val methodCall = Datalog.Call(methodPatName(c.name.raw, m.name.raw), methodParams)
          val guard = Datalog.Computed(
            Datalog.StringConstant(cls.name.raw),
            Datalog.Evaluation(
              Seq(Datalog.Var("this") -> transType(c.typ)),
              Datalog.TScalaString,
              Scala(q"(obj: $tyOID) => obj.typ")
            )
          )
          sig -> Datalog.Body(Seq(guard, methodCall))
      }.toSeq
    })

    val methodDispatchs = params.map { case (sig, params) =>
      Datalog.Pattern(None, dispatchPatName(sig), params, bodies.get(sig).toSeq)
    }.toSeq

    val normalClasses = classes.filter(!_.isDefunAuxiliary)
    val coalescedBodies = normalClasses.map { cls =>
      val guard = Datalog.Computed(
        Datalog.StringConstant(cls.name.raw),
        Datalog.Evaluation(
          Seq(Datalog.Var("this") -> transType(cls.typ)),
          Datalog.TScalaString,
          Scala(q"(obj: $tyOID) => obj.typ")
        )
      )
      val coalescedCall = Datalog.Call(coalescedPatName(cls.name.raw), Seq(Datalog.Var("this"), Datalog.Var("obj")))
      Datalog.Body(Seq(guard, coalescedCall))
    }

    val uncoalescedBodies = normalClasses.map { cls =>
      val guard = Datalog.Computed(
        Datalog.StringConstant(cls.name.raw),
        Datalog.Evaluation(
          Seq(Datalog.Var("obj") -> Datalog.TScala(Scala(t"AnyRef"))),
          Datalog.TScalaString,
          Scala(q"(obj: AnyRef) => obj.getClass.getSimpleName")
        )
      )
      val uncoalescedCall = Datalog.Call(uncoalescedPatName(cls.name.raw), Seq(Datalog.Var("obj"), Datalog.Var("this")))
      Datalog.Body(Seq(guard, uncoalescedCall))
    }


    val thisParam = Datalog.Param("this", GP_URI)
    val objParam = Datalog.Param("obj", Datalog.TScala(Scala(t"AnyRef")))
    val coalescedDispatch = Datalog.Pattern(None, coalescedPatName(), Seq(thisParam, objParam), coalescedBodies)
    val uncoalescedDispatch = Datalog.Pattern(None, uncoalescedPatName(), Seq(objParam, thisParam), uncoalescedBodies)

    methodDispatchs :+ coalescedDispatch :+ uncoalescedDispatch
  }

  private def transNull(): Datalog.Pattern = gensym.scoped {
    Datalog.Pattern(None, constructorPatName("Null"), Seq(Datalog.Param("this", transType(TNull))), Seq(
      Datalog.Body(Seq())
    ))
  }

  private def getURIAttribute(uri: Datalog.Var, attribute: String, out: Datalog.Term, outType: Datalog.Type): Datalog.Computed = {
    val compAttr = Term.Name(attribute)
    val compArg = Term.Name("uri")
    val compParam = Term.Param(Nil, compArg, Some(GP_URI.asScala), None)
    Datalog.Computed(
      out, Datalog.Evaluation(Seq(uri -> GP_URI), outType, Scala(q"($compParam) => $compArg.$compAttr")
      )
    )
  }

  private def getURIIsNull(uri: Datalog.Var, out: Datalog.Term): Datalog.Computed = {
    getURIAttribute(uri, "isNull", out, Datalog.TScalaBoolean)
  }

  private def getURIAllocId(uri: Datalog.Var, out: Datalog.Term): Datalog.Computed = {
    getURIAttribute(uri, "allocId", out, Datalog.TScalaInt)
  }

  private def getURITyp(uri: Datalog.Var, out: Datalog.Term): Datalog.Computed = {
    getURIAttribute(uri, "typ", out, Datalog.TScalaString)
  }

  private def transInstanceOf(): Datalog.Pattern = gensym.scoped {
    val params = Seq(
      Datalog.Param("this", GP_URI),
      Datalog.Param("t", Datalog.TScalaString),
      Datalog.Param("out", Datalog.TScalaBoolean)
    )

    val tyVar = Datalog.Var("ty")
    val tyComp = getURITyp(Datalog.Var("this"), tyVar)

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
    val tyComp = getURITyp(Datalog.Var("this"), tyVar)

    val tyParamVar = Datalog.Var("t")
    val isSubtype = Datalog.ExtensionalCall("subtype", Seq(tyVar, tyParamVar))

    val bodies = Seq(
      Datalog.Body(Seq(tyComp, isSubtype)),
    )
    Datalog.Pattern(None, castPatName, params, bodies)
      .addHint(OptimizationHints.NoInline, OptimizationHints.NoInlineInput)
  }

  private def transClass(classDef: ClassDef): Seq[Datalog.Pattern] = {
    val unCoalescingPattern =
      if (!classDef.isDefunAuxiliary)
        Seq(generateConstructorCoalesced(classDef), generateConstructorUncoalesced(classDef))
      else
        Seq()

    val clsPattern = classDef.content.flatMap {
      case field: FieldDef => Seq(transField(classDef, field))
      case method: MethodDef => Seq(transMethod(classDef, method))
      // TODO: Prevent recreating the default constructor
      case constructor: ConstructorDef =>
        Seq(
          transConstructor(classDef, constructor),
          transSuper(classDef, constructor)
        )
    }
    (clsPattern ++ unCoalescingPattern)
  }

  private def generateConstructorCoalesced(classDef: ClassDef): Datalog.Pattern = gensym.scoped {
    import scala.meta._

    val fields = classDef.fields.flatMap {
      case FieldDef(_, _, Name(name), ty, _, _) if ty.asSet.isDefined =>
        println(s"Can not coalesced field ${classDef.name.raw}.$name with set type!")
        None
      case f => Some(f)
    }

    val className = classDef.name.raw
    val uriParam = Datalog.Param("uri", transType(classDef.typ))
    val uriVar = Datalog.Var(uriParam.name)
    val objType = transDataType(classDef.typ)
    val objParam = Datalog.Param("obj", objType)
    val objVar = Datalog.Var(objParam.name)

    val (readFields, fieldVars) = fields.filter { f =>
      // TODO: Hack MonoMap
      f.typ match {
        case TClass(ref) if ref.target.isDefined && ref.target.get.isMonotoneMapClass => false
        case _ => true
      }
    }.map { f =>
      val varName = gensym.fresh(f.name.raw)
      val fieldReadVars = flattenVars(varName, f.typ).map(_._1)
      val fieldPat = fieldPatName(className, f.name.raw)
      val fieldReadCall = Datalog.Call(fieldPat, uriVar +: fieldReadVars)
        .addHint(MagicSetHints.FixedAdornment(true +: fieldReadVars.map(_ => true)))
        .addHint(ObjectHints.FieldGet)

      // coalesced fields
      val (coalescedChildCalls, vars) = fieldReadVars.zip(f.typ.flatten).map { case (v, t) =>
        t match {
          case TClass(ClassRef(name)) =>
            val coalescedChildVar = Datalog.Var(gensym.fresh(v.name))
            val coalescedChildCall = Seq(Datalog.Call(coalescedPatName(), Seq(v, coalescedChildVar)))
            (coalescedChildCall, coalescedChildVar)
          case _ =>
            (Seq(), v)
        }
      }.unzip
      (fieldReadCall +: coalescedChildCalls.flatten, vars)
    }.unzip

    // read and pass the allocation id and all fields to the scala function
    val allocId = Datalog.Var(gensym.fresh("allocId"))
    val readFieldsFlat = getURIAllocId(uriVar, allocId) +: readFields.flatten
    val fieldVarsFlat = allocId +: fieldVars.flatten
    val fieldTypes = TScalaInt +: fields.flatMap(_.typ.flatten)

    val constrArgs = fieldVarsFlat.map { v => Term.Name(v.name) }.toList
    val constrParams = constrArgs.zip(fieldTypes).map {
      case (vt, t) => Term.Param(Nil, vt, Some(genScala.transType(t)), None)
    }

    val constrScalaFun = Term.Function(
      constrParams,
      Term.Apply(Term.Select(Term.Name(className), Term.Name("apply")), constrArgs)
    )

    val evalParams = fieldVarsFlat.zip(fieldTypes).map { case (v, t) => v -> transDataType(t) }
    val genOutObj = Datalog.Computed(objVar, Datalog.Evaluation(evalParams, objType, Scala(constrScalaFun)))
    val bodyWithObject = Datalog.Body(
      getURIIsNull(uriVar, Datalog.False) +: readFieldsFlat :+ genOutObj
    )

    val genNullObj = Datalog.Computed(objVar, Datalog.Evaluation(Seq(), objType, Scala(q"() => null")))
    val bodyWithNull = Datalog.Body(
      Seq(getURIIsNull(uriVar, Datalog.True), genNullObj)
    )
    val bodies = Seq(bodyWithObject, bodyWithNull)
    val params = Seq(uriParam, objParam)
    val constrCoalescedPat = Datalog.Pattern(None, coalescedPatName(classDef.name.raw), params, bodies)
    constrCoalescedPat
      //.addHint(MagicSetHints.NoInputRelation)
  }

  private def generateConstructorUncoalesced(classDef: ClassDef): Datalog.Pattern = gensym.scoped {
    import scala.meta.Term

    val fields = classDef.fields.flatMap {
      case FieldDef(_, _, Name(name), ty, _, _) if ty.asSet.isDefined =>
        println(s"Can not uncoalesced field ${classDef.name.raw}.$name with set type!")
        None
      case f => Some(f)
    }

    val className = classDef.name.raw
    val uriParam = Datalog.Param("uri", transType(classDef.typ))
    val uriVar = Datalog.Var(uriParam.name)
    val objType = transDataType(classDef.typ)
    val objParam = Datalog.Param("obj", objType)
    val objVar = Datalog.Var(objParam.name)

    def readFieldComp(fieldName: String, ty: Type, tupleIndex: Option[Int]): (Datalog.Var, Datalog.Computed) = {
      val fieldReadVar = Datalog.Var(gensym.fresh(fieldName))
      val comp = Datalog.Computed(
        fieldReadVar,
        Datalog.Evaluation(
          Seq(objVar -> objType),
          transDataType(ty),
          Scala(Term.Function(
            List(Term.Param(Nil, Term.Name("obj"), Some(MetaType.Name(className)), None)),
            if (tupleIndex.isDefined)
              q"""shapeless(${Term.Name("obj")}.${Term.Name(fieldName)})(${tupleIndex.get})"""
            else
              q"""${Term.Name("obj")}.${Term.Name(fieldName)}"""
          ))
        )
      )
      (fieldReadVar, comp)
    }

    val fieldVarsAndComps = fields.filter { f =>
      // TODO: Hack MonoMap
      f.typ match {
        case TClass(ref) if ref.target.isDefined && ref.target.get.isMonotoneMapClass => false
        case _ => true
      }
    }.map { f =>
      // FIXME: This tuple check will fail if we allow coalescing sets, since tuples can then be contained inside a
      //  set. For now a Tuple can only be the outermost type at this source position.
      val isTuple = f.typ.isInstanceOf[TTuple]
      val (vars, comps) = f.typ.flatten.zipWithIndex.map {
        case (ty@TClass(ClassRef(name)), i) =>
          val (fieldReadVar, fieldReadComp) = readFieldComp(f.name.raw, ty, if (isTuple) Some(i) else None)
          val childUri = Datalog.Var(gensym.fresh(f.name.raw))
          val call = Datalog.Call(uncoalescedPatName(), Seq(fieldReadVar, childUri))
          (childUri, Seq(fieldReadComp, call))
        case (ty, i) =>
          val (fieldReadVar, fieldReadComp) = readFieldComp(f.name.raw, ty, if (isTuple) Some(i) else None)
          (fieldReadVar, Seq(fieldReadComp))
      }.unzip
      (f, vars, comps)
    }

    def setterCall(fieldDef: FieldDef, args: Seq[Datalog.Var]) = {
      val patName = fieldPatName(className, fieldDef.name.raw)
      Datalog.Call(patName, uriVar +: args)
    }

    val intOptionType = TScala(Scala(t"Option[Int]"))
    val (allocVar, allocComp) = readFieldComp("allocId", intOptionType, None)

    def objIsNull(isNull: Boolean) = Datalog.Computed(
      if (isNull) Datalog.True else Datalog.False,
      Datalog.Evaluation(
        Seq(objVar -> objType),
        Datalog.TScalaBoolean,
        Scala(q"""(objOrNull: ${genScala.transType(classDef.typ)}) => objOrNull == null""")
      )
    )

    def allocIdIsDefinedComp(isDefined: Boolean) = Datalog.Computed(
      if (isDefined) Datalog.True else Datalog.False,
      Datalog.Evaluation(
        Seq(allocVar -> transType(intOptionType)),
        Datalog.TScalaBoolean,
        Scala(q"""(allocIdOption: Option[Int]) => allocIdOption.isDefined""")
      )
    )

    // existing object was returned
    val genURIWithId = Datalog.Computed(
      uriVar,
      Datalog.Evaluation(
        Seq(allocVar -> transType(intOptionType)),
        transType(classDef.typ),
        Scala(q"""(allocId: Option[Int]) => $oOID(${className}, allocId.get)""")
      )
    )
    val allSetterCalls = fieldVarsAndComps.flatMap { case (f, vars, comps) =>
      comps.flatten :+ setterCall(f, vars).addHint(ObjectHints.FieldSet())
    }
    val bodyWithExistingObject = Datalog.Body(
      objIsNull(false) +: allocComp +: allocIdIsDefinedComp(true) +: genURIWithId +: allSetterCalls
    )

    // new object was created in scala
    val allSetterCallsWithFixedTimestamp = fieldVarsAndComps.flatMap { case (f, vars, comps) =>
      comps.flatten :+ setterCall(f, vars).addHint(ObjectHints.FieldSet(fixedTimestamp = Some(0)))
    }
    val bodyWithNewObject = if (classDef.isCaseClass) {
      val primaryConstr = classDef.constructors.find(_.isPrimary).getOrElse(
        throw new RuntimeException(s"Case class ${classDef.name} does not contain a primary constructor")
      )
      val primaryParamNames = primaryConstr.params.map(_.name.raw)
      val (primaryFields, _) = fieldVarsAndComps.partition { case (f, _, _) =>
        primaryParamNames.contains(f.name.raw)
      }
      val qualifiedName = constructorPatName(classDef.name.raw) + primaryConstr.signature
      val genURIWithoutId = Datalog.Call(qualifiedName, uriVar +: primaryFields.flatMap(_._2))
      val readFieldCalls = primaryFields.flatMap(_._3.flatten)

      Datalog.Body(
        objIsNull(false) +: allocComp +: allocIdIsDefinedComp(false) +: readFieldCalls :+ genURIWithoutId
      )
    } else {
      val (objVar, objComp) = createObject(className, classDef.typ)
      val constrCall = Datalog.Call(constructorPatName(className), Seq(objVar))
      val createObjectAtoms = Seq(objComp, constrCall, Datalog.Eq(uriVar, objVar))
      Datalog.Body(
        objIsNull(false) +: allocComp +: allocIdIsDefinedComp(false) +: (createObjectAtoms ++ allSetterCallsWithFixedTimestamp)
      )
    }

    // object is null
    val (nullVar, nullComp) = createNullObject()
    val nullConstrCall = Datalog.Call(constructorPatName("Null"), Seq(nullVar))
    val bodyWithNull = Datalog.Body(
      Seq(objIsNull(true), nullComp, nullConstrCall, Datalog.Eq(uriVar, nullVar))
    )

    val params = Seq(objParam, uriParam)
    val bodies = Seq(bodyWithNull, bodyWithExistingObject, bodyWithNewObject)
    Datalog.Pattern(None, uncoalescedPatName(classDef.name.raw), params, bodies)
  }

  private def transFieldInitBody(classDef: ClassDef): Seq[(Seq[Datalog.Term], Datalog.Body)] = {
    /**
     * Collect all fields from classDef and all inherited fields from all parents
     */
    def collectFields(classDef: ClassDef): Seq[(ClassDef, FieldDef)] = {
      val fields = classDef.fields.filter { f =>
        // TODO: Hack MonoMap
        f.typ match {
          case TClass(ref) if ref.target.isDefined && ref.target.get.isMonotoneMapClass => false
          case _ => true
        }
      }.map((classDef, _))
      val parentFields = classDef.parentClassRefs.flatMap(ref =>
        collectFields(ref.target.getOrElse(throw new IllegalArgumentException(s"Unresolved class $ref")))
      )
      parentFields ++ fields
    }

    // set the default value for each field
    val thisVar = Datalog.Var("this")
    val fieldTermsAndCons = collectFields(classDef).filter(_._2.body.isDefined).map { case (fieldClassDef, fieldDef) =>
      val fieldName = fieldPatName(fieldClassDef.name.raw, fieldDef.name.raw)
      // alternative bodies for this field
      for ((terms, cons) <- transExpression(fieldDef.body.get)) yield {
        val fieldInit = Datalog.Call(fieldName, thisVar +: terms).addHint(ObjectHints.FieldSet(fixedTimestamp = Some(0)))
        (terms, cons :+ fieldInit)
      }
    }

    if (fieldTermsAndCons.isEmpty)
      Seq((Seq(), Datalog.Body(Seq())))
    else {
      val alternatives = TupleOps.cartesianProduct(fieldTermsAndCons)
      alternatives.map(atomsAndTerms =>
        (atomsAndTerms.flatMap(_._1), Datalog.Body(atomsAndTerms.flatMap(_._2)))
      )
    }
  }

  private def transConstructor(classDef: ClassDef, constructorDef: ConstructorDef): Datalog.Pattern = {
    val qualifiedName = constructorPatName(classDef.name.raw) + constructorDef.signature
    val thisParam = Datalog.Param("this", transType(classDef.typ))
    val params = constructorDef.params.flatMap(p => flattenParam(p.name.raw, p.typ, genFresh = false))
    val constrBodies = transConstructorBody(classDef, constructorDef)
    val (fieldInitTerms, fieldInitBodies) = transFieldInitBody(classDef).unzip

    // we ignore the constructor body for a primary constructor
    if (classDef.isCaseClass && constructorDef.isPrimary) {
      import scala.meta._

      // all fields that are defined, but not used in the primary constructor
      // Note: this works, since the primary constructor always uses the field name as parameter name
      val primaryParamNames = constructorDef.params.map(_.name.raw)
      val fieldParams = classDef.fields
        .filter(f => !primaryParamNames.contains(f.name.raw))
        .zip(fieldInitTerms).flatMap { case (f, terms) =>
          terms.map {
            // we erase the type here, since we don't know it... this is not nice, but it works
            case Datalog.Var(name) => Datalog.Param(name, Datalog.TAny)
            case t => throw new RuntimeException(s"Unexpected term for field initialization $t")
          }
      }
      // use all fields to calculate an allocId from them
      val constructorParams = params ++ fieldParams
      val hashVar = Datalog.Var(gensym.fresh("allocIdHash"))
      val hashParams = constructorParams.map { p =>
        Term.Param(Nil, Term.Name(p.name), Some(p.typ.asScala), None)
      }.toList
      val hashScalaFun = Term.Function(
        hashParams,
        q"java.util.Objects.hash(..${constructorParams.map(p => Term.Name(p.name)).toList})"
        /*constructorParams
          .map { p => q"""${Term.Name(p.name)}.##""" }
          .reduce[Term] { case (c1, c2) => q"31 * ($c1) + $c2" }*/
      )
      val hashComp = Datalog.Computed(hashVar, Datalog.Evaluation(
        constructorParams.map(p => Datalog.Var(p.name) -> p.typ),
        Datalog.TScalaInt,
        Scala(hashScalaFun)
      ))

      // 1. define all literal for field declarations
      // 2. calculate the allocId based on the literals and params
      // 3. create the object
      // 4. assign the value to all fields
      val bodies = constrBodies.flatMap { cB =>
        fieldInitBodies.map { fB =>
          val (varSets, varDecls) = fB.atoms.partition(a => a.hasHint(ObjectHints.FieldSetKey))
          Datalog.Body(
            (varDecls :+ hashComp) ++
              (Datalog.Call(constructorPatName(classDef.name.raw), Seq(Datalog.Var("this"), hashVar)) +: (cB.atoms ++ varSets))
          )
        }
      }
      Datalog.Pattern(None, qualifiedName, thisParam +: params, bodies)
    } else {
      val bodies = constrBodies.flatMap { cB =>
        fieldInitBodies.map { fB =>
          Datalog.Body(
            (fB.atoms ++ cB.atoms)
          )
        }
      }
      Datalog.Pattern(None, qualifiedName, thisParam +: params, bodies)
    }
  }

  private def transSuper(classDef: ClassDef, constructorDef: ConstructorDef): Datalog.Pattern = {
    val qualifiedName = constructorSuperPatName(classDef.name.raw) + sep + constructorDef.signature
    val thisParam = Datalog.Param("this", transType(classDef.typ))
    val params = constructorDef.params.map(p => Datalog.Param(p.name.raw, transType(p.typ)))
    Datalog.Pattern(None, qualifiedName, thisParam +: params, transConstructorBody(classDef, constructorDef))
  }

  private def transConstructorBody(classDef: ClassDef, constructorDef: ConstructorDef): Seq[Datalog.Body] = {
    for ((optReturn, cons, _) <- transStatements(constructorDef.body, None, constructorDef)) yield {
      if (optReturn.nonEmpty)
        throw new IllegalStateException(s"Constructor ${classDef.name} must not call return")
      Datalog.Body(cons)
    }
  }

  private def transField(classDef: ClassDef, fieldDef: FieldDef): Datalog.Pattern = {
    // TODO: Hack MonoMap
    //   we just use the class name as relation name for now => Name collision
    //   we do not support lifting for now
    //   we should not include coalesced here
    fieldDef.typ match {
      case TClass(ref) if ref.target.isDefined && ref.target.get.isMonotoneMapClass =>
        // Create a monoMap field relation
        val monoCls = ref.target.get
        val monoTypes = monoCls.montoneTypes.get
        return Datalog.Pattern(None, monoMapPatName(monoCls.name.raw), Seq(
          Datalog.Param("key", transType(monoTypes._1)), Datalog.Param("value", transType(monoTypes._2)), Datalog.Param("coalesced", transDataType(monoTypes._2))
        ), Seq())
      case _ => // nothing
    }

    val qualifiedName = fieldPatName(classDef.name.raw, fieldDef.name.raw)
    val params = Datalog.Param("this", transType(classDef.typ)) +:
      flattenParam(fieldDef.name.raw, fieldDef.typ, genFresh = false)
    val pat = Datalog.Pattern(None, qualifiedName, params, Seq())
    pat.addHint(ObjectHints.Field(fieldDef.immutable))
  }

  private def transMethod(classDef: ClassDef, methodDef: MethodDef): Datalog.Pattern = gensym.scoped {
    gensym.register(methodDef.vars.keys.map(_.raw) + "this")

    val qualifiedName = methodPatName(classDef.name.raw, methodDef.name.raw)

    val thisVar = Datalog.Var("this")
    val thisParam = Datalog.Param("this", transType(classDef.typ))
    //TODO: we might not want to flat input tuple argument to += for Monotones
    val argParams = methodDef.params.flatMap { case Param(name, typ) =>
      flattenParam(name.raw, typ, genFresh = false)
    }

    // Special aggregate functions in a monotone class are only translated to scala
    val isMonotoneAddMethod = classDef.isMonotoneClass && methodDef.name.raw == AssignmentOp.AGG_ELEMENT.name.raw
    val isMonotoneAggregateMethod = classDef.isMonotoneClass && (methodDef.name match {
      case Name("init") | Name("join") => true
      case _ => false
    })
    // Do not flatten return tuples for monotone types
    val returnParams =
      if (isMonotoneAddMethod) {
        // TODO: Support nested tuples
        val resType = methodDef.outType match {
          case ty: TClass => transDataType(ty)
          case ty => transType(ty)
        }
        Seq(Datalog.Param(gensym.fresh("return"), resType))
      } else if (methodDef.returnsUnit) {
        Seq()
      } else {
        flattenParam("return", methodDef.outType, genFresh = true)
      }

    val bodyRes = if (isMonotoneAggregateMethod)
      Seq((None, Seq(), None))
    else
      transStatements(methodDef.body, None, methodDef)

    val bodies = {
      for ((optReturn, cons, _) <- bodyRes) yield {
      val returnTerms = optReturn.getOrElse(Seq())
      if (isMonotoneAddMethod && methodDef.outType.isInstanceOf[TTuple]) {
        // return a real scala tuple, not a flattened one
        // TODO: coealesced inside tuples
        import scala.meta._

        val orgReturnParams = flattenVars("return", methodDef.outType, genFresh = true).toList
        val metaTerms = orgReturnParams.map(p => Term.Name(p._1.name))
        val metaParams = orgReturnParams.map(p => Term.Param(Nil, Term.Name(p._1.name), Some(p._2.asScala), None))
        val transformReturnCond = Datalog.Computed(
          Datalog.Var(returnParams.head.name),
          Datalog.Evaluation(
            orgReturnParams,
            returnParams.head.typ,
            Scala(q"(..$metaParams) => (..$metaTerms)")
          )
        )
        val returnCons = orgReturnParams.zip(returnTerms).map { case ((v, _), t) =>
          Datalog.Eq(v, t)
        }
        Datalog.Body(cons ++ returnCons :+ transformReturnCond)
      } else if (isMonotoneAddMethod && methodDef.outType.isInstanceOf[TClass]) {
        // TODO: Remove this by merging it with the if-body above
        val returnParam = returnParams.head
        val returnTerm = returnTerms.head
        val clsName = methodDef.outType.asInstanceOf[TClass].ref.name.raw
        val coalescedCall = Datalog.Call(coalescedPatName(), Seq(returnTerm, Datalog.Var(returnParam.name)))
        Datalog.Body(cons :+ coalescedCall)
      } else {
        val returnCons = returnParams.zip(returnTerms).map { case (p, t) =>
          Datalog.Eq(Datalog.Var(p.name), t)
        }
        Datalog.Body(cons ++ returnCons)
      }
    }
    }

    if (methodDef.isStatic) {
      val pat = Datalog.Pattern(transVis(methodDef.vis), qualifiedName, argParams ++ returnParams,  bodies)
      if (methodDef.isMain)
        pat.addHint(MagicSetHints.Main(argParams.map(_ => true) ++ returnParams.map(_ => false)))
          .addHint(ObjectHints.AllocationRoot)
          .addHint(ObjectHints.FieldRoot)
      pat
    } else {
      Datalog.Pattern(transVis(methodDef.vis), qualifiedName, thisParam +: (argParams ++ returnParams), bodies)
    }
  }

  type Constraints = Seq[Datalog.Atom]
  type Tuple = Seq[Datalog.Term]
  type Alternatives[A] = Seq[A]

  type Path = Option[(SourceObject, Boolean)]
  type ExpRes = Alternatives[(Tuple, Constraints)]
  type StmRes = Alternatives[(Option[Tuple], Constraints, Path)]

  private def transStatements(stmts: Seq[Statement], path: Path, enclosingContent: ClassContent): StmRes = stmts match {
    case Nil =>
      Seq((None, Seq(), path))
    case s::rest =>
      val alternatives = for ((sReturn, sConstraints, sPath) <- transStatement(s, path, enclosingContent)) yield {
        if (sReturn.isDefined)
          Seq((sReturn, sConstraints, sPath))
        else {
          transStatements(rest, sPath, enclosingContent).map(res => (res._1, sConstraints ++ res._2, res._3))
        }
      }
      alternatives.flatten
  }

  private def transStatement(stmt: Statement, path: Path, enclosingContent: ClassContent): StmRes = stmt match {
    case ExprStmt(expression) =>
      for ((_, cons) <- transExpression(expression))
        yield (None, cons, path)

    case ReturnStmt(expression) =>
      for ((tup, cons) <- transExpression(expression))
        yield (Some(tup), cons, path)

    case IfStmt(cnd, thn, els) =>
      val cndTrans = transExpression(cnd)
      val thnTrans = transStatements(thn, path, enclosingContent)
      val elsTrans = transStatements(els, path, enclosingContent)

      val thnRes: StmRes =
        for ((Seq(cndTerm), cndCons) <- cndTrans;
             (thnTerm, thnCons, _) <- thnTrans)
        yield (thnTerm, cndCons ++ Seq(Datalog.Eq(cndTerm, Datalog.True)) ++ thnCons, Some(stmt.sourceObject -> true))
      val elsRes: StmRes =
        for ((Seq(cndTerm), cndCons) <- cndTrans;
             (elsTerm, elsCons, _) <- elsTrans)
        yield (elsTerm, cndCons ++ Seq(Datalog.Eq(cndTerm, Datalog.False)) ++ elsCons, Some(stmt.sourceObject -> false))
      thnRes ++ elsRes

    case fieldAssign@FieldAssignStmt(recv, name, expression) =>
      // to allow inheritance of attributes we use the classDef target of the field lookup
      val (classDef, fieldDef) = fieldAssign.target.getOrElse(throw new IllegalArgumentException(s"Unresolved field $name"))
      val qualifiedName = fieldPatName(classDef.name.raw, name.raw)

      val transRecv = for ((terms, cons) <- transExpression(recv)) yield {
        for ((argTerms, argCons) <- transExpression(expression)) yield {
          val call = Datalog.Call(qualifiedName, terms ++ argTerms)
          val ts = enclosingContent match {
            case c: ConstructorDef if c.isPrimary => Some(0)
            case _ => None
          }

          val fieldSet = call.addHint(ObjectHints.FieldSet(fixedTimestamp = ts))
          (None, cons ++ argCons :+ fieldSet, path)
        }
      }
      transRecv.flatten

    case VarDeclareStmt(name, typ, Some(expression), true) =>
      val vars = flattenVars(name.raw, typ).map(_._1)
      for ((terms, cons) <- transExpression(expression))
        yield (None, cons ++ vars.zip(terms).map { case (v, t) => Datalog.Eq(v, t) }, path)

    case VarPhiAssignStmt(name, _, ifStmt, thnName, elsName) =>
      val (sSource, sCond) = path.get
      // FIXME: For now this is always the case. We could remove the source object.
      assert(sSource == ifStmt.sourceObject)
      // This is just a renaming. We do not need to care about tuples here.
      val conditionVarName = if (sCond) thnName else elsName
      Seq(
          (None, Seq(Datalog.Eq(Datalog.Var(name.raw), Datalog.Var(conditionVarName.raw))), path)
      )

    case s =>
      throw new IllegalArgumentException(s"Statement ${s.getClass} declared by $s is not supported!")
  }

  private def transExpression(expression: Expression): ExpRes = expression match {
    case VarReadExpr(name) if name.raw == "_" | name.raw == "#" =>
      Seq((Seq(), Seq()))

    case VarReadExpr(name) =>
      val expTyp = expression.typ.getOrElse(throw new IllegalArgumentException(s"Untyped expression $expression"))
      Seq((flattenVars(name.raw, expTyp).map(_._1), Seq()))

    case fieldRead@FieldReadExpr(recv, targetName) =>
      // to allow inheritance of attributes we use the classDef target of the field lookup
      val (classDef, fieldDef) = fieldRead.target.getOrElse(throw new IllegalArgumentException(s"Unresolved field $targetName"))

      // TODO: Hack MonoMap
      fieldDef.typ match {
        case TClass(ref) if ref.target.isDefined && ref.target.get.isMonotoneMapClass => return Seq((Seq(), Seq()))
        case _ => // nothing
      }

      for ((terms, cons) <- transExpression(recv)) yield {
        // reading the result value should perform an aggregation instead
        if (classDef.isMonotoneClass && targetName.raw == "result") {
          val Some((valType, resType)) = classDef.montoneTypes
          val aggVarType = transDataType(resType)

          val args = terms ++ valType.flatten.map(_ => Datalog.Var(gensym.fresh("_")))
          val readAgg = Datalog.CustomAggregation(
            aggVarType,
            None,
            // TODO: Is there a way to pass this to the aggregation ? Otherwise we can not support constructor args
            // TODO: Actually there is a way: Append the coalesced object as first argument to the result tuple that we
            //  aggregate over
            //  E.g: Avg$__plus__(this: inca.runtime.data.ObjectID, value: Int, return$0: (Avg, Int, Double)) {
            //      coealesced(this, baseObj)
            //      retunr$0 = (baseObj, ..., ...)
            //    }
            //  Just make it static...
            // TODO: This uses a fixed allocId for now
            Scala(q"${Term.Name(classDef.name.raw)}(0).__aggregation__"),
            methodPatName(classDef.name.raw, AssignmentOp.AGG_ELEMENT.name.raw),
            args :+ Datalog.Var(gensym.fresh("_")),
            args.size
          )
          val aggVar = Datalog.Var(gensym.fresh("agg"))
          val resultComp = Datalog.Computed(aggVar, readAgg)
            .addHint(MagicSetHints.IgnoreCall)
            .addHint(MagicSetHints.FixedAdornment(args.map(_ => true) :+ false))

          // TODO: Coalesing / Uncoalesing

          val resultVars = flattenVars("result", resType, genFresh = true)
          val unpackCons = {
            if (resultVars.size > 1) {
              // we got a scala tuple back from the aggregation, unpack it
              resultVars.zipWithIndex.map { case ((v, ty), idx) =>
                Datalog.Computed(v, Datalog.Evaluation(
                  Seq(aggVar -> aggVarType),
                  ty,
                  Scala(q"(aggVar: ${aggVarType.asScala}) => aggVar.${Term.Name("_" + (idx + 1))}")
                ))
              }
            } else if (resType.isInstanceOf[TClass]) {
              // Todo: extend this to nested tuple
              // Todo: extend this to nested tuple
              val clsName = resType.asInstanceOf[TClass].ref.name.raw
              val uncoalescedCall = Datalog.Call(uncoalescedPatName(), Seq(aggVar, resultVars.head._1))
              Seq(uncoalescedCall)
              //Seq()
            } else {
              // we got a single value back from the aggregation
              Seq(Datalog.Eq(resultVars.head._1, aggVar))
            }
          }

          (resultVars.map(_._1), (cons :+ resultComp) ++ unpackCons)
        } else {
          val fieldReadVars = flattenVars(gensym.fresh(targetName.raw), fieldDef.typ).map(_._1)
          val fieldRead = Datalog.Call(fieldPatName(classDef.name.raw, targetName.raw), terms ++ fieldReadVars)
            .addHint(MagicSetHints.FixedAdornment(terms.map(_ => true) ++ fieldReadVars.map(_ => true)))
            .addHint(ObjectHints.FieldGet)
          (fieldReadVars, cons :+ fieldRead)
        }
      }

    case constrExpr@ConstructorExpr(classRef, args) =>
      val classDef = classRef.target.getOrElse(throw new IllegalArgumentException(s"Unresolved classRef ${classRef.name}"))

      // constructors are never implicitly inherited !
      val (constructedVar, constrComp) = createObject(classDef.name.raw, classDef.typ)
      val argRes = args.map(e => transExpression(e))
      val constructorDef = constrExpr.target.getOrElse(throw new IllegalArgumentException(s"Unresolved constructor $constrExpr"))

      val constrName = constructorPatName(classRef.name.raw) + constructorDef.signature

      // create single call constraint when no arguments are passed
      if (argRes.isEmpty)
        return Seq((Seq(constructedVar), Seq(constrComp, Datalog.Call(constrName, Seq(constructedVar)))))

      for (tups <- TupleOps.cartesianProduct(argRes)) yield {
        val (argTerms, argCons) = tups.unzip
        (Seq(constructedVar), argCons.flatten ++ Seq(constrComp, Datalog.Call(constrName, constructedVar +: argTerms.flatten)))
      }

    case superExpr@SuperExpr(args) =>
      val argRes = args.map(e => transExpression(e))
      val (classDef, constructorDef) = superExpr.target.getOrElse(throw new IllegalArgumentException(s"Unresolved constructor $superExpr"))
      val constrName = constructorSuperPatName(classDef.name.raw) + sep + constructorDef.signature

      for (tups <- TupleOps.cartesianProduct(argRes)) yield {
        val (argTerms, argCons) = tups.unzip
        (Seq(), argCons.flatten ++ Seq(Datalog.Call(constrName, Datalog.Var("this") +: argTerms.flatten)))
      }

    case methodCallExp@MethodCallExpr(recv, fun, args, isFix) =>
      val argRes = args.map(e => transExpression(e))

      val (classDef, methodDef) = methodCallExp.target.getOrElse(throw new IllegalArgumentException(s"Unresolved method $methodCallExp"))

      // TODO: Hack MonoMap
      if (classDef.isMonotoneMapClass && methodDef.name.raw == "keys") {
        // query all keys
        val keysVar = Datalog.Var(gensym.fresh("keys"))
        val queryCall = Datalog.Call(monoMapPatName(classDef.name.raw), Seq(keysVar, Datalog.Var(gensym.fresh("_")), Datalog.Var(gensym.fresh("_"))))
          .addHint(MagicSetHints.IgnoreCall)
        //.addHint(MagicSetHints.)
        return Seq((Seq(keysVar), Seq(queryCall)))
      }

      val outVars = if (classDef.isMonotoneClass && fun.raw == AssignmentOp.AGG_ELEMENT.name.raw)
        Seq(Datalog.Var(gensym.fresh("methodCall")))
      else
        flattenVars(gensym.fresh("methodCall"), methodDef.outType).map(_._1)

      val qualifiedName = dispatchPatName(methodDef.name + sep + methodDef.signature)

      val Seq((terms, atoms)) = (for ((terms, cons) <- transExpression(recv)) yield {
        if (argRes.isEmpty)
          return Seq((outVars, cons :+ Datalog.Call(qualifiedName, terms ++ outVars)))

        for (tups <- TupleOps.cartesianProduct(argRes)) yield {
          val (argTerms, argCons) = tups.unzip

          // TODO: Hack MonoMap
          if (classDef.isMonotoneMapClass) {
            val monoTypes = classDef.montoneTypes.get
            val resultType = monoTypes._2.asInstanceOf[TClass]

            val (addVars, addCons) =
              if (methodDef.name.raw == "get") {
                // perform aggregation over map relation
                val readAgg = Datalog.CustomAggregation(
                  transDataType(resultType),
                  None,
                  Scala(q"${Term.Name(resultType.ref.name.raw)}(0).__aggregation__"),
                  monoMapPatName(classDef.name.raw),
                  argTerms.head :+ Datalog.Var(gensym.fresh("_")) :+ Datalog.Var(gensym.fresh("agg")),
                  2
                )
                val outVar = Datalog.Var(gensym.fresh("out"))
                val aggVar = Datalog.Var(gensym.fresh("agg"))
                val resultComp = Datalog.Computed(aggVar, readAgg)
                  .addHint(MagicSetHints.IgnoreCall)

                (Seq(outVar), Seq(resultComp, Datalog.Call(uncoalescedPatName(), Seq(aggVar, outVar))))
              } else if (methodDef.name.raw == AssignmentOp.AGG_ELEMENT.name.raw) {
                val objVar = Datalog.Var(gensym.fresh("obj"))
                val coalescedCall = Datalog.Call(coalescedPatName(), Seq(argTerms.flatten.toSeq(1), objVar))
                (Seq(), Seq(coalescedCall, Datalog.Call(monoMapPatName(classDef.name.raw), argTerms.flatten :+ objVar)))
              } else {
                (Seq(), Seq())
              }
            (addVars, cons ++ argCons.flatten ++ addCons)
          } else {
            (outVars, cons ++ argCons.flatten ++ Seq(Datalog.Call(qualifiedName, terms ++ argTerms.flatten ++ outVars)))
          }
        }
      }).flatten

      // TODO: Does only work for Unit return types for now. Otherwise the return argument is not bound
      //  We might need some encoding for empty set to support none unit methods
      //  For unit methods we do not need to change anything on the call side
      //  For none unit methods we would need to filter / aggregate the result of the enclosing method
      //  For the Abstract syntax graph we would need to aggregate the visitVar method calls
      if (isFix) {
        if (methodDef.returnsUnit)
          Seq((Seq(), Seq()), (terms, atoms))
        else
          throw new RuntimeException("Fixpoint iterations for none unit method are currently not supported.")
      } else
        Seq((terms, atoms))

    case TypeCastExpr(recv, toTyp) =>
      for ((Seq(term), cons) <- transExpression(recv)) yield {
        val castCall = Datalog.Call(castPatName, Seq(term, Datalog.StringConstant(toTyp.toString)))
        (Seq(term), cons :+ castCall)
      }

    case InstanceOfExpr(recv, ofTyp) =>
      for ((Seq(term), cons) <- transExpression(recv)) yield {
        val outVar = Datalog.Var(gensym.fresh("isInstance"))
        val instanceOfCall = Datalog.Call(instanceOfPatName, Seq(term, Datalog.StringConstant(ofTyp.toString), outVar))
        (Seq(outVar), cons :+ instanceOfCall)
      }

    case NullExpr() =>
      val (nullVar, nullComp) = createNullObject()
      val nullConstrCall = Datalog.Call(constructorPatName("Null"), Seq(nullVar))
      Seq((Seq(nullVar), Seq(nullComp, nullConstrCall)))

    case TupleExpr(exps) =>
      if (exps.isEmpty)
        Seq((Seq(), Seq()))
      else {
        val expRes = exps.map(e => transExpression(e))
        for (tups <- TupleOps.cartesianProduct(expRes)) yield {
          val (terms, cons) = tups.unzip
          (terms.flatten, cons.flatten)
        }
      }

    case TupleReadExpr(recv, index) =>
      def numberOfElements(ty: Type): Int = {
        ty match {
          case TTuple(ts) => ts.foldLeft(0)(_ + numberOfElements(_)) //ts.map(numberOfElements).sum
          case _ => 1
        }
      }

      // if the accessed element is a tuple we need to return more than one element
      val startIdx = index.raw - 1
      recv.typ match {
        case Some(TTuple(ts)) =>
          val elementsToRead = numberOfElements(ts(startIdx))
          for ((terms, cons) <- transExpression(recv)) yield {
            (terms slice(startIdx, startIdx + elementsToRead), cons)
          }
        case _ => throw new IllegalArgumentException(s"Can not perform tuple read on expression of type ${recv.typ}")
      }

    case SetExpr(exps, tty) =>
      if (exps.isEmpty) {
        // TODO: How do we encode empty sets?
        throw new RuntimeException("Empty sets are currently not supported!")
        /*val arity = expression.typ match {
          case Some(TSet(TTuple(ts))) => ts.size
          case Some(TSet(_)) => 1
          case None => throw new IllegalArgumentException(s"Untyped expression: $expression")
        }
        val extName = "ext_empty_set_arity" + arity
        val retVar = Datalog.Var(gensym.fresh("empty"))
        val extEmptySet = Datalog.ExtensionalCall(extName, Seq(retVar))
        Seq((Seq(retVar), Seq(extEmptySet)))*/
      } else
        exps.flatMap(transExpression)

    case setMember@SetMemberExpr(name, recv, predicate) =>
      val typ = setMember.typ.getOrElse(throw new IllegalArgumentException(s"Missing type for expression $setMember"))
      val vars = flattenVars(name.raw, typ).map(_._1)
      val transRecv = transExpression(recv)
      val transPred = if (predicate.isDefined) transExpression(predicate.get) else Seq()
      for ((recvTerms, recvCons) <- transRecv) yield {
        val eqs = vars.zip(recvTerms).map(vt => Datalog.Eq(vt._1, vt._2))
        val predicates = transPred.flatMap { case (predTerms, predCons) =>
          predCons ++ predTerms.map(pt => Datalog.Eq(pt, Datalog.True))
        }
        (vars, recvCons ++ eqs ++ predicates)
      }

    case SetComprehension(exps, body) =>
      val transSetMember = exps.map(transExpression) ++ Seq(Seq((Seq(), Seq())))
      val transBody = transExpression(body)
      for (ms <- TupleOps.cartesianProduct(transSetMember);
           (bTerms, bCons) <- transBody) yield {
        val (_, mCons) = ms.unzip
        (bTerms, mCons.flatten ++ bCons)
      }

    case setFold@SetFold(recv, projection, opClass, opMethod, neutral) =>
      val aggIndex = setFold.aggIndex
      val aggType = recv.typ match {
        case Some(TSet(ty)) => ty
        //case Some(TSet(TTuple(_))) => throw new IllegalArgumentException("Aggregation over tuples is unsupported!")
        case _ => throw new RuntimeException("Could not get type of set !")
      }

      val aggregandPat = aggType.flatten(aggIndex) match {
        case td: TClass =>
          val pat = generatePattern(recv, aggregatePatName(opClass.name.raw, opMethod.raw))

          val outParamSize = aggType.flatten.size
          val leftParams = pat.params.slice(0, pat.params.size - outParamSize + aggIndex)
          val rightParams = pat.params.slice(leftParams.size + 1, pat.params.size)
          val oldOutName = pat.params(leftParams.size).name
          val newOutName = gensym.fresh("out")
          val newOutParam = Datalog.Param(newOutName, transDataType(td))
          val coalesceCon = Datalog.Call(coalescedPatName(), Seq(Datalog.Var(oldOutName), Datalog.Var(newOutName)))
          pat.copy(params = leftParams ++ (newOutParam +: rightParams), bodies = pat.bodies.map(b => Datalog.Body(b.atoms :+ coalesceCon)))
        case _ =>
          generatePattern(recv, aggregatePatName(opClass.name.raw, opMethod.raw))
      }

      generatedPatterns += aggregandPat

      val projRes = projection.map(transExpression)
      for (tups <- TupleOps.cartesianProduct(projRes)) yield {
        val (projTerms, projCons) = tups.unzip

        val expTyp = setFold.typ.getOrElse(throw new IllegalArgumentException(s"Cannot compile untyped fold $setFold"))
        val aggFun = genScala.genAggregation(opMethod + "Agg", neutral, opClass.name.raw, opMethod.raw, expTyp)

        val freeArgs = recv.vars.toSeq.flatMap { case (v, ty) => flattenVars(v.raw, ty.get).map(_._1) }
        val outVars = projTerms.map { p =>
           if (p.isEmpty)
             Datalog.Var(gensym.fresh("out"))
           else
             p.head
        }
        val aggregation = Datalog.CustomAggregation(transDataType(expTyp), Some("Fold aggregation."), Scala(aggFun), aggregandPat.name, freeArgs ++ outVars, freeArgs.size + aggIndex)
        val foldVar = Datalog.Var(gensym.fresh("fold"))
        val compCon = Datalog.Computed(foldVar, aggregation)

        aggType.flatten(aggIndex) match {
          case td: TClass =>
            val foldVarUncoalesced = Datalog.Var(gensym.fresh("fold"))
            val uncoalesce = Datalog.Call(uncoalescedPatName(), Seq(foldVar, foldVarUncoalesced))
            (Seq(foldVarUncoalesced), projCons.flatten :+ compCon :+ uncoalesce)
          case _ =>
            (Seq(foldVar), projCons.flatten :+ compCon)
        }
      }

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

    case BaseApplyInfixExpr(left, op, right)
      if op.tree.value == "++" && left.typ.exists(_.isInstanceOf[TSet]) && right.typ.exists(_.isInstanceOf[TSet]) =>
        transExpression(left) ++ transExpression(right)

    case BaseApplyInfixExpr(left, op, right)
      if op.tree.value == "&" && left.typ.exists(_.isInstanceOf[TSet]) && right.typ.exists(_.isInstanceOf[TSet]) =>
      val transLeft = transExpression(left)
      val transRight = transExpression(right)

      // FIXME: I don't think we need a rename here right now. There is only one way we get a name clash. This can only
      //  happen if both left and right perform a variable read with the same name. A variable read with the same name
      //  always references the same variable.
      for ((leftTerms, leftCons) <- transLeft;
           (rightTerms, rightCons) <- transRight) yield {
        val eqTerms = leftTerms.zip(rightTerms).map { case (l, r) => Datalog.Eq(l, r) }
        (leftTerms, leftCons ++ rightCons ++ eqTerms)
      }

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
          Datalog.Evaluation(
            Seq(leftTerm -> transType(left.typ.get), rightTerm -> transType(right.typ.get)),
            transType(resType),
            Scala(funCode)
          )
        )
        (Seq(evalOut), leftCons ++ rightCons ++ Seq(evalConstraint))
      }

    case exps =>
      throw new IllegalArgumentException(s"Expression not supported $exps")
  }

  private def generatePattern(exp: Expression, basename: String): Datalog.Pattern = {
    val name = gensym.freshGlobal(basename)
    val vars = exp.vars.toSeq.flatMap { case (v, ty) => flattenVars(v.raw, ty.get) }
    val params = vars.map { case (v, ty) => Datalog.Param(v.name, ty) }
    val expTys = exp.typ.getOrElse(throw new IllegalArgumentException(s"Cannot compile untyped expression $exp")).flatten
    val outParams = expTys.map(ty => Datalog.Param(gensym.fresh("out"), transType(ty)))

    val bodies = for ((terms, cons) <- transExpression(exp))
      yield Datalog.Body(cons ++ outParams.zip(terms).map(pt => Datalog.Eq(Datalog.Var(pt._1.name), pt._2)))

    Datalog.Pattern(None, name, params ++ outParams, bodies)
  }


  private def flattenParam(name: String, typ: Type, genFresh: Boolean): Seq[Datalog.Param] =
    flattenVars(name, typ, genFresh).map { case (v, ty) => Datalog.Param(v.name, ty) }

  private def flattenVars(name: String, ty: Type, genFresh: Boolean = false): Seq[(Datalog.Var, Datalog.Type)] =
    ty match {
      case TSet(ty) =>
        flattenVars(name, ty, genFresh)
      case TTuple(ts) =>
        ts.zipWithIndex.flatMap { case (ty, ix) => flattenVars(name + "_" + (ix + 1), ty, genFresh) }
      case ty =>
        Seq(Datalog.Var(if (genFresh) gensym.fresh(name) else name) -> transType(ty))
    }

  private def transVis(vis: Option[Visibility]): Option[Datalog.Visibility] =
    vis.map { case Private => Datalog.Private }

  private def transDataType(typ: Type): Datalog.Type = typ match {
    case TClass(ClassRef(name)) => Datalog.TData(name.raw)
    case TAny | TNull | TScala(_) => Datalog.TScala(Scala(typ.asScala))
    case TTuple(ts) => Datalog.TScala(Scala(t"(..${ts.map(genScala.transType).toList})"))
    case _ => throw new IllegalArgumentException(s"Cannot translate $typ to Scala type")
  }

  private def transType(typ: Type): Datalog.Type = typ match {
    case TAny => Datalog.TAny
    case TNull | TClass(_) => GP_URI
    case TScala(ty) => Datalog.TScala(ty)
    case TSet(ty) => transType(ty)
    // Note: Most of the times we want to flatten the tuple, but for monotones we expect this to work
    case TTuple(ts) => Datalog.TScala(Scala(t"(..${ts.map(t => transType(t).asScala).toList})"))
    case _ => throw new IllegalArgumentException(s"Cannot translate $typ to Datalog")
  }
}