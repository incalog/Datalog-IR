package inca.frontend.objectoriented.lowering

import inca.backend.hints.{MagicSetHints, ObjectHints, OptimizationHints}
import inca.backend.ir.Datalog
import inca.backend.ir.Datalog.Computed
import inca.compiler.SourceObject
import inca.frontend.objectoriented.core._
import inca.frontend.objectoriented.lowering.GenerateDatalog._
import inca.runtime.data.objectoriented.{Identity, NullID, ObjectID, StructuralID}
import inca.util.TupleOps
import inca.util.Scala.{symbolOf, typeOf}
import inca.util.{Gensym, Scala}

import scala.collection.immutable.MultiDict
import scala.collection.mutable.ListBuffer
import scala.meta.{Lit, Stat, Term, Type => MetaType}
import scala.meta.quasiquotes._

object GenerateDatalog {
  private val sep: String = "$"
  private val internalPrefix: String = "_" + sep

  val castPatName: String = internalPrefix + "cast"
  val instanceOfPatName: String = internalPrefix + "instanceOf"

  def staticMethodPatName(classDef: ClassDef, methodDef: MethodDef): String = s"${classDef.name.raw}$sep${methodDef.name.raw}"
  def methodPatName(methodDef: MethodDef): String = s"${methodDef.name.raw}$sep${methodDef.signature}"
  def dispatchPatName(methodNameWithSignature: String): String = "dispatch" + sep + s"${methodNameWithSignature}"
  def aggregatePatName(className: String, methodName: String): String = s"${internalPrefix}aggregate_$className${sep}$methodName"
  def coalescedPatName(className: String): String = s"${internalPrefix}coalesced_$className"
  def uncoalescedPatName(className: String): String = s"${internalPrefix}uncoalesced_$className"
  def constructorPatName(className: String): String = className + sep
  def constructorSuperPatName(className: String): String = s"${internalPrefix}super_$className"
  def fieldPatName(className: String, fieldName: String): String = s"$className$sep$sep$fieldName"

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

  val tyID: meta.Type = typeOf[Identity]
  val oNID: meta.Term = symbolOf(NullID)

  val oOID: meta.Term = symbolOf(ObjectID)
  val tyOID: meta.Type = typeOf[ObjectID]

  val oSID: meta.Term = symbolOf(StructuralID)
  val tySID: meta.Type = typeOf[StructuralID]

  def GP_URI: Datalog.TScala = Datalog.TScala(Scala(tyID))

  private def createObject(className: String, typ: Type): (Datalog.Var, Computed) = {
    val objVar = Datalog.Var(gensym.fresh("obj"))
    val constrScalaFun = Term.Function(Nil, q"$oOID($className)")
    val constrComp = Datalog.Computed(objVar, Datalog.Evaluation(
      Seq(), transType(typ), Scala(constrScalaFun))
    )
    (objVar, constrComp.addHint(ObjectHints.AllocationInit))
  }

  private def createCaseClassObject(className: String, typ: Type, fields: Seq[(String, Type, Tuple)]): (Datalog.Var, Computed) = {
    val objVar = Datalog.Var(gensym.fresh("obj"))

    val flattenFields = fields.flatMap {
      case (f, ty, terms) => flattenVars(f, ty).map(_._1.name).zip(terms)
    }.toList

    val fieldArgs = flattenFields.map { case (f, _) => Term.Name(gensym.fresh(f)) }
    val fieldParams = fieldArgs.map(a => Term.Param(Nil, a, Some(Datalog.TAny.asScala), None))
    val fieldTuples = flattenFields.zip(fieldArgs).map { case ((f, _), t) => Term.Tuple(List(Lit.String(f), t)) }

    val constrScalaFun = q"""(..$fieldParams) => $oSID($className, ..$fieldTuples)"""
    val constrComp = Datalog.Computed(objVar, Datalog.Evaluation(
      flattenFields.map(_._2 -> Datalog.TAny),
      transType(typ),
      Scala(constrScalaFun))
    )
    (objVar, constrComp)
  }

  private def objNullGuard(objVar: Datalog.Var, objType: Datalog.Type, isNull: Boolean) = Datalog.Computed(
    if (isNull) Datalog.True else Datalog.False,
    Datalog.Evaluation(
      Seq(objVar -> objType),
      Datalog.TScalaBoolean,
      Scala(q"""(objOrNull: Any) => objOrNull == null""")
    )
  )

  private def createNullObject(): (Datalog.Var, Computed) = {
    val objVar = Datalog.Var(gensym.fresh("null"))
    val constrScalaFun = Term.Function(Nil, q"$oNID()")
    val constrComp = Datalog.Computed(objVar, Datalog.Evaluation(
      Seq(), transDataType(TNull), Scala(constrScalaFun))
    )
    (objVar, constrComp)
  }

  private def getURIAttribute(uri: Datalog.Term, attribute: String, out: Datalog.Term, outType: Datalog.Type): Datalog.Computed = {
    val compAttr = Term.Name(attribute)
    val compArg = Term.Name("uri")
    val compParam = Term.Param(Nil, compArg, Some(GP_URI.asScala), None)
    Datalog.Computed(
      out, Datalog.Evaluation(Seq(uri -> GP_URI), outType, Scala(q"($compParam) => $compArg.$compAttr")
      )
    )
  }

  private def getSIDField(uri: Datalog.Term, attribute: String, out: Datalog.Term, outType: Datalog.Type): Datalog.Computed = {
    val compAttr = Term.Name(attribute)
    val compArg = Term.Name("uri")
    val compParam = Term.Param(Nil, compArg, Some(GP_URI.asScala), None)
    Datalog.Computed(out, Datalog.Evaluation(
      Seq(uri -> GP_URI),
      outType,
      Scala(q"($compParam) => $compArg.readField[${outType.asScala}]($attribute)")
    )
    )
  }

  private def getURIIsNull(uri: Datalog.Term, out: Datalog.Term): Datalog.Computed = {
    getURIAttribute(uri, "isNull", out, Datalog.TScalaBoolean)
  }

  private def getURITyp(uri: Datalog.Term, out: Datalog.Term): Datalog.Computed = {
    getURIAttribute(uri, "typ", out, Datalog.TScalaString)
  }

  def transModule(): Datalog.Module = {
    val Module(name, imports, classes) = coreModule
    val concreteClasses = classes.filter(c => !c.isAbstract)

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
     * For a given class collect for each method the implementation class of the method.
     */
    def collectMethods(classDef: ClassDef)(implClass: ClassDef = classDef): Map[(String, ClassDef), (ClassDef, MethodDef)] = {
      val methods = implClass.content.flatMap {
        case m: MethodDef if implClass.isMonotoneMapClass && Seq("keys", "get").contains(m.name.raw) => None
        case m: MethodDef if !m.isStatic =>  Some((methodPatName(m), classDef) -> (implClass, m))
        case _ => None
      }.toMap

      val parentMethods = implClass.parentClassRefs.flatMap { ref =>
        val parentClassDef = ref.target.getOrElse(throw new IllegalArgumentException(s"Unresolved class ${ref.name.raw}"))
        collectMethods(classDef)(parentClassDef)
      }.toMap
      // We rely on the default map collision behaviour to find the concrete implementation class
      parentMethods ++ methods
    }

    // Translate dynamic dispatching
    val dispatchFacts: Map[String, Seq[(ClassDef, (ClassDef, MethodDef))]] = classes.flatMap(c => collectMethods(c)())
      .groupBy(_._1._1).view.mapValues(_.map(v => v._1._2 -> v._2)).toMap
    val dispatchPats = dispatchFacts.map {
      case (qualifiedMethodName, classMapping) =>
        Datalog.Pattern(None, dispatchPatName(qualifiedMethodName), Seq(
          Datalog.Param("className", Datalog.TScalaString),
          Datalog.Param("implClass", Datalog.TScalaString),
        ), classMapping.map { case (c, (implC, _)) =>
          Datalog.Body(Seq(
            Datalog.Eq(Datalog.Var("className"), Datalog.StringConstant(c.name.raw)),
            Datalog.Eq(Datalog.Var("implClass"), Datalog.StringConstant(implC.name.raw)),
          ))
        }).addHint(MagicSetHints.NoInputRelation)
    }.toSeq

    // Translate all methods
    val methodPats = dispatchFacts.map {
      case (qualifiedMethodName, clsMapping) => transMethodWithSameQualifiedName(qualifiedMethodName, clsMapping.map(_._2))
    }.toSeq

    val uriParam = Datalog.Param("uri", GP_URI)
    val uriVar = Datalog.Var("uri")
    val objVar = Datalog.Var("obj")

    // Uncoalesced Null
    val uncoalescedObjType = Datalog.TScala(Scala(t"Any"))
    val uncoalescedObjParam = Datalog.Param("obj", uncoalescedObjType)
    val (nullVar, nullComp) = createNullObject()
    val uncoalescedNullBody = Datalog.Body(
      Seq(
        objNullGuard(objVar, uncoalescedObjType, isNull = true),
        nullComp,
        Datalog.Call(constructorPatName("Null"), Seq(nullVar)),
        Datalog.Eq(uriVar, nullVar)
      )
    )
    val uncoalescedNullPat = Datalog.Pattern(
      None, uncoalescedPatName("Null"), Seq(uncoalescedObjParam, uriParam), Seq(uncoalescedNullBody)
    )

    // Coalesced Null
    val coalescedObjType = Datalog.TScala(Scala(t"Null"))
    val coalescedObjParam = Datalog.Param("obj", coalescedObjType)
    val genNullObj = Datalog.Computed(objVar, Datalog.Evaluation(Seq(), coalescedObjType, Scala(q"() => null")))
    val coalescedNullBody = Datalog.Body(
      Seq(getURIIsNull(uriVar, Datalog.True), genNullObj)
    )
    val coalescedNullPat = Datalog.Pattern(
      None, coalescedPatName("Null"), Seq(uriParam, coalescedObjParam), Seq(coalescedNullBody)
    )

    (methodPats ++ dispatchPats) :+ coalescedNullPat :+ uncoalescedNullPat
  }

  private def transNull(): Datalog.Pattern = gensym.scoped {
    Datalog.Pattern(None, constructorPatName("Null"), Seq(Datalog.Param("this", transType(TNull))), Seq(
      Datalog.Body(Seq())
    ))
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
    val clsPattern = classDef.content.flatMap {
      case field: FieldDef if !classDef.isCaseClass =>
        Seq(transField(classDef, field))
      case method: MethodDef if method.isStatic =>
        Seq(transStaticMethod(classDef, method))
      case constructor: ConstructorDef if !classDef.isCaseClass =>
        Seq(
          transConstructor(classDef, constructor),
          transSuper(classDef, constructor)
        )
      case _ => // Member methods are handled in dynamic dispatch translation
        Seq()
    }
    if (!classDef.isDefunAuxiliary)
      clsPattern :+ generateConstructorCoalesced(classDef) :+ generateConstructorUncoalesced(classDef)
    else
      clsPattern
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

    val (fieldVars, readFields) = fields.map { f =>
      var fieldReadVars: Seq[Datalog.Var] = Seq()
      var fieldReadCalls: Seq[Datalog.Atom] = Seq()
      if (classDef.isCaseClass) {
        // Read fields from SID for case classes
        val res = flattenVars(f.name.raw, f.typ).map { case (f, ty) =>
          val outVar = Datalog.Var(gensym.fresh(f.name))
          val comp = getSIDField(uriVar, f.name, outVar, ty)
          (Seq(outVar), Seq(comp))
        }.unzip
        fieldReadVars = res._1.flatten
        fieldReadCalls = res._2.flatten
      } else {
        // Read fields from field relation for normal classes
        fieldReadVars = flattenVars(gensym.fresh(f.name.raw), f.typ).map(_._1)
        fieldReadCalls = Seq(Datalog.Call(fieldPatName(className, f.name.raw), uriVar +: fieldReadVars)
          .addHint(MagicSetHints.FixedAdornment(true +: fieldReadVars.map(_ => true)))
          .addHint(ObjectHints.FieldGet)
        )
      }
      // coalesced fields if required
      val (coalescedChildCalls, vars) = fieldReadVars.zip(f.typ.flatten).map { case (v, t) =>
        t match {
          case TClass(ClassRef(clsName)) =>
            val coalescedChildVar = Datalog.Var(gensym.fresh(v.name))
            val coalescedChildCall = Seq(Datalog.Call(coalescedPatName(clsName.raw), Seq(v, coalescedChildVar)))
            (coalescedChildCall, coalescedChildVar)
          case _ =>
            (Seq(), v)
        }
      }.unzip
      (vars, fieldReadCalls ++ coalescedChildCalls.flatten)
    }.unzip

    // read and pass the allocation id and all fields to the scala function
    val fieldVarsFlat = uriVar +: fieldVars.flatten
    val fieldTypes = tyID +: fields.flatMap(_.typ.flatten).map(genScala.transType)

    val constrArgs = fieldVarsFlat.map { v => Term.Name(v.name) }.toList
    val constrParams = constrArgs.zip(fieldTypes).map {
      case (vt, t) => Term.Param(Nil, vt, Some(t), None)
    }

    val constrScalaFun = Term.Function(
      constrParams,
      Term.Apply(Term.Select(Term.Name(className), Term.Name("apply")), constrArgs)
    )

    val fieldDataTypes = GP_URI +: fields.flatMap(_.typ.flatten).map(transDataType)
    val evalParams = fieldVarsFlat.zip(fieldDataTypes).map { case (v, t) => v -> t }
    val genOutObj = Datalog.Computed(objVar, Datalog.Evaluation(evalParams, objType, Scala(constrScalaFun)))
    val clsGuard = Datalog.Computed(
      Datalog.StringConstant(classDef.name.raw),
      Datalog.Evaluation(
        Seq(Datalog.Var("uri") -> transType(classDef.typ)),
        Datalog.TScalaString,
        Scala(q"(obj: $tyID) => obj.typ")
      )
    )
    val bodyWithObject = Datalog.Body(
      clsGuard +: getURIIsNull(uriVar, Datalog.False) +: readFields.flatten :+ genOutObj
    )

    val directSubclasses = coreModule.classes.filter { cls =>
      cls.parentClassRefs.exists(_.name.raw == className)
    }
    val subclassBodies = directSubclasses.map { subCls =>
      Datalog.Body(Seq(
          Datalog.Call(coalescedPatName(subCls.name.raw), Seq(uriVar, objVar))
      ))
    }

    val bodyWithNull = Datalog.Body(
      Seq(Datalog.Call(coalescedPatName("Null"), Seq(uriVar, objVar)))
    )
    val bodies = bodyWithObject +: subclassBodies :+ bodyWithNull
    val params = Seq(uriParam, objParam)
    val constrCoalescedPat = Datalog.Pattern(None, coalescedPatName(classDef.name.raw), params, bodies)
    constrCoalescedPat
  }

  private def generateConstructorUncoalesced(classDef: ClassDef): Datalog.Pattern = gensym.scoped {
    import scala.meta.Term

    val fields = classDef.fields.flatMap {
      case FieldDef(_, _, Name(name), ty, _, _) if ty.asSet.isDefined =>
        println(s"WARNING: Can not uncoalesced field ${classDef.name.raw}.$name with set type!")
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

    val fieldVarsAndComps = fields.map { f =>
      // FIXME: This tuple check will fail if we allow coalescing sets, since tuples can then be contained inside a
      //  set. For now a Tuple can only be the outermost type at this source position.
      val isTuple = f.typ.isInstanceOf[TTuple]
      val (vars, comps) = f.typ.flatten.zipWithIndex.map {
        case (ty@TClass(ClassRef(name)), i) =>
          val (fieldReadVar, fieldReadComp) = readFieldComp(f.name.raw, ty, if (isTuple) Some(i) else None)
          val childUri = Datalog.Var(gensym.fresh(f.name.raw))
          val call = Datalog.Call(uncoalescedPatName(name.raw), Seq(fieldReadVar, childUri))
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

    val identityObjectOptionType = TScala(Scala(t"Option[$tyID]"))
    val (identityObjectVar, identityObjectComp) = readFieldComp("__identity", identityObjectOptionType, None)

    def objectIsDefinedComp(isDefined: Boolean) = Datalog.Computed(
      if (isDefined) Datalog.True else Datalog.False,
      Datalog.Evaluation(
        Seq(identityObjectVar -> transType(identityObjectOptionType)),
        Datalog.TScalaBoolean,
        Scala(q"""(identityObjOption: Option[$tyID]) => identityObjOption.isDefined""")
      )
    )

    // existing object was returned
    val allSetterCalls = {
      if (classDef.isCaseClass) {
        // Case classes are immutable, therefore we don't need to update the object
        Seq()
      } else {
        fieldVarsAndComps.flatMap { case (f, vars, comps) =>
          comps.flatten :+ setterCall(f, vars).addHint(ObjectHints.FieldSet())
        }
      }
    }
    val assignUri = Datalog.Computed(uriVar, Datalog.Evaluation(
      Seq(identityObjectVar -> transType(identityObjectOptionType)),
      GP_URI,
      Scala(q"(identityOption: Option[$tyID]) => identityOption.get")
    ))

    val clsGuard = Datalog.Computed(
      Datalog.StringConstant(className),
      Datalog.Evaluation(
        Seq(Datalog.Var("obj") -> Datalog.TScala(Scala(t"AnyRef"))),
        Datalog.TScalaString,
        Scala(q"(obj: AnyRef) => obj.getClass.getSimpleName")
      )
    )

    val bodyWithExistingObject = Datalog.Body(
      objNullGuard(objVar, objType, isNull=false) +: clsGuard +: identityObjectComp +: objectIsDefinedComp(true) +: assignUri +: allSetterCalls
    )

    // new object was created in scala
    val allSetterCallsWithFixedTimestamp = fieldVarsAndComps.flatMap { case (f, vars, comps) =>
      comps.flatten :+ setterCall(f, vars).addHint(ObjectHints.FieldSet(fixedTimestamp = Some(0)))
    }
    val objComps = if (classDef.isCaseClass) {
      val inputFields = fieldVarsAndComps.map { case (f, v, _) => (f.name.raw, f.typ, v) }
      val fieldComps = fieldVarsAndComps.flatMap { case (_, _, c) => c }.flatten
      val (term, comp) = createCaseClassObject(className, classDef.typ, inputFields)
      fieldComps :+ comp :+ Datalog.Eq(uriVar, term)
    } else {
      val (term, comp) = createObject(className, classDef.typ)
      comp +: Datalog.Eq(uriVar, term) +: allSetterCallsWithFixedTimestamp
    }

    val bodyWithNewObject = Datalog.Body(
      objNullGuard(objVar, objType, isNull=false)  +: clsGuard +: identityObjectComp +: objectIsDefinedComp(false) +: objComps
    )

    // object is null
    val bodyWithNull = Datalog.Body(
      Seq(Datalog.Call(uncoalescedPatName("Null"), Seq(objVar, uriVar)))
    )

    // uncoalesced subclasses
    val directSubclasses = coreModule.classes.filter { cls =>
      cls.parentClassRefs.exists(_.name.raw == className)
    }
    val subclassBodies = directSubclasses.map { subCls =>
      Datalog.Body(Seq(
        Datalog.Call(uncoalescedPatName(subCls.name.raw), Seq(objVar, uriVar))
      ))
    }

    val params = Seq(objParam, uriParam)
    val bodies = bodyWithNull +: bodyWithExistingObject +: bodyWithNewObject +: subclassBodies
    Datalog.Pattern(None, uncoalescedPatName(classDef.name.raw), params, bodies)
  }

  private def transFieldInitBody(classDef: ClassDef): Seq[(Seq[Datalog.Term], Datalog.Body)] = {
    /**
     * Collect all fields from classDef and all inherited fields from all parents
     */
    def collectFields(classDef: ClassDef): Seq[(ClassDef, FieldDef)] = {
      val fields = classDef.fields.map((classDef, _))
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
    val bodies = constrBodies.flatMap { cB =>
      fieldInitBodies.map {
        fB => Datalog.Body(fB.atoms ++ cB.atoms)
      }
    }
    Datalog.Pattern(None, qualifiedName, thisParam +: params, bodies)
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
    val qualifiedName = fieldPatName(classDef.name.raw, fieldDef.name.raw)
    val params = Datalog.Param("this", transType(classDef.typ)) +:
      flattenParam(fieldDef.name.raw, fieldDef.typ, genFresh = false)
    val pat = Datalog.Pattern(None, qualifiedName, params, Seq())
    pat.addHint(ObjectHints.Field(fieldDef.immutable))
  }

  private def transStaticMethod(classDef: ClassDef, methodDef: MethodDef): Datalog.Pattern = gensym.scoped {
    val argParams = methodDef.params.flatMap { case Param(name, typ) =>
      flattenParam(name.raw, typ, genFresh = false)
    }

    val returnParams =
      if (methodDef.returnsUnit) {
        Seq()
      } else {
        flattenParam("return", methodDef.outType, genFresh = true)
      }

      val bodyRes = transStatements(methodDef.body, None, methodDef)
      val bodies = for ((optReturn, cons, _) <- bodyRes) yield {
        val returnTerms = optReturn.getOrElse(Seq())
        val returnCons = returnParams.zip(returnTerms).map { case (p, t) =>
          Datalog.Eq(Datalog.Var(p.name), t)
        }
        Datalog.Body(cons ++ returnCons)
      }

    val qualifiedName = staticMethodPatName(classDef, methodDef)
    val pat = Datalog.Pattern(transVis(methodDef.vis), qualifiedName, argParams ++ returnParams, bodies)
    if (methodDef.isMain)
      pat.addHint(MagicSetHints.Main(argParams.map(_ => true) ++ returnParams.map(_ => false)))
         .addHint(ObjectHints.AllocationRoot)
         .addHint(ObjectHints.FieldRoot)
    else
      pat
  }

  private def transMethodWithSameQualifiedName(qualifiedName: String, pairs: Seq[(ClassDef, MethodDef)]): Datalog.Pattern = gensym.scoped {
    val allMethods = pairs.map(_._2)
    allMethods.foreach { m => gensym.register(m.vars.keys.map(_.raw) + "this") }

    val reprMethod = allMethods.head
    val thisParam = Datalog.Param("this", GP_URI)

    val clsParam = Datalog.Param(gensym.fresh("cls"), Datalog.TScalaString)
    val argParams = reprMethod.params.flatMap { case Param(name, typ) =>
      flattenParam(name.raw, typ, genFresh = false)
    }

    val repCls = pairs.map(_._1).head
    val isMonotoneAdd = repCls.isMonotoneClass && reprMethod.name.raw == AssignmentOp.AGG_ELEMENT.name.raw

    val returnParams = {
      if (isMonotoneAdd) {
        val resType = repCls.montoneTypes.get._2
        Seq(Datalog.Param(gensym.fresh("return"), transDataType(resType)))
      } else if (reprMethod.returnsUnit) {
        Seq()
      } else {
        flattenParam("return", reprMethod.outType, genFresh = true)
      }
    }

    val pairsSet = pairs.toSet // Remove duplicates
    val bodies = pairsSet.flatMap {
      case (c, m) =>
        val bodyRes = {
          if (isMonotoneAdd && !repCls.isMonotoneMapClass) {
            // Return the lifted value
            val body = m.body.dropRight(1)
            val liftExp = body.last match {
              case ExprStmt(expr) => expr
            }
            transStatements(body.dropRight(1) :+ ReturnStmt(liftExp), None, m)
          } else if (isMonotoneAdd && repCls.isMonotoneMapClass) {

            val outVar = Datalog.Var(gensym.fresh("out"))
            val coalArg = Datalog.Var(argParams.last.name)
            val clsName = repCls.montoneTypes.get._2 match {
              case TClass(ClassRef(name)) => name.raw
              case ty => throw new IllegalStateException(s"Can not coalesced none class type $ty")
            }
            val call = Datalog.Call(coalescedPatName(clsName), Seq(coalArg, outVar))
            Seq((Some(Seq(outVar)), Seq(call), None))
          } else {
            transStatements(m.body, None, m)
          }
        }
        for ((optReturn, cons, _) <- bodyRes) yield {
          val clsGuard = Datalog.Eq(Datalog.Var(clsParam.name), Datalog.StringConstant(c.name.raw))
          val returnTerms = optReturn.getOrElse(Seq())
          val returnCons = returnParams.zip(returnTerms).map { case (p, t) =>
            Datalog.Eq(Datalog.Var(p.name), t)
          }
          Datalog.Body(clsGuard +: (cons ++ returnCons))
        }
    }.toSeq

    Datalog.Pattern(transVis(reprMethod.vis), qualifiedName, clsParam +: thisParam +: (argParams ++ returnParams), bodies)
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

      for ((terms, cons) <- transExpression(recv)) yield {
        // reading the result value should perform an aggregation instead
        if (classDef.isMonotoneClass && targetName.raw == "result") {
          val Some((valType, resType)) = classDef.montoneTypes
          val aggVarType = transDataType(resType)

          val args = terms ++ valType.flatten.map(_ => Datalog.Var(gensym.fresh("_")))
          val addMethod = classDef.methods.filter(_.name.raw == AssignmentOp.AGG_ELEMENT.name.raw).head
          val methodName = methodPatName(addMethod)
          val dispatchName = dispatchPatName(methodName)
          val runtimeTypeVar = Datalog.Var(gensym.fresh("C"))
          val runtimeTypeComp = getURITyp(terms.head, runtimeTypeVar)
          val dispatchTypeVar = Datalog.Var(gensym.fresh("D"))
          val dispatchCall = Datalog.Call(dispatchName, Seq(runtimeTypeVar, dispatchTypeVar))

          val readAgg = Datalog.CustomAggregation(
            aggVarType,
            None,
            Scala(q"${Term.Name(classDef.name.raw)}.__aggregation__"),
            methodName,
            dispatchTypeVar +: args :+ Datalog.Var(gensym.fresh("_")),
            args.size + 1
          )
          val aggVar = Datalog.Var(gensym.fresh("agg"))
          val resultComp = Datalog.Computed(aggVar, readAgg)
            .addHint(MagicSetHints.IgnoreCall)
            .addHint(MagicSetHints.FixedAdornment(args.map(_ => true) :+ false))

          // TODO: Support tuples by using scala tuples

          val resultVars = flattenVars("result", resType, genFresh = true)
          val unpackCons = {
            resType match {
              case clazz@TClass(ClassRef(clsName)) =>
                val uncoalescedCall = Datalog.Call(uncoalescedPatName(clsName.raw), Seq(aggVar, resultVars.head._1))
                Seq(uncoalescedCall)
              case _ =>
                // we got a single value back from the aggregation
                Seq(Datalog.Eq(resultVars.head._1, aggVar))
            }
          }

          (resultVars.map(_._1), (cons :+ runtimeTypeComp :+ dispatchCall :+ resultComp) ++ unpackCons)
        } else if (classDef.isCaseClass) {
          val recvTerm = terms.head
          val fieldReadVars = flattenVars(targetName.raw, fieldDef.typ)
          val res = fieldReadVars.map { case (v, ty) =>
            val outVar = Datalog.Var(gensym.fresh(v.name))
            val readVarComp = getSIDField(recvTerm, v.name, outVar, ty)
            (Seq(outVar), Seq(readVarComp))
          }.unzip
          (res._1.flatten, res._2.flatten)
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
      val constructorDef = constrExpr.target.getOrElse(throw new IllegalArgumentException(s"Unresolved constructor $constrExpr"))

      if (classDef.isCaseClass) {
        // Construct an SID with all fields
        val primaryConstructor = classDef.constructors.filter(_.isPrimary).head
        val constructorFieldNames = primaryConstructor.params.map(_.name.raw)

        // translate and name or constructor arguments and fields
        val constructorArgs = primaryConstructor.params.zip(args).map { case (p, a) => (p.name.raw, p.typ, a) }
        val additionalFields = classDef.fields
          .filter(f => !constructorFieldNames.contains(f.name.raw))
          .map(f => (f.name.raw, f.typ, f.body.getOrElse(throw new RuntimeException("Uninitialized immutable field!"))))

        val argRes = (constructorArgs ++ additionalFields).map {
          case (fieldName, typ, exp) => transExpression(exp).map(r => (fieldName, typ, r))
        }

        if (argRes.isEmpty) {
          // Create empty case class
          val (constructedVar, constrComp) = createCaseClassObject(classDef.name.raw, classDef.typ, Seq())
          return Seq((Seq(constructedVar), Seq(constrComp)))
        }

        for (tups <- TupleOps.cartesianProduct(argRes)) yield {
          val (fieldNames, types, argTermsAndCons) = tups.unzip3
          val (argTerms, argCons) = argTermsAndCons.unzip
          val constructorArgs = (fieldNames, types, argTerms).zipped.toSeq
          val (constructedVar, constrComp) = createCaseClassObject(classDef.name.raw, classDef.typ, constructorArgs)
          (Seq(constructedVar), argCons.flatten ++ Seq(constrComp))
        }
      } else {
        val argRes = args.map(e => transExpression(e))
        val (constructedVar, constrComp) = if (classDef.isDefunAuxiliary) {
          // TODO: This is problematic for cases such as:
          //  Set(a, 1, 2) and Set(a, 3, 4), since they would be represented by the same object when defunctionalized
          //  Or worse: Is this the same set Set(1,2,3) and Set(2,3,4) or are these different classes ?
          val primaryConstructor = classDef.constructors.filter(_.isPrimary).head
          val constructorArgs = primaryConstructor.params.zip(argRes).flatMap {
            case (p, a) =>
              val (terms, _) = a.unzip
              terms.map(t => (p.name.raw, p.typ, t))
          }
          createCaseClassObject(classDef.name.raw, classDef.typ, constructorArgs)
        } else
          createObject(classDef.name.raw, classDef.typ)
        val constrName = constructorPatName(classRef.name.raw) + constructorDef.signature

        // create single call constraint when no arguments are passed
        if (argRes.isEmpty)
          return Seq((Seq(constructedVar), Seq(constrComp, Datalog.Call(constrName, Seq(constructedVar)))))

        for (tups <- TupleOps.cartesianProduct(argRes)) yield {
          val (argTerms, argCons) = tups.unzip
          (Seq(constructedVar), argCons.flatten ++ Seq(constrComp, Datalog.Call(constrName, constructedVar +: argTerms.flatten)))
        }
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

      val outVars = if (classDef.isMonotoneClass && fun.raw == AssignmentOp.AGG_ELEMENT.name.raw)
        flattenVars(gensym.fresh("methodCall"), classDef.montoneTypes.get._2).map(_._1)
      else
        flattenVars(gensym.fresh("methodCall"), methodDef.outType).map(_._1)

      val methodName = methodPatName(methodDef)
      val dispatchName = dispatchPatName(methodName)

      val Seq((terms, atoms)) = (for ((Seq(term), cons) <- transExpression(recv)) yield {
        val runtimeTypeVar = Datalog.Var(gensym.fresh("C"))
        val runtimeTypeComp = getURITyp(term, runtimeTypeVar)
        val dispatchTypeVar = Datalog.Var(gensym.fresh("D"))
        val dispatchCall = Datalog.Call(dispatchName, Seq(runtimeTypeVar, dispatchTypeVar))

        val isMonoMapKeys = classDef.isMonotoneMapClass && methodDef.name.raw == "keys"
        val isMonoMapGet = classDef.isMonotoneMapClass && methodDef.name.raw == "get"

        if (isMonoMapKeys) {
          val addMethod = classDef.methods.filter(_.name.raw == AssignmentOp.AGG_ELEMENT.name.raw).head
          val addMethodName = methodPatName(addMethod)
          val dispatchTypeVarAdd = Datalog.Var(gensym.fresh("D"))
          val dispatchCallAdd = Datalog.Call(dispatchPatName(addMethodName), Seq(runtimeTypeVar, dispatchTypeVarAdd))
          val queryCall = Datalog.Call(addMethodName, dispatchTypeVarAdd +: term +: outVars :+ Datalog.Var(gensym.fresh("_")) :+ Datalog.Var(gensym.fresh("_")))
            .addHint(MagicSetHints.IgnoreCall)
          return Seq((outVars, cons :+ runtimeTypeComp :+ dispatchCallAdd :+ queryCall))
        } else if (argRes.isEmpty) {
          val methodCall = Datalog.Call(methodName, dispatchTypeVar +: term +: outVars)
          val call = Seq(runtimeTypeComp, dispatchCall, methodCall)

          return if (isFix) {
            if (methodDef.returnsUnit)
              Seq((Seq(), Seq()), (outVars, cons ++ call))
            else
              throw new RuntimeException("Fixpoint iterations for none unit method are currently not supported.")
          } else {
            Seq((outVars, cons ++ call))
          }
        }

        for (tups <- TupleOps.cartesianProduct(argRes)) yield {
          val (argTerms, argCons) = tups.unzip

          if (isMonoMapGet) {
            val Some((_, resType)) = classDef.montoneTypes

            val addMethod = classDef.methods.filter(_.name.raw == AssignmentOp.AGG_ELEMENT.name.raw).head
            val addMethodName = methodPatName(addMethod)
            val dispatchTypeVarAdd = Datalog.Var(gensym.fresh("D"))
            val dispatchCallAdd = Datalog.Call(dispatchPatName(addMethodName), Seq(runtimeTypeVar, dispatchTypeVarAdd))

            val readAgg = Datalog.CustomAggregation(
              transDataType(resType),
              None,
              Scala(q"${Term.Name(resType.asInstanceOf[TClass].ref.name.raw)}.__aggregation__"),
              addMethodName,
              dispatchTypeVarAdd +: term +: argTerms.flatten :+ Datalog.Var(gensym.fresh("_")) :+ Datalog.Var(gensym.fresh("_")),
              argTerms.flatten.size + 3
            )

            val aggVar = Datalog.Var(gensym.fresh("agg"))
            val aggComp = Datalog.Computed(aggVar, readAgg).addHint(MagicSetHints.IgnoreCall)

            val resultVar = Datalog.Var(gensym.fresh("result"))
            val unpackCons = resType match {
              case TClass(ClassRef(clsName)) =>
                Seq(Datalog.Call(uncoalescedPatName(clsName.raw), Seq(aggVar, resultVar)))
              case _ =>
                Seq() // We never get here. We already fail before.
            }
            val aggCons = Seq(runtimeTypeComp, dispatchCallAdd, aggComp)
            (Seq(resultVar), (cons ++ argCons.flatten ++ aggCons ++ unpackCons))
          } else {
            val methodCall = Datalog.Call(methodName, dispatchTypeVar +: term +: (argTerms.flatten ++ outVars))
            val call = Seq(runtimeTypeComp, dispatchCall, methodCall)
            (outVars, cons ++ argCons.flatten ++ call)
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
      } else {
        Seq((terms, atoms))
      }

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
      } else {
        //exps.flatMap(transExpression)
        // Each body must contain all constraint to correctly thread the allocation counter
        val (tups, cons) = exps.flatMap(transExpression).unzip
        tups.map(t => t -> cons.flatten)
      }

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
        case td@TClass(ClassRef(clsName)) =>
          val pat = generatePattern(recv, aggregatePatName(opClass.name.raw, opMethod.raw))

          val outParamSize = aggType.flatten.size
          val leftParams = pat.params.slice(0, pat.params.size - outParamSize + aggIndex)
          val rightParams = pat.params.slice(leftParams.size + 1, pat.params.size)
          val oldOutName = pat.params(leftParams.size).name
          val newOutName = gensym.fresh("out")
          val newOutParam = Datalog.Param(newOutName, transDataType(td))
          val coalesceCon = Datalog.Call(coalescedPatName(clsName.raw), Seq(Datalog.Var(oldOutName), Datalog.Var(newOutName)))
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
          case TClass(ClassRef(clsName)) =>
            val foldVarUncoalesced = Datalog.Var(gensym.fresh("fold"))
            val uncoalesce = Datalog.Call(uncoalescedPatName(clsName.raw), Seq(foldVar, foldVarUncoalesced))
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
        // include all constraint to correctly thread the allocation counter
        // Note lhsCons should be all equal no matter which element. The same holds for rhsCons.
        transExpression(left) ++ transExpression(right)
        /*val (lhsTups, lhsCons) = transExpression(left).unzip
        val (rhsTups, rhsCons) = transExpression(right).unzip
        // Add all lhsCons to each rhs constraint. Note: Use the correct order !
        val lhsRes = lhsTups.zip(lhsCons).map { case (t, c) => t -> Seq(rhsCons.head, c).flatten }
        val rhsRes = rhsTups.zip(rhsCons).map { case (t, c) => t -> Seq(c, lhsCons.head).flatten }
        lhsRes ++ rhsRes*/

    /*case BaseApplyInfixExpr(left, op, right)
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
          Datalog.Evaluation(
            Seq(leftTerm -> transType(left.typ.get), rightTerm -> transType(right.typ.get)),
            transType(resType),
            Scala(funCode)
          )
        )
        (Seq(evalOut), leftCons ++ rightCons ++ Seq(evalConstraint))
      }

    case SetFromEdb(edbName, tty) =>
      val outVars = tty.flatten.map(_ => Datalog.Var(gensym.fresh("edb")))
      val edbCall = Datalog.ExtensionalCall(edbName.raw, outVars)
      Seq((outVars, Seq(edbCall)))
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