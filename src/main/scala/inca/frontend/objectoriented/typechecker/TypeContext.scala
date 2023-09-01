package inca.frontend.objectoriented.typechecker;

import inca.compiler.SourceLocation
import inca.frontend.objectoriented.core._
import jdk.vm.ci.meta.Assumptions.NoFinalizableSubclass

import scala.collection.{AbstractSet, SortedSet, mutable}
import scala.collection.immutable.MultiDict
import scala.reflect.ClassTag


trait TypeContext extends TypeIO {
  private var modules: Map[Name, Module] = Map()
  private var classDefs: MultiDict[Name, (Module, ClassDef)] = MultiDict()
  private var vars: Map[Name, (VarReadExpr.Target, Type, Boolean)] = Map()
  private var genericParams: Map[(Name,Name),GenericParamDef] = Map()   // maps name of generic Param and the name Class that declares it to the GenericParamDef

  def scopedTypeContext[T](f: => T): T = {
    val v = vars
    val c = classDefs
    val params = genericParams
    val t = f   // here f executed
    vars = v    // reset
    genericParams = params
    classDefs = c
    t
  }

  def subtype(ty1: Type, ty2: Type, className: Name): Boolean = {
      (convertTName(ty1), convertTName(ty2)) match {
      case (_, TAny) => true
      case (TNull, TNull) => true
      case (TNull, TClass(_)) => true
      case (TClass(ref1), TClass(ref2)) if ref1 == ref2 => true
      case (TClass(ref1), TClass(_)) =>
        val parents = classDefs.get(ref1.name).flatMap { case (_, c) => c.parentClassRefs }
        //val parents = ref1.target.getOrElse(throw new IllegalArgumentException(s"unresolved $ty1")).parentClassRefs //lookupClassRef(ref1).get.parentClassRefs
        parents.exists { parent => if (parent.target.isDefined) subtype(parent.classDef.get.typ, ty2, className) else false }
      case (TTuple(tys1), TTuple(tys2)) if tys1.size == tys2.size =>
        tys1.zip(tys2).forall(tt => subtype(tt._1, tt._2, className))
      case (TSet(ty1), TSet(ty2)) => subtype(ty1, ty2, className)
      case (TName(n1), TName(n2)) =>
        if (lookupGenericParam(n1, className, suppressError = true).isDefined)
          n1 == n2
        else
          false
      case (TNull, TName(_)) => true
      case _ => false
    }
  }

  def join(ty1: Type, ty2: Type, className: Name): Type = (ty1, ty2) match {
    case (_, _) if ty1 == ty2 =>
      ty1
    case (TTuple(tys1), TTuple(tys2)) if tys1.size == tys2.size =>
      TTuple(tys1.zip(tys2).map(tt => join(tt._1, tt._2, className)))
    case (TSet(ty1), TSet(ty2)) =>
      join(ty1, ty2, className)
    case (_, TNull) => ty1
    case (TNull, _) => ty2
    case (TClass(TName(name1)), TClass(TName(name2))) =>
      if (subtype(ty1, ty2, className))
        ty2
      else if (subtype(ty2, ty1, className))
        ty1
      else {
        // find a common supertype
        val parentRefs = classDefs.get(name1).flatMap { case (m, c) => c.parentClassRefs.map((m, _)) }
        // get all classDefs from the ClassRefs
        val parentTypes = parentRefs.flatMap { case (m, p) =>
          classDefs.get(p.name).filter {
            case (m2, _) if m == m2 => true
            case _ => false
          }.map(_._2.typ)
        }
        // use the resolved type to find the supertype
        parentTypes.find(pTy => subtype(ty2, pTy, className)).getOrElse(TAny)
      }
    case (_, _) =>
      if (subtype(ty1, ty2, className))
        ty2
      else if (subtype(ty2, ty1, className))
        ty1
      else
        TAny
  }

  def join(types: Seq[Type], className: Name): Type = types.reduce[Type] {
    case (ty1, ty2) => join(ty1, ty2, className)
  }

  def bindModule(module: Module): Unit = {
    val name = module.name
    modules.get(name) foreach { bound =>
      error(s"Found multiple modules with same name $name", name, bound.name)
    }
    modules += (name -> module)
  }

  def lookupModule(name: Name): Option[Module] = modules.get(name) match {
    case Some(module) => Some(module)
    case None =>
      error(s"Unknown module $name", name)
      None
  }

  def bindClass(clazz: ClassDef, module: Module): Unit = {
    classDefs += clazz.name -> (module, clazz)
  }

  def lookupClass(name: Name, suppressError: Boolean = false): Option[ClassDef] = classDefs.get(name) match {
    case set if set.size == 1 =>
      Some(set.head._2)
    case set if set.size >= 2 =>
      val modules = set.toSeq.map(_._1)
      val modulesStr = modules.map(_.name).mkString(", ")
      if (!suppressError)
        error(s"Ambiguous call to $name, found definitions in $modulesStr", (name +: modules): _*)
      None
    case _ =>
      if (!suppressError)
        error(s"Undefined class $name", name)
      None
  }


  def bindVar(name: Name, decl: VarReadExpr.Target, ty: Type, immutable: Boolean): Unit = {
    vars.get(name) foreach { case (previousDecl, _, _) =>
      error(s"Variable $name shadows previously defined variable $previousDecl", name, previousDecl)
    }
    vars += (name -> (decl, ty, immutable))
  }

  def lookupVar(name: Name, suppressError: Boolean = false): Option[(VarReadExpr.Target, Type, Boolean)] = vars.get(name) match {
    case Some(entry) => Some(entry)
    case None =>
      if (!suppressError)
        error(s"Unbound variable $name", name)
      None
  }

  private def collect[C <: ClassContent : ClassTag](clazz: Option[ClassDef], f: C => Boolean): Seq[(ClassDef, C)] = {
    if (clazz.isEmpty) {
      val clsName = implicitly[ClassTag[C]].runtimeClass.getSimpleName
      error(s"Undefined class in $clsName lookup!")
      Seq()
    } else {
      val content = clazz.get.content.flatMap {
        case c: C if f(c) => Some((clazz.get, c))
        case _ => None
      }
      val parentContent: Seq[(ClassDef, C)] = clazz.get.parentClassRefs.flatMap { ref =>
        collect(ref.classDef, f)
      }
      parentContent ++ content
    }
  }


  /** "replaces" a generic parameter with correct type argument for a generic method
   *
   * @param methodDef [[MethodDef]] that defines scope
   * @param tyArgs type arguments for the generic method (possibly empty if e.g. method is not generic)
   * @param ty [[Type]] possible [[TName]] then maybe different [[Type]] returned
   * @return [[Type]] of ty in scope of given [[MethodDef]] methodDef
   */
  def getTypeForGenericParamMethod(methodDef: MethodDef, tyArgs: Seq[Type], ty: Type): Type = ty match{
    case tname@TName(n) =>
      val filtered = methodDef.genericTypeParams.filter(param => param.name == n)
      filtered.headOption match {
        case Some(param) =>
          val index = methodDef.genericTypeParams.indexOf(param)
          tyArgs.lift(index).getOrElse(tname)
        case None => tname
      }
    case _ => ty
  }

  /** "replaces" a generic parameter with correct type argument for a generic class
   *
   * @param classDef [[ClassDef]] that defines scope
   * @param tyArgs type arguments for the generic method (possibly empty if e.g. class is not generic)
   * @param ty [[Type]] possible [[TName]] then maybe different [[Type]] returned
   * @return [[Type]] of ty in scope of given [[ClassDef]] classDef
   */
  def getTypeForGenericParamClass(classDef: ClassDef, tyArgs: Seq[Type], ty: Type): Type = ty match {
    case tname@TName(n) =>
      val filtered = classDef.genericTypeParams.filter(param => param.name == n)
      filtered.headOption match {
        case Some(param) =>
          val index = classDef.genericTypeParams.indexOf(param)
          tyArgs.lift(index).getOrElse(tname)
        case None => tname
      }
    case _ => ty
  }


  /** replaces all occurrences of typToReplace in classDef with newTyp and returns a new [[ClassDef]] */
  def substClassDef(classDef: ClassDef, typToReplace: TName, newTyp: Type): ClassDef = ???
  // would probably not work to substitute when created (new C[...]) and then look up substituted ClassDef when needed,
  // since e.g. a fieldReadExpr (like c1.a) looks up the class by name (here C)
  // so that no distinction between original class and substituted class would be possible (?)



  /** "replaces" a generic parameter with correct type argument in case of inheritance
   *
   * @param clazz        [[ClassDef]] that defines scope
   * @param ty           [[Type]] possible [[TName]] then maybe different [[Type]] returned
   * @param overrideFlag set flag to true if ty=TName(n) then n not in the generic parameters of clazz (used for inheritance)
   * @return [[Type]] that given generic parameter ty represents
   */
  def getTypeForGenericParamInheritance(clazz: ClassDef, ty: Type, overrideFlag: Boolean = true): Type = ty match {
    case TName(n) =>
      val newOutTypeSeq: Seq[Option[Type]] = clazz.parentClassRefs.map { tName =>
        lookupClass(tName.name) match {
          case Some(classDef) =>
            val filtered: Seq[GenericParamDef] = classDef.genericTypeParams.filter { param =>
              (param.name == n) && (!overrideFlag || !clazz.genericTypeParams.contains(param))
            }
            val index = classDef.genericTypeParams.indexOf(filtered.headOption.getOrElse(None))
            if (index > -1) // then a type was found
              Some(tName.tyArgs(index))
            else {
              val recRes = getTypeForGenericParamInheritance(classDef, ty) // find generic types of super class of super class
              if (recRes != ty)
              // try to replace found Type again so that its finally replaced by type for parameter known to given clazz
                Some(getTypeForGenericParamInheritance(clazz, recRes, overrideFlag = false))
              else
                Some(recRes)
              // None
            }
          case _ => None
        }
      }
      newOutTypeSeq.find(opt => opt.isDefined) match {
        case Some(value) => value match {
          case Some(value2) =>
            value2
          case None => ty
        }
        case None => ty
      }
    case _ => ty
  }


  /** substitutes generic parameter occurrences (that are checked coming from expressions) in given [[ClassContent]] c
   * with type argument given to reference to super class of given [[ClassDef]] clazz
   *
   * @return [[ClassContent]] of same class as given c with replaced type parameters
   */
  private def substGenericParamForInheritance(clazz: ClassDef, c: ClassContent): ClassContent = c match {
    case MethodDef(annos, vis, name, genericTypeParams, params, outType, body) =>
      val newOutType: Type = getTypeForGenericParamInheritance(clazz,outType)
      val newParams: Seq[Param] = params.map{ param =>
        val newParamType = getTypeForGenericParamInheritance(clazz, param.typ)
        newParamType.tyArgs = param.typ.tyArgs
        Param(param.name, newParamType)
      }
      MethodDef(annos, vis, name, genericTypeParams, newParams, newOutType, body)

    case FieldDef(annos, vis, name, typ, body, immutable) =>
      val newType: Type = getTypeForGenericParamInheritance(clazz, typ)
      newType.tyArgs = typ.tyArgs
      FieldDef(annos, vis, name, newType, body, immutable)

    case ConstructorDef(annos,vis,params,body) =>
      val newParams: Seq[Param] = params.map { param =>
        val newParamType = getTypeForGenericParamInheritance(clazz, param.typ)
        newParamType.tyArgs = param.typ.tyArgs
        Param(param.name, newParamType)
      }
      ConstructorDef(annos,vis,newParams,body)

    case other => other
  }


  def lookupField(clazz: Option[ClassDef], name: Name): Option[(ClassDef, FieldDef)] = {
    val clsName = if (clazz.isDefined) clazz.get.name.raw else ""
    var allFields = collect[FieldDef](clazz, f => f.name == name)

    // Special case for monotone classes to satisfy the typechecker
    if (clazz.isDefined && clazz.get.isMonotoneClass) {
      val Some((_, resType)) = clazz.get.montoneTypes
      val resultField = FieldDef(Seq(), None, Name("result"), resType, None, immutable = true)
      allFields :+= (clazz.get -> resultField)
    }

    if (allFields.isEmpty) {
      error(s"Undefined field $clsName.$name", name)
      None
    } else if (allFields.size > 1) {
      val (parentClass, _) = allFields.head
      error(s"Field $name shadows previously defined field in class ${parentClass.name}", name)
      None
    } else {
      allFields.headOption match {
        case Some((classDef: ClassDef, fieldDef: FieldDef)) =>
          Some(classDef,substGenericParamForInheritance(clazz.getOrElse(classDef),fieldDef).asInstanceOf[FieldDef])
        case None => None
      }
    }
  }

  /** returns Class and Content for which all args match the corresponding params */
  private def areArgParamsSubtypes(classDefAndContent: Seq[(ClassDef,ClassContent)], args: Seq[Type],
                                   subClass: Option[ClassDef] = None): Seq[(ClassDef,ClassContent)] = {
    val zipped: Seq[((ClassDef, ClassContent), Seq[(Type, Param)])] = classDefAndContent.map(tup =>
      (tup, args.zip(tup._2 match {
        case MethodDef(annos, vis, name, genericTypeParams, params, outType, body) => params
        case ConstructorDef(annos, vis, params, body) => params
        case field@_ => throw new IllegalArgumentException(s"tried to check params of field $field")
      }))
    )
    zipped.filter { tup =>
      tup._2.forall { case (arg, param) => subtype(arg, param.typ, subClass.getOrElse(tup._1._1).name) }
    }.map(tup => tup._1)
  }


  def lookupMethodCandidates(clazz: Option[ClassDef], args: Seq[Type], name: Name): Seq[(ClassDef, MethodDef)] = {
    val collected = collect[MethodDef](clazz, m => {
      m.name == name && m.params.size == args.size
    })
    val substituted: Seq[(ClassDef, MethodDef)] = collected.map(tup => (tup._1,substGenericParamForInheritance(clazz.getOrElse(tup._1),tup._2).asInstanceOf[MethodDef]))

    areArgParamsSubtypes(substituted, args).asInstanceOf[Seq[(ClassDef,MethodDef)]]
  }

  // TODO use generic Types in searching method (?)
  def lookupMethod(clazz: Option[ClassDef], args: Seq[Type], name: Name): Option[(ClassDef, MethodDef)] = {
    val clsName = if (clazz.isDefined) clazz.get.name.raw else ""
    val allMethods = lookupMethodCandidates(clazz, args, name)

    if (allMethods.isEmpty) {
      error(s"Undefined method $clsName.$name(${args.mkString(",")})", name)
      None
    } else {
      // always choose the method lowest in the class hierarchy
      allMethods.lastOption
    }
  }

  // for a superclass the given classDef clazz is the superclass (in which a constructor is searched)
  // but the given types of the arguments are the types according to the scope of the subclass
  // -> substitution works but since the class that defines type parameters isn`t present subtyping failed
  // -> solution: give ClassDef of subclass that attempts to call constructor of superclass
  def lookupConstructorCandidates(clazz: Option[ClassDef], tyArgs: Seq[Type], args: Seq[Type], subClass: Option[ClassDef] = None): Seq[(ClassDef, ConstructorDef)] = {
    val collected = collect[ConstructorDef](clazz, c => {
      c.params.size == args.size
    })

    val substituted: Seq[(ClassDef, ConstructorDef)] = collected.map(tup => (tup._1, substGenericParamForInheritance(clazz.getOrElse(tup._1), tup._2).asInstanceOf[ConstructorDef]))

    val substituted2: Seq[(ClassDef, ConstructorDef)] = substituted.map { tup =>
      val ConstructorDef(annos,vis,params,body) = tup._2
      val newParams = params.map{ p =>
        val pTyp: Type = clazz match {
          case Some(classDef) => getTypeForGenericParamClass(classDef,tyArgs,p.typ)
          case None => TAny // error already in collect
        }
        Param(p.name, pTyp)
      }
      (tup._1,ConstructorDef(annos,vis,newParams,body))
    }

    val newArgs: Seq[Type] = {
      if (clazz.isDefined)
        args.map(getTypeForGenericParamInheritance(clazz.get, _))
      else
        args
    }

    areArgParamsSubtypes(substituted2, newArgs, subClass).asInstanceOf[Seq[(ClassDef,ConstructorDef)]]

  }

  def lookupConstructor(clazz: Option[ClassDef], tyArgs: Seq[Type], args: Seq[Type], location: SourceLocation, subClass: Option[ClassDef] = None): Option[(ClassDef, ConstructorDef)] = {
    val clsName = if (clazz.isDefined) clazz.get.name.raw else ""
    val allConstructor = lookupConstructorCandidates(clazz, tyArgs, args, subClass)
    val ambiguousConstructors = allConstructor.groupBy(_._1).filter(_._2.size > 1)

    if (allConstructor.isEmpty) {
      error(s"No matching constructor found for class $clsName: this(${args.mkString(",")})", location)
      None
    } else if (ambiguousConstructors.nonEmpty) {
      ambiguousConstructors.foreach {
        case (cls, _) => error(s"Ambiguous constructor for class ${cls.name.raw}: this(${args.mkString(",")})", location)
      }
      None
    } else {
      // always choose the constructor lowest in the class hierarchy
      Some(allConstructor.last)
    }
  }

  def lookupGenericParam(paramName: Name, className: Name, suppressError: Boolean = false): Option[GenericParamDef] = genericParams.get((paramName,className)) match {
    case Some(entry) => Some(entry)
    case None =>
      if (!suppressError)
        error(s"Unbound generic parameter $paramName", paramName)
      None
  }

  def bindGenericParam(name: Name, genericParam: GenericParamDef, className: Name, suppressError: Boolean = false): Unit = {
    val shadowedClass = lookupClass(name, suppressError = true) match {
      case cls@Some(_) =>
        if (!suppressError)
          error(s"Generic parameter shadows previously defined class with same name.", name)
        cls
      case None =>
        None
    }
    if (shadowedClass.isEmpty) {
      genericParams.get((name,className)) match {
        case Some(value) =>
          if (!suppressError)
            error(s"Generic parameter shadows previously defined parameter.", name)
        case None =>
          genericParams += (name,className) -> genericParam
      }
    }
  }

  def convertTName(typ: Type): Type = typ match {
    case tName@TName(name) =>
      val cls = lookupClass(name, suppressError = true) match {
        case Some(classDef) =>
          val cls = classDef.typ
          cls.tyArgs = tName.tyArgs
          Some(cls)
        case None => None
      }
      cls.getOrElse(tName)
    case _ => typ
  }


}
