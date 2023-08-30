package inca.frontend.objectoriented.typechecker;

import inca.compiler.SourceLocation
import inca.frontend.objectoriented.core._

import scala.collection.{AbstractSet, SortedSet, mutable}
import scala.collection.immutable.MultiDict
import scala.reflect.ClassTag

/* TODO
    Methode zum registrieren von Typparametern (ParamDefs)
    Lookup (auf ParamTypes, zurückgeben ParamDef)
    Hilfsfunktionen: subtype
 */

trait TypeContext extends TypeIO {
  private var modules: Map[Name, Module] = Map()
  private var classDefs: MultiDict[Name, (Module, ClassDef)] = MultiDict()
  private var vars: Map[Name, (VarReadExpr.Target, Type, Boolean)] = Map()
  private var genericParams: Map[Name, GenericParamDef] = Map()

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

  def subtype(ty1: Type, ty2: Type): Boolean = {
    //println(ty1,ty2)
    (convertTName(ty1), convertTName(ty2)) match {
    case (_, TAny) => true
    case (TNull, TNull) => true
    case (TNull, TClass(_)) => true
    case (TClass(ref1), TClass(ref2)) if ref1 == ref2 => true
    case (TClass(ref1), TClass(_)) =>
      val parents = classDefs.get(ref1.name).flatMap { case (_, c) => c.parentClassRefs }
      //val parents = ref1.target.getOrElse(throw new IllegalArgumentException(s"unresolved $ty1")).parentClassRefs //lookupClassRef(ref1).get.parentClassRefs
      parents.exists { parent => if (parent.target.isDefined) subtype(parent.classDef.get.typ, ty2) else false }
    case (TTuple(tys1), TTuple(tys2)) if tys1.size == tys2.size =>
      tys1.zip(tys2).forall(tt => subtype(tt._1, tt._2))
    case (TSet(ty1), TSet(ty2)) => subtype(ty1, ty2)
    case (TName(n1), TName(n2)) if (lookupGenericParam(n1, suppressError = true).isDefined) => n1 == n2
//    case (TName(n1), TName(n2)) if (lookupClass(n1, suppressError = true).isDefined) => n1 == n2 // TODO ???
//    case (TClass(tname: TName), TName(n)) => tname.name == n // TODO ???
    case (TNull, TName(_)) => true
    case _ => false
  }
  }

  def join(ty1: Type, ty2: Type): Type = (ty1, ty2) match {
    case (_, _) if ty1 == ty2 =>
      ty1
    case (TTuple(tys1), TTuple(tys2)) if tys1.size == tys2.size =>
      TTuple(tys1.zip(tys2).map(tt => join(tt._1, tt._2)))
    case (TSet(ty1), TSet(ty2)) =>
      join(ty1, ty2)
    case (_, TNull) => ty1
    case (TNull, _) => ty2
    case (TClass(TName(name1)), TClass(TName(name2))) =>  //TODO genericTypes
      if (subtype(ty1, ty2))
        ty2
      else if (subtype(ty2, ty1))
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
        parentTypes.find(pTy => subtype(ty2, pTy)).getOrElse(TAny)
      }
    case (_, _) =>
      if (subtype(ty1, ty2))
        ty2
      else if (subtype(ty2, ty1))
        ty1
      else
        TAny
  }

  def join(types: Seq[Type]): Type = types.reduce[Type] {
    case (ty1, ty2) => join(ty1, ty2)
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
        val collected = collect(ref.classDef, f)
        //println("collected: ", collected)
        collected
      }
      parentContent ++ content
    }
  }

  // TODO find generic types of super class of super class
  // TODO maybe include subst of current class` typeArgs here or in a separate subst method (possible?)
  //  so that it can be used e.g. for MethodCallExpr
  private def substGenericParam(clazz: ClassDef, ty: Type, prevClazz: Option[ClassDef] = None): Type = ty match {
    case TName(n) =>
      val newOutTypeSeq: Seq[Option[Type]] = clazz.parentClassRefs.map { tName =>
        lookupClass(tName.name) match {
          case Some(classDef) =>
            val filtered: Seq[GenericParamDef] = classDef.genericTypeParams.filter(param => param.name == n)
            val index = classDef.genericTypeParams.indexOf(filtered.headOption.getOrElse(None))
            if (index > -1)
              Some(tName.tyArgs(index))
            else {
              //Some(substGenericParam(classDef,ty,Some(clazz)))
              None
            }
          case _ => None
        }
      }
      newOutTypeSeq.find(opt => opt.isDefined) match {
        case Some(value) => value match {
          case Some(value2) => value2
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
      val newOutType: Type = substGenericParam(clazz,outType)
      val newParams: Seq[Param] = params.map{ param =>    // TODO necessary ?
        val newParamType = substGenericParam(clazz, param.typ)
        newParamType.tyArgs = param.typ.tyArgs
        Param(param.name, newParamType)
      }
      MethodDef(annos, vis, name, genericTypeParams, newParams, newOutType, body)

    case FieldDef(annos, vis, name, typ, body, immutable) =>
      val newType: Type = substGenericParam(clazz, typ)
      newType.tyArgs = typ.tyArgs
      FieldDef(annos, vis, name, newType, body, immutable)

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



  def lookupMethodCandidates(clazz: Option[ClassDef], args: Seq[Type], name: Name): Seq[(ClassDef, MethodDef)] = {
    val collected = collect[MethodDef](clazz, m => {
      m.name == name && m.params.size == args.size
    })

    // TODO refactor and test whether helpful
    val substituted: Seq[(ClassDef, MethodDef)] = collected.map(tup => (tup._1,substGenericParamForInheritance(clazz.getOrElse(tup._1),tup._2).asInstanceOf[MethodDef]))
    // val argParams: Seq[(Type, (ClassDef, MethodDef))] = args.zip(substituted)

    val zipped: Seq[((ClassDef,MethodDef), Seq[(Type,Param)])] = substituted.indices.map(i =>
     (substituted(i), args.zip(substituted(i)._2.params))
    )
    zipped.filter{ tup =>
      tup._2.forall { case (arg, param) => subtype(arg, param.typ) }
    }.map(tup => tup._1)

    //substituted.zip(argParams).filter{case (sub,argParam) => subtype(argParam, p.typ)}.map(tup => tup._1)
    //collected.map(classWithMethod => args.zip(substMethod(classWithMethod._2).params)).filter{ case (t1, p) => subtype(t1, p.typ) }
//    args.zip(m.params).forall
  }

  // TODO use generic Types in searching method
  def lookupMethod(clazz: Option[ClassDef], args: Seq[Type], name: Name): Option[(ClassDef, MethodDef)] = {
    val clsName = if (clazz.isDefined) clazz.get.name.raw else ""
    val allMethods = lookupMethodCandidates(clazz, args, name)

    if (allMethods.isEmpty) {
      error(s"Undefined method $clsName.$name(${args.mkString(",")})", name)
      None
    } else {
      // always choose the method lowest in the class hierarchy
      //println(allMethods.lastOption)
      allMethods.lastOption
    }
  }

  def lookupConstructorCandidates(clazz: Option[ClassDef], tyArgs: Seq[Type], args: Seq[Type]): Seq[(ClassDef, ConstructorDef)] = {
    collect[ConstructorDef](clazz, c => {
      c.params.size == args.size && args.zip(c.params).forall { case (t1, p) =>

        //println(p.typ)

        val pTyp: Type = clazz match {
          case Some(classDef) => p.typ match {
            case tname@TName(n) =>
              val filtered = classDef.genericTypeParams.filter(param => param.name == n)
              filtered.headOption match {
                case Some(param) =>
                  val index = classDef.genericTypeParams.indexOf(param)
                  tyArgs.lift(index).getOrElse(tname)
                case None => tname
              }
            case ty => ty
          }
          case None =>
            TAny // error already in collect TODO ???
        }

        //println(pTyp)

        subtype(t1, pTyp) }
    })
  }

  def lookupConstructor(clazz: Option[ClassDef], tyArgs: Seq[Type], args: Seq[Type], location: SourceLocation): Option[(ClassDef, ConstructorDef)] = {
    val clsName = if (clazz.isDefined) clazz.get.name.raw else ""
    val allConstructor = lookupConstructorCandidates(clazz, tyArgs, args)
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

  def lookupGenericParam(name: Name, suppressError: Boolean = false): Option[GenericParamDef] = genericParams.get(name) match {
    case Some(entry) => Some(entry)
    case None =>
      if (!suppressError)
        error(s"Unbound generic parameter $name", name)
      None
  }

  def bindGenericParam(name: Name, typ: GenericParamDef, suppressError: Boolean = false): Unit = {
    val shadowedClass = lookupClass(name, suppressError = true) match {
      case cls@Some(_) =>
        if (!suppressError)
          error(s"Generic parameter shadows previously defined class with same name.", name)
        cls
      case None =>
        None
    }
    if (shadowedClass.isEmpty) {
      genericParams.get(name) match {
        case Some(value) =>
          if (!suppressError)
            error(s"Generic parameter shadows previously defined parameter.", name)
        case None =>
          genericParams += name -> typ
      }
    }
  }

  def convertTName(typ: Type): Type = typ match {
    case tName@TName(name) =>
      val cls = lookupClass(name, suppressError = true) match {
        case Some(classDef) =>
          //val cls = TClass(tName)
          val cls = classDef.typ
          cls.tyArgs = tName.tyArgs
          Some(cls)
        case None => None
      }
      cls.getOrElse(tName)
    case _ => typ
  }


}
