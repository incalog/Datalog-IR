package inca.frontend.objectoriented.typechecker;

import inca.compiler.SourceLocation
import inca.frontend.objectoriented.core._

import scala.collection.immutable.MultiDict
import scala.reflect.ClassTag

/* TODO
  Methode zum registrieren von Typparametern (ParamDefs)
  Lookup (auf ParamTypes, zurückgeben ParamDef)
  scopedTypeContext anpassen
  Hilfsfunktionen: subtype
 */

trait TypeContext extends TypeIO {
  private var modules: Map[Name, Module] = Map()
  private var classDefs: MultiDict[Name, (Module, ClassDef)] = MultiDict()
  private var vars: Map[Name, (VarReadExpr.Target, Type, Boolean)] = Map()

  def scopedTypeContext[T](f: => T): T = {
    // TODO anpassen: speichern & zurücksetzen
    val v = vars
    val c = classDefs
    val t = f   // hier f ausgeführt
    vars = v    // zurücksetzen
    classDefs = c
    t
  }

  def subtype(ty1: Type, ty2: Type): Boolean = (ty1, ty2) match {
    case (_, TAny) => true
    case (TNull, TNull) => true
    case (TNull, TClass(_)) => true
    case (TClass(ref1), TClass(ref2)) if ref1 == ref2 => true
    case (TClass(ref1), TClass(_)) =>
      val parents = classDefs.get(ref1.name).flatMap { case (_, c) => c.parentClassRefs }
      //val parents = ref1.target.getOrElse(throw new IllegalArgumentException(s"unresolved $ty1")).parentClassRefs //lookupClassRef(ref1).get.parentClassRefs
      parents.exists { parent => if (parent.target.isDefined) subtype(parent.target.get.typ, ty2) else false }
    case (TTuple(tys1), TTuple(tys2)) if tys1.size == tys2.size =>
      tys1.zip(tys2).forall(tt => subtype(tt._1, tt._2))
    case (TSet(ty1), TSet(ty2)) => subtype(ty1, ty2)
    case _ => false
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
    case (TClass(ClassRef(name1,_)), TClass(ClassRef(name2,_))) =>  //TODO genericTypes
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

  def lookupClass(name: Name): Option[ClassDef] = classDefs.get(name) match {
    case set if set.size == 1 =>
      Some(set.head._2)
    case set if set.size >= 2 =>
      val modules = set.toSeq.map(_._1)
      val modulesStr = modules.map(_.name).mkString(", ")
      error(s"Ambiguous call to $name, found definitions in $modulesStr", (name +: modules): _*)
      None
    case _ =>
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
      val parentContent = clazz.get.parentClassRefs.flatMap(ref => collect(ref.target, f))
      parentContent ++ content
    }
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
      allFields.headOption
    }
  }

  def lookupMethodCandidates(clazz: Option[ClassDef], args: Seq[Type], name: Name): Seq[(ClassDef, MethodDef)] = {
    collect[MethodDef](clazz, m => {
      m.name == name && m.params.size == args.size && args.zip(m.params).forall { case (t1, p) => subtype(t1, p.typ) }
    })
  }

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

  def lookupConstructorCandidates(clazz: Option[ClassDef], args: Seq[Type]): Seq[(ClassDef, ConstructorDef)] = {
    collect[ConstructorDef](clazz, c => {
      c.params.size == args.size && args.zip(c.params).forall { case (t1, p) => subtype(t1, p.typ) }
    })
  }

  def lookupConstructor(clazz: Option[ClassDef], args: Seq[Type], location: SourceLocation): Option[(ClassDef, ConstructorDef)] = {
    val clsName = if (clazz.isDefined) clazz.get.name.raw else ""
    val allConstructor = lookupConstructorCandidates(clazz, args)
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
}
