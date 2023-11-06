package inca.frontend.oodl.typechecker

import inca.frontend.oodl.syntax.*
import inca.ir.Name
import inca.ir.util.SourceLocation

import scala.collection.immutable.MultiDict

trait TypeContext extends TypeIO:
  private var vars: Map[Name, (Var.Target, Type, Boolean)] = Map()
  //private var tyVars: Map[Name, TName.Target] = Map()
  private var classDefs: MultiDict[Name, (Module, ClassDef)] = MultiDict()

  private var modules: Map[Name, Module] = Map()

  def scopedTypeContext[T](f: => T): T = {
    val varsSaved = vars
    //val tyVarsSaved = tyVars
    val c = classDefs
    val modulesSaved = modules
    val t = f
    vars = varsSaved
    //tyVars = tyVarsSaved
    classDefs = c
    modules = modulesSaved
    t
  }

  def bindVar(name: Name, decl: Var.Target, ty: Type, immutable: Boolean): Unit = {
    vars.get(name) foreach { case (previousDecl, _, _) =>
      error(s"Variable $name shadows previously defined variable $previousDecl", name, previousDecl)
    }
    vars += (name -> (decl, ty, immutable))
  }

  def lookupVar(name: Name): Option[(Var.Target,Type,Boolean)] =
    vars.get(name) match
      case Some(entry) => Some(entry)
      case None => None

  def isFreeVar(name: Name): Boolean =
    !vars.contains(name)

  def getBindings: Map[Name, Type] =
    vars.view.mapValues(_._2).toMap

  /*def bindTyVar(name: Name, decl: TName.Target): Unit = {
    tyVars.get(name) match {
      case Some(prevDecl) => error(s"Type Variable $name shadows previously defined type variable $name at $prevDecl")
      case None =>
    }
    tyVars += name ->decl
  }

  def lookupTyVar(name: Name): Option[TName.Target] = {
    tyVars.get(name) match {
      case Some(decl) => Some(decl)
      case None =>
        error(s"Unbound type variable $name", name)
        None
    }
  }*/

  //def isTypeVar(name: Name): Boolean = tyVars.contains(name)

  def bindModule(module: Module): Unit = {
    val name = module.name
    modules.get(name) foreach { bound =>
      error(s"Found multiple modules with same name $name", name, bound.name)
    }
    modules += (name -> module)
  }

  def lookupModule(name: Name): Option[Module] =
    modules.get(name) match {
      case Some(module) => Some(module)
      case None =>
        error(s"Unknown module $name", name)
        None
    }

  /** Module content */

  def bindClass(clazz: ClassDef, module: Module): Unit =
    classDefs += clazz.name -> (module, clazz)

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

  /** Class content */

  private def collect[C <: ClassContent](classDef: ClassDef)(f: ClassContent => Boolean): Seq[(ClassDef, C)] = {
    val content = classDef.content.flatMap {
      case c if f(c) => Some((classDef, c.asInstanceOf[C]))
      case _ => None
    }
    val parentContent = classDef.parentCls.flatMap {
      case t: TName => t.target match
        case Some(cls: ClassDef) => collect[C](cls)(f)
        case _ => Seq()
      case _ => Seq()
    }
    parentContent ++ content
  }

  def lookupFieldCandidates(classDef: ClassDef, fieldName: Name): Seq[(ClassDef, FieldDef)] = {
    val fieldCandidates = collect[FieldDef](classDef) {
      case f: FieldDef => f.name == fieldName
      case _ => false
    }
    val (generatedFields, userDefinedFields) = fieldCandidates.partition { (c, f) =>
      f.isGeneratedConstructorField
    }
    Seq() ++ generatedFields.headOption ++ userDefinedFields
  }

  def lookupField(classDef: ClassDef, fieldName: Name, location: SourceLocation*): Option[(ClassDef, FieldDef)] = {
    val fieldCandidates = lookupFieldCandidates(classDef, fieldName)
    if (fieldCandidates.isEmpty) {
      error(s"Undefined field '$fieldName' for class '${classDef.name}'", location: _*)
      None
    } else if (fieldCandidates.size > 1) {
      val (parentClass, _) = fieldCandidates.head
      error(s"Field '$fieldName' shadows previously defined field in class '${parentClass.name}'", location: _*)
      None
    } else {
      fieldCandidates.headOption
    }
  }

  def lookupMethodCandidates(classDef: ClassDef, name: Name, args: Seq[Type]): Seq[(ClassDef, MethodDef)] = {
    collect[MethodDef](classDef) {
      case m: MethodDef => m.name == name && m.params.size == args.size
      case _ => false
    }
  }
  
  def lookupMethod(classDef: ClassDef, name: Name, args: Seq[Type]): Option[(ClassDef, MethodDef)] = {
    val methodCandidates = lookupMethodCandidates(classDef, name, args)
    if (methodCandidates.isEmpty) {
      error(s"Undefined method '$name' for class '$classDef')", name)
      None
    } else {
      // always choose the method lowest in the class hierarchy
      methodCandidates.lastOption
    }
  }

  def lookupConstructorCandidates(classDef: ClassDef, params: Seq[Type]): Seq[(ClassDef, ConstructorDef)] = {
    collect[ConstructorDef](classDef) {
      case c: ConstructorDef => c.params.size == params.size
      case _ => false
    }
  }

  def lookupConstructor(classDef: ClassDef, args: Seq[Type], location: SourceLocation): Option[(ClassDef, ConstructorDef)] = {
    val allConstructor = lookupConstructorCandidates(classDef, args)
    val ambiguousConstructors = allConstructor.groupBy(_._1).filter(_._2.size > 1)

    if (allConstructor.isEmpty) {
      error(s"No matching constructor found for class ${classDef.name}: this(${args.mkString(",")})", location)
      None
    } else if (ambiguousConstructors.nonEmpty) {
      ambiguousConstructors.foreach {
        case (cls, _) => error(s"Ambiguous constructor for class ${cls.name}: this(${args.mkString(",")})", location)
      }
      None
    } else {
      // always choose the constructor lowest in the class hierarchy
      allConstructor.lastOption
    }
  }


