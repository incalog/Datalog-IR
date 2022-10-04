package inca.frontend.objectoriented.typechecker;

import inca.compiler.SourceLocation
import inca.frontend.objectoriented.core._

import scala.collection.immutable.MultiDict

trait TypeContext extends TypeIO {
  private var modules: Map[Name, Module] = Map()
  private var classDefs: MultiDict[Name, (Module, ClassDef)] = MultiDict()
  private var vars: Map[Name, (VarReadExpr.Target, Type, Boolean)] = Map()

  def scopedTypeContext[T](f: => T): T = {
    val v = vars
    val c = classDefs
    val t = f
    vars = v
    classDefs = c
    t
  }

  def bindVar(name: Name, decl: VarReadExpr.Target, ty: Type, immutable: Boolean): Unit = {
    vars.get(name) foreach { case (previousDecl, _, _) =>
      error(s"Variable $name shadows previously defined variable $previousDecl", name, previousDecl)
    }
    vars += (name -> (decl, ty, immutable))
  }

  def lookupVar(name: Name): Option[(VarReadExpr.Target, Type, Boolean)] =
    vars.get(name) match {
      case Some(entry) => Some(entry)
      case None =>
        error(s"Unbound variable $name", name)
        None
    }

  private def collectFields(clazz: Option[ClassDef], name: Name): Seq[(ClassDef, FieldDef)] = {
    if (clazz.isEmpty) {
      error(s"Undefined class in field lookup", name)
      Seq()
    } else {
      val fields = clazz.get.fields.filter(_.name == name).map((clazz.get, _))
      val parentFields = clazz.get.parentClassRefs.flatMap(ref => collectFields(ref.target, name))
      parentFields ++ fields
    }
  }

  def lookupField(clazz: Option[ClassDef], name: Name): Option[(ClassDef, FieldDef)] = {
    if (clazz.isEmpty) {
      error(s"Undefined class in field lookup", name)
      return None
    }

    val allFields = collectFields(clazz, name)
    if (allFields.isEmpty) {
      error(s"Undefined field ${clazz.get.name}.$name", name)
    } else if (allFields.size > 1) {
      val (parentClass, _) = allFields.head
      error(s"Field $name shadows previously defined field in class ${parentClass.name}", name)
    }
    Some(allFields.head)
  }

  def lookupMethod(clazz: Option[ClassDef], name: Name): Option[MethodDef] = {
    if (clazz.isEmpty) {
      error(s"Undefined class in method lookup", name)
      None
    } else {
      var methods = clazz.get.methods.filter(_.name == name)
      if (methods.isEmpty) {
        // Check if the method is inherited from a parent class
        // We must guarantee that all classRefs are resolved here. This should be the case, since we only have a single
        // classDef instance per class stored in the module. Each classDef instance has a list of classRefs, that are
        // resolved at the beginning of typechecking a module.
        methods = clazz.get.parentClassRefs.flatMap { ref =>
          lookupMethod(ref.target, name)
        }
        if (methods.isEmpty) {
          error(s"Undefined method ${clazz.get.name}.$name", name)
        }
      }
      methods.headOption
    }
  }

  def lookupConstructor(clazz: Option[ClassDef], numArgs: Int, location: SourceLocation): Option[ConstructorDef] = {
    if (clazz.isEmpty) {
      error(s"Undefined class in constructor lookup", location)
      None
    } else {
      var constructors = clazz.get.constructors
        .filter(_.params.size == numArgs)
      if (constructors.isEmpty) {
        // Check if the parent class has a constructor
        constructors = clazz.get.parentClassRefs.flatMap { ref =>
          lookupConstructor(ref.target, numArgs, location)
        }
        // We will just use the default construct with all fields, that will be generated automatically
        if (constructors.isEmpty) {
          error(s"Undefined constructor ${clazz.get.name}", location)
        }
      }

      if (constructors.size > 1) {
        error(s"Ambiguous constructor ${clazz.get.name}", location)
      }
      constructors.headOption
    }
  }

  def bindClass(clazz: ClassDef, module: Module): Unit = {
    classDefs += clazz.name -> (module, clazz)
  }

  def lookupClass(name: Name): Option[ClassDef] =
    classDefs.get(name) match {
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
}
