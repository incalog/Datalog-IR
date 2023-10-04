package inca.frontend.objectoriented.core

import inca.compiler.SourceLocation
import inca.frontend.util.Resolvable

import scala.language.postfixOps

case class Module(name: Name, imports: Seq[Import], classes: Seq[ClassDef])
  extends SourceLocation with Import.Target {

  def usedModuleNames: Seq[Name] = name +: imports.map(_.name)

  def prettyprint(implicit indent: String): String = {
    val importsS = if (imports.isEmpty) "" else
      "\n" + imports.map(_.prettyprint).mkString("\n")
    val contentS = if (classes.isEmpty) "" else
      "\n" + classes.map(_.prettyprint("")).mkString("\n")
    s"${indent}module $name\n$importsS$contentS".stripMargin
  }

  override def toString: String = prettyprint("")
}

case class Import(name: Name) extends SourceLocation with Resolvable[Import.Target] {
  def prettyprint(implicit indent: String): String = s"${indent}import $name"
}
object Import {
  trait Target
}

trait ClassContent extends SourceLocation with Annotations {
  def vis: Option[Visibility]
  def prettyprint(implicit indent: String): String
  override def toString: String = prettyprint("")
}

case class GenericParamDef(name: Name) extends SourceLocation with TName.Target {
  def prettyprint(implicit indent: String): String = name.toString
  override def toString: String = prettyprint("")
}

// Note: The innerType is used for defunctionalized sets, to reflect the inner type of the set
case class ClassDef(annos: Seq[Annotation], vis: Option[Visibility], name: Name, genericTypeParams: Seq[GenericParamDef], parentClassRefs: Seq[TName], content: Seq[ClassContent])
  extends SourceLocation with Annotations with TName.Target with VarReadExpr.Target {

  val contentMap: Map[Name, Seq[ClassContent]] = content.groupBy {
    case field: FieldDef => field.name
    case method: MethodDef => method.name
    case _: ConstructorDef => name
  }

  def fields: Seq[FieldDef] = content.collect { case f: FieldDef => f }
  def methods: Seq[MethodDef] = content.collect { case f: MethodDef => f }
  def constructors: Seq[ConstructorDef] = content.collect { case f: ConstructorDef => f }
  def isCaseClass: Boolean = annos.contains(CaseAnnotation)
  def isMonotoneClass: Boolean = annos.exists(a => a.isInstanceOf[MonotoneAnnotation]) || isMonotoneMapClass
  def isMonotoneMapClass: Boolean = annos.exists(a => a.isInstanceOf[MonotoneMapAnnotation])
  def isAbstract: Boolean = annos.contains(AbstractAnnotation)
  def isGeneric: Boolean = genericTypeParams.nonEmpty
  def containsMain: Boolean = methods.exists(m => m.isMain)
  def getMain: Seq[MethodDef] = methods.filter(m => m.isMain)

  def isDefunAuxiliary: Boolean = annos.contains(DefunAuxiliaryAnnotation)
  def montoneTypes: Option[(Type, Type)] = annos.flatMap {
    case MonotoneMapAnnotation(types) => Some((types.head, types.last))
    case MonotoneAnnotation(_, types) => Some((types.head, types.last))
    case _ => None
  }.headOption

  def typ: TClass = {
    val ref = TName(name)
    ref.target = Some(this)
    val tclass = TClass(ref)
//    tclass.tyArgs = ref.tyArgs
    tclass
  }

  def prettyprint(implicit indent: String): String = {
    val visS = if (vis.contains(Private)) "private " else ""
    val contentS = if (content.isEmpty) "" else
      "\n" + content.map(_.prettyprint(indent+"\t")).mkString("\n\n")
    val parentClassesS = if (parentClassRefs.nonEmpty) {
      val tailS = if (parentClassRefs.tail.nonEmpty) "with " + parentClassRefs.tail.mkString("with ") else ""
      s"""extends ${parentClassRefs.head} $tailS"""
    } else
      s""
    val genericTypeParam = if (genericTypeParams.nonEmpty) genericTypeParams.mkString("[", ", ", "]") else ""
    s"""$annoPrefix$indent${visS}class $name$genericTypeParam $parentClassesS {$contentS\n$indent}""".stripMargin
  }
  override def toString: String = prettyprint("")
}

case class FieldDef(annos: Seq[Annotation], vis: Option[Visibility], name: Name, typ: Type, body: Option[Expression],
                    immutable: Boolean)
  extends ClassContent with Resolvable[MethodDef] {

  def prettyprint(implicit indent: String): String = {
    val visS = if (vis.contains(Private)) "private " else ""
    val expr = if (body.isEmpty) "" else s" = ${body.get}"
    val prefix = if (immutable) "val " else "var "
    s"$indent$visS$prefix$name: ${typ.prettyprint}$expr"
  }
}

case class MethodDef(annos: Seq[Annotation], vis: Option[Visibility], name: Name, genericTypeParams: Seq[GenericParamDef], params: Seq[Param], outType: Type, body: Seq[Statement])
  extends ClassContent with Resolvable[Signature] {

  lazy val vars: Map[Name, Option[Type]] = (body.flatMap(_.vars) ++ params.flatMap(_.vars)).toMap

  def returnsUnit: Boolean = outType == TUnit
  def isMain: Boolean = annos.contains(MainAnnotation)
  def isStatic: Boolean = isMain || annos.contains(StaticAnnotation)
  def isGeneric: Boolean = genericTypeParams.nonEmpty

  // The signature is resolved by the TypeContext. Type information about the methods and there superclasses is required
  // to correctly identify matching methods from the parent class.
  def signature: Signature = target.getOrElse("")

  def prettyprint(implicit indent: String): String = {
    val visS = if (vis.contains(Private)) "private " else ""
    val paramsS = params.map(_.prettyprint).mkString(", ")
    val bodyS = body.map(_.prettyprint(indent + "\t")).mkString("\n")
    val outS = outType.prettyprint
    val genericTypeParam = if (genericTypeParams.nonEmpty) genericTypeParams.mkString("[", ", ", "]") else ""

    s"""$annoPrefix$indent${visS}def $name$genericTypeParam($paramsS): $outS = {
       |$bodyS
       |$indent}""".stripMargin
  }
}

case class ConstructorDef(annos: Seq[Annotation], vis: Option[Visibility], params: Seq[Param], body: Seq[Statement])
  extends ClassContent with Resolvable[Signature] {

  lazy val vars: Map[Name, Option[Type]] = (body.flatMap(_.vars) ++ params.flatMap(_.vars)).toMap

  def isMain: Boolean = annos.contains(MainAnnotation)
  def isPrimary: Boolean = annos.contains(PrimaryAnnotation)
  def isStatic: Boolean = isMain || annos.contains(StaticAnnotation)
  def signature: Signature = target.getOrElse("")

  def prettyprint(implicit indent: String): String = {
    val visS = if (vis.contains(Private)) "private " else ""
    val paramsS = params.map(_.prettyprint).mkString(", ")
    val bodyS = body.map(_.prettyprint(indent + "\t")).mkString("\n")
    s"""$annoPrefix$indent${visS}this($paramsS) = {
       |$bodyS
       |$indent}""".stripMargin
  }
}

case class Param(name: Name, typ: Type) extends SourceLocation with VarReadExpr.Target {
  def vars: Map[Name, Option[Type]] = Map(name -> Some(typ))

  override def toString: String = prettyprint
  def prettyprint: String = s"$name: ${typ.prettyprint}"
}


