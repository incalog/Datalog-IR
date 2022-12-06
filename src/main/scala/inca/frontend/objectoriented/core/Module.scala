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

// Note: The innerType is used for defunctionalized sets, to reflect the inner type of the set
case class ClassDef(annos: Seq[Annotation], vis: Option[Visibility], name: Name, parentClassRefs: Seq[ClassRef], content: Seq[ClassContent])
  extends SourceLocation with Annotations with VarReadExpr.Target {

  val contentMap: Map[Name, Seq[ClassContent]] = content.groupBy {
    case field: FieldDef => field.name
    case method: MethodDef => method.name
    case _: ConstructorDef => name
  }

  def fields: Seq[FieldDef] = content.collect { case f: FieldDef => f }
  def methods: Seq[MethodDef] = content.collect { case f: MethodDef => f }
  def constructors: Seq[ConstructorDef] = content.collect { case f: ConstructorDef => f }

  def typ: TClass = {
    val ref = ClassRef(name)
    ref.target = Some(this)
    TClass(ref)
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
    s"""$annoPrefix$indent${visS}class $name $parentClassesS {$contentS\n$indent}""".stripMargin
  }
  override def toString: String = prettyprint("")
}

case class ClassRef(name: Name) extends SourceLocation with Resolvable[ClassDef] {
  override def toString: String = name.toString
}

case class FieldDef(annos: Seq[Annotation], vis: Option[Visibility], name: Name, typ: Type, body: Option[Expression],
                    immutable: Boolean, aggregateMethod: Option[(ClassRef, Name)])
  extends ClassContent with Resolvable[MethodDef] {

  lazy val isAggregation: Boolean = aggregateMethod.isDefined

  def prettyprint(implicit indent: String): String = {
    val visS = if (vis.contains(Private)) "private " else ""
    val expr = if (body.isEmpty) "" else s" = ${body.get}"
    val prefix = if (immutable) "val " else "var "
    val descr = s"$indent$visS$prefix$name: ${typ.prettyprint}$expr"
    aggregateMethod match {
      case Some((ClassRef(refName), methodName)) =>
        s"$descr with $refName.$methodName"
      case None =>
        descr
    }
  }
}

case class MethodDef(annos: Seq[Annotation], vis: Option[Visibility], name: Name, params: Seq[Param], outType: Type, body: Seq[Statement])
  extends ClassContent with Resolvable[Signature] {

  lazy val vars: Map[Name, Option[Type]] = (body.flatMap(_.vars) ++ params.flatMap(_.vars)).toMap

  def returnsUnit: Boolean = outType == TUnit
  def isMain: Boolean = annos.contains(MainAnnotation)
  def isStatic: Boolean = isMain || annos.contains(StaticAnnotation)

  // The signature is resolved by the TypeContext. Type information about the methods and there superclasses is required
  // to correctly identify matching methods from the parent class.
  def signature: Signature = target.getOrElse(0)

  def prettyprint(implicit indent: String): String = {
    val visS = if (vis.contains(Private)) "private " else ""
    val paramsS = params.map(_.prettyprint).mkString(", ")
    val bodyS = body.map(_.prettyprint(indent + "\t")).mkString("\n")
    val outS = outType.prettyprint
    s"""$annoPrefix$indent${visS}def $name($paramsS): $outS = {
       |$bodyS
       |$indent}""".stripMargin
  }
}

case class ConstructorDef(annos: Seq[Annotation], vis: Option[Visibility], params: Seq[Param], body: Seq[Statement])
  extends ClassContent with Resolvable[Signature] {

  lazy val vars: Map[Name, Option[Type]] = (body.flatMap(_.vars) ++ params.flatMap(_.vars)).toMap

  def isMain: Boolean = annos.contains(MainAnnotation)
  def isStatic: Boolean = isMain || annos.contains(StaticAnnotation)
  def signature: Signature = target.getOrElse(0)

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