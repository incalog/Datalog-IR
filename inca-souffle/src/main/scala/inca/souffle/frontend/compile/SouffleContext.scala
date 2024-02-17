package inca.souffle.frontend.compile

import inca.souffle.syntax.ProgramContent.*
import inca.souffle.syntax.{ADTConstructor, ComponentType, QualifiedName, TypeDeclConstraint}

import scala.annotation.tailrec


trait SouffleContext:

  private var relDecls: Map[String, RelationDecl] = Map()
  private var compDecls: Map[ComponentType, ComponentDecl] = Map()
  private var typeDecls: Map[String, TypeDecl] = Map()
  private var adtConstrs: Map[String, TypeDecl] = Map()
  private var compInits: Map[String, ComponentInit] = Map()
  private var currentNestedComponent: Seq[ComponentType] = Seq()

  private var componentTypeToTypeDecl: Map[Seq[ComponentType], Map[String, TypeDecl]] = Map()
  private var componentTypeToADTConstr: Map[Seq[ComponentType], Map[String, TypeDecl]] = Map()
  private var componentTypeToInits: Map[Seq[ComponentType], Map[String, ComponentInit]] = Map()
  private var componentTypeToRelDecl: Map[Seq[ComponentType], Map[String, RelationDecl]] = Map()

  def scopedTypeContext[T](f: => T): T = {
    val declsSaved = relDecls
    val compsSaved = compDecls
    val typeDeclsSaved = typeDecls
    val compInitsSaved = compInits
    val savedCurrentNestedComponent = currentNestedComponent
    val t = f
    relDecls = declsSaved
    compDecls = compsSaved
    typeDecls =  typeDeclsSaved
    compInits = compInitsSaved
    currentNestedComponent = savedCurrentNestedComponent
    t
  }

  def newComponentLevel(compType: ComponentType): Unit =
    currentNestedComponent = currentNestedComponent :+ compType

  def bindRelationDecl(decl: RelationDecl): Unit =
    val relMap = decl.names.map { name =>
      name -> decl
    }.toMap
    relDecls ++= relMap
    val updatedRelMap = componentTypeToRelDecl.getOrElse(currentNestedComponent, Map()) ++ relMap
    componentTypeToRelDecl += (currentNestedComponent -> updatedRelMap)

  def lookupRelationDecl(qn: QualifiedName): Option[RelationDecl] =
    // no prefix
    if (qn.ns.size == 1)
      relDecls.get(qn.ns.head)
    else
      lookupRelationDeclHelper(currentNestedComponent, qn.ns)

  @tailrec
  private def lookupRelationDeclHelper(compPath: Seq[ComponentType], qn: Seq[String]): Option[RelationDecl] =
    if (qn.size == 1)
      componentTypeToRelDecl.get(compPath) match
        case Some(relMap) => relMap.get(qn.head)
        case None => throw IllegalArgumentException("FAIL3")
    else
      componentTypeToInits.get(compPath) match
        case Some(compInitMap) =>
          compInitMap.get(qn.head) match
            case Some(compInit) =>
              lookupRelationDeclHelper(compPath :+ compInit.compType, qn.tail)
            case None => throw IllegalArgumentException(s"FAIL1: Could not find $qn at level ${compPath.mkString(", ")}")
        case None => throw IllegalArgumentException(s"FAIL2: Could not find $qn at level ${compPath.mkString(", ")}")


  def bindComponentDecl(comp: ComponentDecl): Unit =
    // TODO consider type parameters
    compDecls += (comp.ty -> comp)

  def lookupComponentDecl(compType: ComponentType): Option[ComponentDecl] =
    compDecls.get(compType)

  def bindTypeDecl(decl: TypeDecl): Unit =
    // collect adt constructors first
    decl.rhs match
      case TypeDeclConstraint.ADTType(alts) =>
        alts.foreach { constr =>
          adtConstrs += (constr.name-> decl)
          val updatedADTMap = componentTypeToADTConstr.getOrElse(currentNestedComponent, Map()) ++ Map(constr.name -> decl)
          componentTypeToADTConstr += (currentNestedComponent -> updatedADTMap)
        }
      case _ => // do nothing

    typeDecls += (decl.name -> decl)
    val updatedTypeMap = componentTypeToTypeDecl.getOrElse(currentNestedComponent, Map()) ++ Map(decl.name -> decl)
    componentTypeToTypeDecl += (currentNestedComponent -> updatedTypeMap)

  def lookupTypeDeclDecl(qn: QualifiedName): Option[TypeDecl] =
    // no prefix
    if (qn.ns.size == 1)
      typeDecls.get(qn.ns.head)
    else
      lookupTypeDeclHelper(currentNestedComponent, qn.ns)

  @tailrec
  private def lookupTypeDeclHelper(compPath: Seq[ComponentType], qn: Seq[String]): Option[TypeDecl] =
    if (qn.size == 1)
      componentTypeToTypeDecl.get(compPath) match
        case Some(typeMap) => typeMap.get(qn.head)
        case None => throw IllegalArgumentException("FAIL3")
    else
      componentTypeToInits.get(compPath) match
        case Some(compInitMap) =>
          compInitMap.get(qn.head) match
            case Some(compInit) =>
              lookupTypeDeclHelper(compPath :+ compInit.compType, qn.tail)
            case None => throw IllegalArgumentException(s"FAIL1: Could not find $qn at level ${compPath.mkString(", ")}")
        case None => throw IllegalArgumentException(s"FAIL2: Could not find $qn at level ${compPath.mkString(", ")}")

  def bindComponentInit(compInit: ComponentInit): Unit =
    compInits += (compInit.n -> compInit)
    val newInitMap = componentTypeToInits.getOrElse(currentNestedComponent, Map()) + (compInit.n -> compInit)
    componentTypeToInits += currentNestedComponent -> newInitMap

  def lookupComponentInt(name: String): Option[ComponentInit] =
    compInits.get(name)


  def lookupADTConstructor(qn: QualifiedName): Option[TypeDecl] =
    // no prefix
    if (qn.ns.size == 1)
      adtConstrs.get(qn.ns.head)
    else
      lookupADTConstructorHelper(currentNestedComponent, qn.ns)

  @tailrec
  private def lookupADTConstructorHelper(compPath: Seq[ComponentType], qn: Seq[String]): Option[TypeDecl] =
    if (qn.size == 1)
      componentTypeToADTConstr.get(compPath) match
        case Some(adtMap) => adtMap.get(qn.head)
        case None => throw IllegalArgumentException("FAIL3")
    else
      componentTypeToInits.get(compPath) match
        case Some(compInitMap) =>
          compInitMap.get(qn.head) match
            case Some(compInit) =>
              lookupADTConstructorHelper(compPath :+ compInit.compType, qn.tail)
            case None => throw IllegalArgumentException(s"FAIL1: Could not find $qn at level ${compPath.mkString(", ")}")
        case None => throw IllegalArgumentException(s"FAIL2: Could not find $qn at level ${compPath.mkString(", ")}")

