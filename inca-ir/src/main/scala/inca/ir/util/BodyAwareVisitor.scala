package inca.ir.util

import inca.ir.{Atom, Body, Relation}
import inca.ir.visitors.IRVisitor

/**
 * For each construct that contains a body a possible parent construct exists, that contains this child body.
 * E.g a body of a disjunction is contained in the body of a relation, as such we say that the parent of the disjunction
 * is the relation. This visitor keeps track of such relationships and provides methods for visiting atoms and bodies
 * that includes this information.
 * */
trait BodyAwareVisitor extends IRVisitor:
  private def currentEnclosure: SourceLocation = _currentEnclosure.get

  private def parentEnclosure: Option[SourceLocation] = _parentEnclosure

  private var _parentEnclosure: Option[SourceLocation] = None
  private var _currentEnclosure: Option[SourceLocation] = None
  private var currentAtom: Option[Atom] = None
  private var currentRelation: Option[Relation] = None

  private var visitedLocations: Set[SourceLocation] = Set()

  protected def locationScoped[A](f: => A): A =
    val oldCurrentAtom = currentAtom
    val oldCurrentRelation = currentRelation
    val oldParentEnclosure = _parentEnclosure
    _parentEnclosure = _currentEnclosure
    try f
    finally {
      currentAtom = oldCurrentAtom
      currentRelation = oldCurrentRelation
      _currentEnclosure = _parentEnclosure
      _parentEnclosure = oldParentEnclosure
    }

  override def visitRelation(relation: Relation): Seq[Relation] =
    currentRelation = Some(relation)
    super.visitRelation(relation)

  def visitBody(body: Body, enclosure: SourceLocation, parentEnclosureOption: Option[SourceLocation]): Seq[Body] =
    super.visitBody(body)

  override def visitBody(body: Body): Seq[Body] = locationScoped {
    _currentEnclosure = currentAtom match
      case a@Some(atom) => a
      case _ => currentRelation
    visitedLocations += currentEnclosure
    val bs = visitBody(body, currentEnclosure, parentEnclosure)
    currentAtom = None
    bs
  }

  def exitEnclosure(enclosure: SourceLocation, parentEnclosureOption: Option[SourceLocation]): Unit

  def visitAtom(atom: Atom, enclosure: SourceLocation, parentEnclosureOption: Option[SourceLocation]): Seq[Atom] =
    super.visitAtom(atom)

  override def visitAtom(atom: Atom): Seq[Atom] =
    currentAtom = Some(atom)
    val as = visitAtom(atom, currentEnclosure, parentEnclosure)
    if (visitedLocations.contains(atom))
      exitEnclosure(atom, _currentEnclosure)
    as
