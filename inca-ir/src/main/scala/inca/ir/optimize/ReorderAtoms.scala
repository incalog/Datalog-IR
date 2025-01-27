package inca.ir.optimize

import inca.ir
import inca.ir.Hint.preserveHints
import inca.ir.analysis.{AnalysisKey, AnalysisResult}
import inca.ir.printer.IRDebugPrinter
import inca.ir.util.SourceLocation
import inca.ir.{Atom, Body, Call, Eq, ExtensionalCall, Ref, Relation, Term, Var}
import inca.ir.visitors.IRVisitor

import scala.compiletime.uninitialized

case class Path(ps: Seq[SourceLocation]):
  def ++(other: Path): Path = Path(this.ps ++ other.ps)
  def +:(location: SourceLocation): Path = Path(this.ps :+ location)
  def length: Int = ps.length
  def contains(loc: SourceLocation): Boolean = ps.contains(loc)

  lazy val atoms: Seq[Atom] = ps.reverse.collect { case at: Atom => at }
  lazy val directEnclosingAtom: Option[Atom] = ps.reverse.collectFirst { case at: Atom => at }
  override def toString: String = directEnclosingAtom.getOrElse("Missing atom").toString


object Path:
  def empty: Path = new Path(Seq())
  def apply(ps: Seq[SourceLocation]): Path = new Path(ps)
  def apply(loc: SourceLocation): Path = new Path(Seq(loc))

case object UseDefKey extends AnalysisKey:
  override val key: String = "UseDef"
  override type Result = UseDefResult

case class UseDefResult(path: Path) extends AnalysisResult:
  val result: UseDefResult = this
  def contains(loc: SourceLocation): Boolean = path.contains(loc)
  override val akey: UseDefKey.type = UseDefKey
  override def toString: String = path.toString


class UseDefAnalysis extends IRVisitor:

  def extendPath[A](loc: SourceLocation)(f: => A): A = {
    val oldpath = currentPath
    try {
      currentPath +:= loc
      val a = f
      a
    } finally {
      this.currentPath = oldpath
    }
  }

  def freshPath[A](loc: SourceLocation)(f: => A): A = {
    this.currentPath = Path(loc)
    this.bindingSites = Map()
    try {
      currentPath +:= loc
      val a = f
      a
    } finally {
      this.bindingSites = Map()
      this.currentPath = Path.empty
    }
  }

  private var currentPath: Path = uninitialized

  private var bindingSites: Map[Ref[Var.Target], Set[Path]] = Map()
  private def addBindingSite(ref: Ref[Var.Target]): Unit = bindingSites += ref -> (bindingSites.getOrElse(ref, Set()) + currentPath)
  private def lookupBindingSites(ref: Ref[Var.Target]): Set[Path] = bindingSites(ref)
  private def lookupTransitiveBindingSites(ref: Ref[Var.Target]): Set[Path] =
    var queue = Set(ref)
    var visited: Set[Ref[Var.Target]] = Set()
    var paths: Set[Path] = Set()
    while(queue.nonEmpty) {
      val curRef = queue.head
      visited += curRef
      queue = queue.tail
      paths ++= lookupBindingSites(curRef)
      val transitiveBindingRefs = paths.flatMap(_.atoms.flatMap(_.vars.filter(_.typ.get.mode.isBound)).map(_.ref))
      queue ++= (transitiveBindingRefs -- visited)
    }
    paths

  override def visitRelation(relation: Relation): Seq[Relation] = preserveHints(relation) {
    Seq(Relation(relation.name, relation.params.flatMap(visitParam), relation.bodies.flatMap(b => freshPath(b)(visitBody(b)))))
  }

  override def visitAtom(atom: Atom): Seq[Atom] = extendPath(atom) {
    super.visitAtom(atom)
  }

  override def visitTerm(term: Term): Seq[Term] = extendPath(term) {
    term match
      case Var(ref) if term.typ.get.mode.isBinding =>
        addBindingSite(ref)
      case Var(ref) =>
        val paths = lookupTransitiveBindingSites(ref)
        paths.foreach(p => currentPath.atoms.map(_.updateAnalysisResult(UseDefResult(p))))
      case _ => // nothing
    super.visitTerm(term)
  }


class ReorderAtoms extends IRVisitor with Optimizer:
  override val name: String = "Reorder atoms"

  given Ordering[Atom] = (x: Atom, y: Atom) =>
    val xRes = x.getAnalysisResult(UseDefKey)
    val yRes = y.getAnalysisResult(UseDefKey)
    val xUsesY = xRes.exists(_.contains(y))
    val yUsesX = yRes.exists(_.contains(x))

    println(s"$x <-> $y :: $xUsesY")
    println(s"$y <-> $x :: $yUsesX")
    println()

    if (xUsesY) {
      // x after y
      1
    } else if (yUsesX) {
      // y after x
      -1
    } else {
      // independent, order by king
      (x, y) match
        case (_: Eq, _) => -1
        case (_, _: Eq) => 1
        case _ => 0
    }

  override def analyzeProgram(modules: Seq[ir.Module]): Unit =
    val analysis = UseDefAnalysis()
    analysis.visitProgram(modules)

    //val printer = new IRDebugPrinter {}
    //println(printer.prettyPrint(modules))

  override def visitBody(body: Body): Seq[Body] = preserveHints(body) {
    val bs = Body(body.atoms.sorted)
    println(bs)
    Seq(bs)
  }
