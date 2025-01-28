package inca.ir.optimize

import inca.ir
import inca.ir.Hint.preserveHints
import inca.ir.analysis.{AnalysisKey, AnalysisResult}
import inca.ir.printer.IRDebugPrinter
import inca.ir.util.SourceLocation
import inca.ir.{Atom, Body, Call, Eq, ExtensionalCall, Ref, Relation, Term, Var}
import inca.ir.visitors.IRVisitor

import scala.collection.{immutable, mutable}
import scala.collection.immutable.Queue
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

  private def trueStrictnessPoint(term: Term): Boolean =
    // TODO: Make this more precise by introducing a notion of "bound, but could be binding".
    //  E.g.
    //    A: fib$input(n: >TInt<)
    //    B: n: <TInt> == 0
    //    C: fib_result$0: >TInt< == 0
    //  For this body the analysis detects: A -> {}, B -> {A}, C -> {}. That is, A always appears before B, eventhough
    //  it would be nice if B should appear before A for performance reasons.
    //  All call arguments, all destruct arguments (expect the first) and equation params are bound, but could be
    //  binding.
    term.typ.get.mode.isBound //&& !term.typ.get.mode.isBoundCouldBeBinding

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
      val transitiveBindingRefs = paths.flatMap(_.atoms.flatMap(_.vars.filter(trueStrictnessPoint)).map(_.ref))
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
      case Var(ref) if trueStrictnessPoint(term) =>
        val paths = lookupTransitiveBindingSites(ref)
        paths.foreach(p => currentPath.atoms.map(_.updateAnalysisResult(UseDefResult(p))))
      case _ => // nothing
    super.visitTerm(term)
  }


class AtomReordering extends IRVisitor with Optimizer:
  override val name: String = "Reorder atoms"

  val preferEqOrdering: Ordering[Atom] = (x: Atom, y: Atom) => (x, y) match
    case (_: Eq, _) => 1
    case (_, _: Eq) => -1
    case _ => 0

  private def topologicalSort[T](dependencies: Map[T, Set[T]])(using baseOrdering: Ordering[T]): Seq[T] = {
    var inDegree = Map[T, Int]().withDefaultValue(0)
    var adjacencyList = Map[T, Set[T]]()

    dependencies.foreach { case (node, deps) =>
      adjacencyList += node -> deps
      deps.foreach(d => inDegree += d -> (inDegree(d) + 1))
      if (!inDegree.contains(node))
        inDegree += node -> 0
    }

    val inDegreeZero = inDegree.collect { case (node, 0) => node }
    var zeroInDegree = Queue.from(inDegreeZero).sorted(using baseOrdering)

    val sorted = mutable.Buffer[T]()
    while (zeroInDegree.nonEmpty) {
      val (node, updatedQueue) = zeroInDegree.dequeue
      zeroInDegree = updatedQueue
      sorted.append(node)

      adjacencyList.getOrElse(node, Set()).foreach { neighbor =>
        inDegree += neighbor -> (inDegree(neighbor) - 1)
        if (inDegree(neighbor) == 0)
          zeroInDegree = zeroInDegree.enqueue(neighbor).sorted(using baseOrdering)
      }
    }

    // Check for cycles (if there's still an in-degree > 0, it's a cycle)
    if (sorted.size != inDegree.size)
      throw new IllegalArgumentException("Topological sort not possible")

    sorted.reverse.toSeq
  }

  override def analyzeProgram(modules: Seq[ir.Module]): Unit =
    val analysis = UseDefAnalysis()
    analysis.visitProgram(modules)

    //val printer = new IRDebugPrinter {}
    //println(printer.prettyPrint(modules))

  override def visitBody(body: Body): Seq[Body] = preserveHints(body) {
    val dependencies = body.atoms.map { at =>
      at -> at.getAnalysisResult(UseDefKey).flatMap(_.path.atoms)
    }.toMap

    val sorted = topologicalSort(dependencies)(using preferEqOrdering)

    val bs = Body(sorted.flatMap(visitAtom))
    Seq(bs)
  }
