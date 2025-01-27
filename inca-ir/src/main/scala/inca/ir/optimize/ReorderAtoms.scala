package inca.ir.optimize

import inca.ir
import inca.ir.Hint.preserveHints
import inca.ir.analysis.{AnalysisKey, AnalysisResult}
import inca.ir.printer.IRDebugPrinter
import inca.ir.util.SourceLocation
import inca.ir.{Atom, Body, Ref, Relation, Term, Var}
import inca.ir.visitors.IRVisitor

import scala.compiletime.uninitialized

case class Path(ps: Seq[SourceLocation]):
  def ++(other: Path): Path = Path(this.ps ++ other.ps)
  def +:(location: SourceLocation): Path = Path(this.ps :+ location)
  def length: Int = ps.length
  def contains(loc: SourceLocation): Boolean = ps.contains(loc)

  lazy val atom: Option[Atom] = ps.reverse.collectFirst { case at: Atom => at }
  override def toString: String = atom.toString


object Path:
  def empty: Path = new Path(Seq())
  def apply(ps: Seq[SourceLocation]): Path = new Path(ps)
  def apply(loc: SourceLocation): Path = new Path(Seq(loc))

case object UseDefKey extends AnalysisKey:
  override val key: String = "UseDef"
  override type Result = UseDefResult

case class UseDefResult(path: Path) extends AnalysisResult:
  val result: UseDefResult = this
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

  private var bindingSites: Map[Ref[?], Seq[Path]] = Map()
  private def addBindingSite(ref: Ref[?]): Unit = bindingSites += ref -> (bindingSites.getOrElse(ref, Seq()) :+ currentPath)
  private def lookupBindingSites(ref: Ref[?]): Seq[Path] = bindingSites(ref)

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
        val paths = lookupBindingSites(ref)
        paths.foreach(p => term.updateAnalysisResult(UseDefResult(p)))
      case _ => // nothing
    super.visitTerm(term)
  }


class ReorderAtoms extends IRVisitor with Optimizer:
  override val name: String = "Reorder atoms"

  override def analyzeProgram(modules: Seq[ir.Module]): Unit =
    val analysis = UseDefAnalysis()
    analysis.visitProgram(modules)

    val printer = new IRDebugPrinter {}
    println(printer.prettyPrint(modules))