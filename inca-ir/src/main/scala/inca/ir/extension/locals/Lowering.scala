package inca.ir.extension.locals

import inca.ir
import inca.ir.{Atom, BaseIR, Body, Eq, Name, Relation, Term, Var}
import inca.ir.util.{BodyAwareVisitor, SourceLocation}
import inca.ir.extension.locals.IR
import inca.ir.lowering.BaseLowering

import scala.compiletime.uninitialized

/**
 * Inspired by an SSA transformation, we introduce versioned variables for mutation.
 * For example, the following Datalog code:
 *
 * var a = 4
 * val b = a + 1 // 5
 * a = a + 5
 * val c = a + 1 // 10
 *
 * lowers to:
 *
 * a$0 == 4
 * b == a$0 + 1 // 5
 * a$1 == a$0 + 5
 * c == a$1 + 1 // 10
 *
 * Note that vals are never changes, and only vars are mutated.
 */
object VersionedVarRewriter:
  private val sepSymbol = "_"

  def decompileName(name: Name): (Name, Option[Int]) =
    val s = name.name
    val ix = s.lastIndexOf(sepSymbol)
    if (ix <= 0)
      (ensureSeparator(s), None)
    else
      val digits = s.substring(ix + 1)
      digits.toIntOption match
        case Some(num) =>
          (Name(s.substring(0, ix + 1)), Some(num))
        case None =>
          (ensureSeparator(s), None)

  def ensureSeparator(s: String): Name =
    if (s.endsWith(sepSymbol) && s != sepSymbol) Name(s)
    else Name(s + sepSymbol)

  def baseName(name: Name): Name = VersionedVarRewriter.decompileName(name)._1

  def max(names: Seq[Name]): Name =
    val (decomp, indices) = names.map(VersionedVarRewriter.decompileName).unzip
    val (_, maxIndex) = indices.zipWithIndex.maxBy(_._1.getOrElse(0))
    names(maxIndex)


class VersionedVarRewriter:
  private var used: Map[Name, Int] = Map()

  def freshName(name: Name): Name =
    val base = VersionedVarRewriter.baseName(name)
    used.get(base) match
      case Some(count) =>
        val v = Name(base.name + count)
        used += base -> (count + 1)
        v
      case None =>
        used += base -> 1
        Name(base.name + 0)

  def lastName(name: Name): Name =
    val base = VersionedVarRewriter.baseName(name)
    used.get(base) match
      case Some(count) => Name(base.name + count)
      case None => Name(base.name + 0)

  def isRegistered(name: Name): Boolean = used.contains(VersionedVarRewriter.baseName(name))

  def scoped[A](f: => A): A =
    val oldused = this.used
    try f
    finally this.used = oldused


trait Lowering extends BaseLowering with BodyAwareVisitor:
  enum Phase:
    case Collect
    case Rewrite
  import Phase.*

  override val name: String = "Locals"
  override val loweredIRs: Set[BaseIR] = Set(IR)
  override val requiredIRs: Set[BaseIR] = Set()

  private var phase: Phase = Collect
  private val varRewriter = new VersionedVarRewriter

  type BaseName = Name
  type Enclosure = SourceLocation
  private var maxBodyVars: Map[(Enclosure, Body, BaseName), Name] = Map()

  private def freshVersionedName(name: Name, enclosure: SourceLocation): Name = varRewriter.freshName(name)

  private def getCurrentVersionedName(name: Name): Name = varRewriter.lastName(name)

  private def isMutable(name: Name): Boolean = varRewriter.isRegistered(name)

  override def visitModule(module: ir.Module): ir.Module =
    phase = Collect
    super.visitModule(module)
    phase = Rewrite
    super.visitModule(module)

  // When exiting a body we need to reset the variables. For example, each disjunctive body can use the same variable
  // name again.
  override def visitBody(body: Body, enclosure: SourceLocation, parentEnclosureOption: Option[SourceLocation]): Seq[Body] =
    val newBodies = varRewriter.scoped(super.visitBody(body, enclosure, parentEnclosureOption))

    phase match
      case Collect =>
        // Store the last variable for each body
        maxBodyVars ++= body.vars
          .filter(v => isMutable(v.name))
          .map { v =>
            val baseName = VersionedVarRewriter.baseName(v.name)
            val key = (enclosure, body, baseName)
            key -> getCurrentVersionedName(baseName)
          }
        newBodies
      case Rewrite =>
        newBodies.map { b =>
          val ats = b.atoms
          val maxBodyVarsByEnclosure = maxBodyVars.groupBy {
            case ((enclose, _, baseName), _) => (enclose, baseName)
          }.view.mapValues { v =>
            VersionedVarRewriter.max(v.values.toSeq)
          }

          val maxVarConstraints = maxBodyVars.flatMap {
            case ((`enclosure`, `body`, baseName), newName) =>
              val maxName = maxBodyVarsByEnclosure((enclosure, baseName))
              Some(Eq(Var(maxName), Var(newName)))
            case _ =>
              None
          }
          Body(ats ++ maxVarConstraints)
        }

  override def visitAtom(atom: Atom, enclosure: SourceLocation, parentEnclosureOption: Option[SourceLocation]): Seq[Atom] =
    def eqConstraint(lhsName: Name, rhs: Term): Seq[Atom] =
      val Seq(assign) = visitTerm(rhs)
      Seq(Eq(Var(lhsName), assign))

    phase match
      case Collect =>
        super.visitAtom(atom, enclosure, parentEnclosureOption)
      case Rewrite => atom match
        case DeclVal(v@Var(ref), t) => eqConstraint(ref.name, t)
        case DeclVar(v@Var(ref), t) => eqConstraint(freshVersionedName(ref.name, enclosure), t)
        case Assign(v@Var(ref), t) => eqConstraint(getCurrentVersionedName(ref.name), t)
        case _ => super.visitAtom(atom, enclosure, parentEnclosureOption)

  override def visitTerm(term: Term): Seq[Term] =
    phase match
      case Collect =>
        super.visitTerm(term)
      case Rewrite => term match
        case v@Var(ref) if isMutable(ref.name) => Seq(Var(getCurrentVersionedName(ref.name)))
        case _ => super.visitTerm(term)
