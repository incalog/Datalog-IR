package inca.foreign.scala.ir.data

import inca.foreign.scala.ir.BaseScalaLowering
import inca.ir.{Atom, BaseIR, ExtensionalRelation, ModuleEntry, Name, Relation, Term, TermType, Type, name2string}
import inca.ir.lowering.BaseLowering
import inca.ir.extension.block
import inca.ir.extension.data
import inca.foreign.scala.syntax.Scala
import inca.foreign.scala.ir.primitive.{IR, ScalaDefnModuleEntry, ScalaInca, ScalaTerm, ScalaType}
import inca.ir.Hint.preserveHints
import inca.ir.extension.data.{CaseDefinition, Construct, DataDefinition, Deconstruct, TData}

/**
 * Proposal: Representing ADT as scala enum
 *
 * Step 1: For each ADT definition generate a corresponding scala enum.
 * Step 2: Transfer each ADT type to scala type of the corresponding enum
 * Step 3: Constructing an ADT instance should generate a scala object of this type
 * Step 4: Deconstructing an ADT instance
 *         - extract values via pattern match to optional tuple
 *         - check that tuple is not None
 *         - generate datalog variable for each tuple entry
 */
trait ScalaLowering extends BaseScalaLowering:
  override val loweredIRs: Set[BaseIR] = Set(IR)
  override val requiredIRs: Set[BaseIR] = Set(IR, block.IR)

  override def isTypeSupported(ty: Type): Boolean = ty match
    case TData(name) => true
    case _ => false

  private def translateCaseDefinition(caseDef: CaseDefinition, dataDefinition: DataDefinition): ScalaDefnModuleEntry =
    val CaseDefinition(name, tys) = caseDef
    val params = tys.zipWithIndex.map {
      case (ty, idx) =>
        visitType(ty) match
          case ScalaType(sty) => Scala.Param(s"param_$idx", sty)
          case _ => throw IllegalArgumentException(s"Expected a scala type, but got $ty")
    }
    val sClass =
      if (params.nonEmpty)
        Scala.Class(name, Seq(Scala.Case), Seq(dataDefinition.name), params)
      else
        Scala.Object(name, Seq(Scala.Case), Seq(dataDefinition.name))
    ScalaDefnModuleEntry(sClass)

  override def visitModuleEntry(moduleEntry: ModuleEntry): Seq[ModuleEntry] = preserveHints(moduleEntry) {
    moduleEntry match
      case d@DataDefinition(name, cases) =>
          ScalaDefnModuleEntry(Scala.Trait(name)) +: cases.map(c => translateCaseDefinition(c, d))
      case _ =>
        super.visitModuleEntry(moduleEntry)
  }

  override def visitAtom(atom: Atom): Seq[Atom] = atom match
    case Deconstruct(t, caseName, args) => Seq() // TODO: Fix me
    case _ => super.visitAtom(atom)

  override def visitTerm(term: Term): Seq[Term] = term match
    case Construct(name, args) =>
      val newArgs = args.flatMap(visitTerm)
      val tyName = term.typ match
        case Some(TermType(TData(n), _)) => n
        case Some(TermType(ty, _)) => throw new IllegalArgumentException(s"Unsupported type $ty for constructor $term")
        case _ => throw new IllegalArgumentException(s"Untyped constructor expression $term")
      val constTerm = Scala.Id(name)
      Seq(ScalaTerm(constTerm, ScalaType.named(tyName), newArgs))
    case _ =>
      super.visitTerm(term)