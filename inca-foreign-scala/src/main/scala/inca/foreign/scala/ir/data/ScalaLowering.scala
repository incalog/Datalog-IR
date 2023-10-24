package inca.foreign.scala.ir.data

import inca.foreign.scala.ir.BaseScalaLowering
import inca.ir.{Atom, BaseIR, ExtensionalRelation, ModuleEntry, Name, Relation, Term, TermType, Type, Eq, name2string}
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

  var caseDef2params: Map[Name, Seq[Scala.Param]] = Map()

  private def translateCaseDefinition(caseDef: CaseDefinition, dataDefinition: DataDefinition): ScalaDefnModuleEntry =
    val CaseDefinition(name, tys) = caseDef
    val params = tys.zipWithIndex.map {
      case (ty, idx) => Scala.Param(s"param_$idx", compileType(ty).ty)
    }

    caseDef2params += name -> params

    val sClass =
        Scala.Class(name, Seq(Scala.Case), Seq(dataDefinition.name), params)
//        Scala.Object(name, Seq(Scala.Case), Seq(dataDefinition.name))
    ScalaDefnModuleEntry(sClass)

  override def visitModuleEntry(moduleEntry: ModuleEntry): Seq[ModuleEntry] = preserveHints(moduleEntry) {
    moduleEntry match
      case d@DataDefinition(name, cases) =>
          ScalaDefnModuleEntry(Scala.Trait(name)) +: cases.map(c => translateCaseDefinition(c, d))
      case _ =>
        super.visitModuleEntry(moduleEntry)
  }

  override def visitAtom(atom: Atom): Seq[Atom] = atom match
    case Deconstruct(term, caseName, args) =>
      val ty = term.typ match
        case Some(TermType(t, _)) => t
        case _ => throw IllegalArgumentException(s"Untyped expression $term")
      val caseTy = Scala.TypeName(caseName)

      val paramTys = caseDef2params(caseName)
      val argTerms = args.flatMap(visitTerm)
      if (argTerms.size != paramTys.size)
        throw IllegalArgumentException(s"Expected ${paramTys.size} args, but got ${argTerms.size}")

      // TODO: Fix instanceOf stuff
      val constTrue = ScalaTerm(Scala.BoolLiteral(true), ScalaType.bool, Seq())
      val isInstanceOfCall = ScalaTerm(
        Scala.Lam(
          Seq(Scala.Param("obj", compileType(ty).ty)),
          Scala.Select(Scala.Id("obj"), s"isInstanceOf[$caseName]")
        ),
        ScalaType.bool,
        visitTerm(term)
      )
      val guard = Eq(constTrue, isInstanceOfCall)

      val asInstanceOfCall = Scala.Select(Scala.Id("obj"), s"asInstanceOf[$caseName]")
      val paramReads = paramTys.zip(argTerms).map { case (Scala.Param(pName, pTy), t) =>
        val paramRead = ScalaTerm(
            Scala.Lam(
              Seq(Scala.Param("obj", compileType(ty).ty)),
              Scala.Select(asInstanceOfCall, pName)
            ),
            ScalaType(pTy),
            visitTerm(term)
          )
        Eq(t, paramRead)
      }
      guard +: paramReads
    case _ => super.visitAtom(atom)

  override def visitTerm(term: Term): Seq[Term] = term match
    case Construct(name, args) =>
      val newArgs = args.flatMap(visitTerm)
      val tyName = term.typ match
        case Some(TermType(TData(n), _)) => n
        case Some(TermType(ty, _)) => throw new IllegalArgumentException(s"Unsupported type $ty for constructor $term")
        case _ => throw new IllegalArgumentException(s"Untyped constructor expression $term")
      val constructTerm = Scala.Id(name)
      Seq(ScalaTerm(constructTerm, ScalaType.named(tyName), newArgs))
    case _ =>
      super.visitTerm(term)