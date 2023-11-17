package inca.foreign.scala.ir.data

import inca.ir.{Atom, BaseIR, Eq, ExtensionalRelation, ModuleEntry, Name, Relation, Term, TermType, Type, name2string}
import inca.ir.lowering.BaseLowering
import inca.ir.extension.block
import inca.ir.extension.data
import inca.foreign.scala.ir.primitive.{IR, ScalaConstantTerm, ScalaDefnModuleEntry, ScalaInca, ScalaTerm, ScalaType, ScalaLowering as BaseScalaLowering}
import inca.ir.Hint.preserveHints
import inca.ir.extension.arithmetic.TInt
import inca.ir.extension.data.{CaseDefinition, Construct, DataDefinition, Deconstruct, TData}
import inca.ir.extension.string.TString

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
    case TString => true
    case TInt => true
    case _ => false

  var caseDef2params: Map[Name, Seq[(String, ScalaType)]] = Map()

  private def translateCaseDefinition(caseDef: CaseDefinition, dataDef: DataDefinition): ScalaDefnModuleEntry =
    val CaseDefinition(cName, tys) = caseDef
    val DataDefinition(dName, _) = dataDef
    val params = tys.zipWithIndex.map { case (ty, idx) =>
        val sty = visitType(ty) match
          case t@ScalaType(_) => t
          case t => throw IllegalArgumentException(s"Expected ScalaType, but got $t")
        (s"param_$idx", sty)
    }

    caseDef2params += cName -> params

    val paramsCode = params.map { case (n, t) => s"$n: ${t.name}" }.mkString(", ")
    val classCode = s"case class $cName($paramsCode) extends $dName"
    ScalaDefnModuleEntry(cName, classCode)

  override def visitModuleEntry(moduleEntry: ModuleEntry): Seq[ModuleEntry] = preserveHints(moduleEntry) {
    moduleEntry match
      case d@DataDefinition(name, cases) =>
          ScalaDefnModuleEntry(name, s"trait $name") +: cases.map(c => translateCaseDefinition(c, d))
      case _ =>
        super.visitModuleEntry(moduleEntry)
  }

  override def visitAtom(atom: Atom): Seq[Atom] = atom match
    case Deconstruct(term, caseName, args) =>
      val ty = term.typ match
        case Some(TermType(t, _)) => t
        case _ => throw IllegalArgumentException(s"Untyped expression $term")
      val defTy = compileType(ty)
      val caseTy = ScalaType(caseName)

      val paramTys = caseDef2params(caseName)
      val argTerms = args.flatMap(visitTerm)
      if (argTerms.size != paramTys.size)
        throw IllegalArgumentException(s"Expected ${paramTys.size} args, but got ${argTerms.size}")

      val isInstanceOfCode = s"(obj: ${defTy.name}) => obj.isInstanceOf[${caseTy.name}]"
      val isInstanceOfCall = ScalaTerm(isInstanceOfCode, ScalaType.bool, visitTerm(term))
      val guard = Eq(ScalaConstantTerm.TRUE, isInstanceOfCall)

      val asInstanceOfCall = s"(obj: ${defTy.name}) => obj.asInstanceOf[${caseTy.name}]"
      val paramReads = paramTys.zip(argTerms).map { case ((paramName, pTy), t) =>
        val paramReadCode = s"$asInstanceOfCall.$paramName"
        val paramRead = ScalaTerm(paramReadCode, pTy, visitTerm(term))
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
      Seq(ScalaTerm(name, ScalaType(tyName), newArgs))
    case _ =>
      super.visitTerm(term)