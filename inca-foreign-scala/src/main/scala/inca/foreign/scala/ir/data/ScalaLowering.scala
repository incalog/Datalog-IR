package inca.foreign.scala.ir.data

import inca.foreign.scala.ir.primitive.ScalaInca.cleanName
import inca.ir.{Atom, BaseIR, Eq, ModuleEntry, Name, RefByName, Term, TermArg, TermType, Type, Var, WildcardArg, name2string}
import inca.ir.extension.data
import inca.foreign.scala.ir.primitive.{IR, ScalaConstantTerm, ScalaDefnModuleEntry, ScalaInca, ScalaMonoAggregationOperator, ScalaTerm, ScalaType, ScalaLowering as BaseScalaLowering}
import inca.ir
import inca.ir.Hint.preserveHints
import inca.ir.extension.aggregate.AggregationOperator
import inca.ir.extension.data.{CaseDefinition, Construct, DataDefinition, Deconstruct, TData}
import inca.ir.extension.edbdata.{EdbType, TEdbList, TEdbNode, TEdbValue}
//import inca.ir.extension.edbdata.TEdbNode
import inca.ir.extension.mono.MonoAggregationOperator

trait ScalaLowering extends BaseScalaLowering:
  override def name: String = "DataScalaLowering"

  override def isTypeSupported(ty: Type): Boolean = ty match
    case TData(name) => true
    case _ => super.isTypeSupported(ty)

  var caseDef2params: Map[Name, Seq[(String, Type)]] = Map()

  private def translateCaseDefinition(name: Name, args: Seq[Type], data: TData): ScalaDefnModuleEntry =
    val TData(RefByName(dName)) = data
    val params = caseDef2params(name)
    val paramsCode = params.map {
      case (n, t) => s"$n: ${ScalaInca.compileType(t).name}"
    }.mkString(", ")
    val classCode = s"case class $name($paramsCode) extends ${cleanName(dName)}"
    ScalaDefnModuleEntry(name, classCode)

  override def visitModule(module: ir.Module): ir.Module =
    for (moduleEntry <- module.contents)
      moduleEntry match
        case c@CaseDefinition(name, args, _) =>
          val params = args.zipWithIndex.map { case (ty, idx) =>
            val sty = visitType(ty) match
              case t@ScalaType(_) => t
              case t => t//ScalaInca.compileType(t)
            (s"param_$idx", sty)
          }
          caseDef2params += cleanName(name) -> params
        case _ =>
    super.visitModule(module)

  override def visitModuleEntry(moduleEntry: ModuleEntry): Seq[ModuleEntry] = preserveHints(moduleEntry) {
    moduleEntry match
      case DataDefinition(name) => Seq(ScalaDefnModuleEntry(name, s"trait ${cleanName(name)}"))
      case CaseDefinition(name, args, data) => Seq(translateCaseDefinition(cleanName(name), args, data))
      case _ =>
        super.visitModuleEntry(moduleEntry)
  }

  override def visitAtom(atom: Atom): Seq[Atom] = atom match
    case Deconstruct(term, RefByName(cName), args, true) =>
      val caseName = cleanName(cName)
      val ty = term.typ match
        case Some(TermType(t, _)) => t
        case _ => throw IllegalArgumentException(s"Untyped expression $term")
      val defTy = compileType(ty)
      val caseTy = ScalaType(caseName)
      val isInstanceOfCode = s"(obj: ${defTy.name}) => obj.isInstanceOf[${caseTy.name}]"
      val isInstanceOfCall = ScalaTerm(isInstanceOfCode, ScalaType.bool, visitTerm(term))
      val guard = Eq(ScalaConstantTerm.FALSE, isInstanceOfCall)
      Seq(guard)
    case Deconstruct(term, RefByName(cName), args, false) =>
      val caseName = cleanName(cName)
      val ty = term.typ match
        case Some(TermType(t, _)) => t
        case _ => throw IllegalArgumentException(s"Untyped expression $term")
      val defTy = compileType(ty)
      val caseTy = ScalaType(caseName)

      val paramTys = caseDef2params(caseName)
      val argTerms = args.flatMap(visitArg)
      if (argTerms.size != paramTys.size)
        throw IllegalArgumentException(s"Expected ${paramTys.size} args, but got ${argTerms.size}")

      val isInstanceOfCode = s"(obj: ${defTy.name}) => obj.isInstanceOf[${caseTy.name}]"
      val isInstanceOfCall = ScalaTerm(isInstanceOfCode, ScalaType.bool, visitTerm(term))
      val guard = Eq(ScalaConstantTerm.TRUE, isInstanceOfCall)

      val asInstanceOfCall = s"(obj: ${defTy.name}) => obj.asInstanceOf[${caseTy.name}]"
      val paramReads = paramTys.zip(argTerms).map {
        case ((paramName, pTy), TermArg(t)) =>
          val paramReadCode = s"$asInstanceOfCall.$paramName"
          val paramRead = ScalaTerm(paramReadCode, pTy, visitTerm(term))
          Eq(t, paramRead)
        case ((paramName, pTy), WildcardArg()) =>
          val paramReadCode = s"$asInstanceOfCall.$paramName"
          val paramRead = ScalaTerm(paramReadCode, pTy, visitTerm(term))
          Eq(Var(Name(gensym.fresh("_"))), paramRead)
        case _ => throw IllegalStateException("Found unexpected wildcard! Make sure you called visitTerm")
      }
      guard +: paramReads
    case _ => super.visitAtom(atom)

  override def visitTerm(term: Term): Seq[Term] = term match
    case Construct(RefByName(name), args) =>
      val newArgs = args.flatMap(visitTerm)
      val tyName = term.typ match
        case Some(TermType(TData(RefByName(n)), _)) => cleanName(n)
        case Some(TermType(ty, _)) => throw new IllegalArgumentException(s"Unsupported type $ty for constructor $term")
        case _ => throw new IllegalArgumentException(s"Untyped constructor expression $term")
      Seq(ScalaTerm(cleanName(name), ScalaType(tyName), newArgs))
    case _ =>
      super.visitTerm(term)
