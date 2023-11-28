package inca.ir.extension.tuple

import inca.ir.*
import inca.ir.Hint.preserveHints
import inca.ir.extension.tuple.{IR, Project, TTuple, TupleLit}
import inca.ir.lowering.BaseLowering
import inca.ir.{name2string, string2name}

import scala.collection.immutable.{AbstractSeq, LinearSeq}

trait Lowering extends BaseLowering:

  override val loweredIRs: Set[BaseIR] = Set(IR)
  override val requiredIRs: Set[BaseIR] = Set()

  /** Remember for each variable how we flattened it */
  private var cachedFlatten: Map[(Name,Type), Seq[(Name, Type)]] = Map()

  private def flatten(name: Name, typ: Type): Seq[(Name, Type)] =
    cachedFlatten.get((name, typ)) match
      case Some(cached) =>
        cached
      case None =>
        val res = typ match
          case TTuple(tys) => tys.zipWithIndex.flatMap { case (ty, ix) =>
            flatten(gensym.fresh(name), visitType(ty))
          }
          case _ => Seq((name, visitType(typ)))
        cachedFlatten += (name,typ) -> res
        res

  private def flatten(param: Param): Seq[Param] =
    flatten(param.name, param.ty).map { case (n, t) => Param(n, t) }

  override def visitExtensionalRelation(relation: ExtensionalRelation): Seq[ExtensionalRelation] =
    // Reset the cache of flattened variables
    cachedFlatten = Map()
    super.visitExtensionalRelation(relation)

  override def visitRelation(relation: Relation): Seq[Relation] =
    // Reset the cache of flattened variables
    cachedFlatten = Map()
    super.visitRelation(relation)

  override def visitBody(body: Body): Seq[Body] =
    // save the flattened params
    val flattenParams = cachedFlatten
    val res = super.visitBody(body)
    // forget all flattened vars of this body, but remember the flattened params
    cachedFlatten = flattenParams
    res

  override def visitParam(param: Param): Seq[Param] = preserveHints(param)(flatten(param))

  override def visitArg(arg: Arg): Seq[Arg] = arg match
    case wildcard@WildcardArg() => wildcard.typ match
      case Some(TermType(ty, _)) => 0.until(ty.size).map(_ => WildcardArg())
      case _ => throw IllegalStateException(s"Untyped argument $arg")
    case _ => super.visitArg(arg)

  override def visitTerm(term: Term): Seq[Term] = preserveHints(term) {
    term match
      case Cast(t, ty) =>
        val terms = visitTerm(t)
        val ttys = visitType(ty).flatten
        terms.zip(ttys).map((t, ty) => Cast(t, ty))
      case Project(t, idx) =>
        val tupleTy: TTuple = t.typ match {
          case Some(TermType(ty@TTuple(tys), _)) if idx <= tys.size =>
            ty
          case Some(TermType(ty@TTuple(tys), _)) if idx > tys.size =>
            throw IndexOutOfBoundsException(s"Projection index $idx out of bounds!")
          case Some(TermType(ty,_)) =>
            throw IllegalStateException(s"Term $t has type $ty, but expected TTuple.")
          case None =>
            throw IllegalStateException(s"Untyped term $t")
        }
        val endIndex = idx + tupleTy.tys(idx).size
        visitTerm(t) match {
          case ts: Seq[Term] if endIndex <= ts.size =>
            ts.slice(idx, endIndex)
          case ts: Seq[Term] if endIndex > ts.size =>
            throw IndexOutOfBoundsException(s"Projection index range ($idx, $endIndex) out of bounds!")
          case _ =>
            throw IllegalStateException(s"Can not project unknown term: $term")
        }
      case TupleLit(ts) =>
        ts.flatMap(visitTerm)
      case Var(name) =>
        val vars = flatten(name, term.typ.getOrElse(throw IllegalArgumentException(s"Untyped term $term")).ty)
        vars.map { case (n, _) => Var(n) }
      case _ => super.visitTerm(term)
  }
