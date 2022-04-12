package inca.frontend.souffle.compiler

import inca.backend.ir.Datalog._
import inca.util.Scala

object PropagateUnbounded {

  def transformModule(module: Module): Module =  {
    // first propagate unbounded of computed
    var pats = module.pats.map(TrackComputedUnbounded.transformPattern(_)(Map()))

    // second propagate unbounded of calls until fixed point is reached
    def fixStep(pats: Seq[Pattern]): Seq[Pattern] = {
      val patEnv = pats.map { p => p.name -> p }.toMap
      pats.map(TrackCallUnbounded.transformPattern(_)(patEnv))
    }
    var newPats = fixStep(pats)
    while (pats != newPats) {
      pats = newPats
      newPats = fixStep(pats)
    }
    Module(module.name, module.imports, pats, module.scalaContent)
  }
}

object TrackCallUnbounded extends TrackUnbounded {
  override def transformCall(call: Call, seen: Set[Term])(implicit pats: PatEnv): Set[Term] = {
    val pat = pats(call.name)
    val unboundedIndices = pat.params.zipWithIndex.collect { case (Param(_, TScala(_)), i) => i }
    seen ++ unboundedIndices.map(call.args)
  }
}

object TrackComputedUnbounded extends TrackUnbounded {

  override def transformComputed(computed: Computed, seen: Set[Term])(implicit pats:  PatEnv): Set[Term] =
    if (returnsUnbound(computed.computation))
      Set(computed.lhs)
    else
      Set()

  def returnsUnbound(computation: Computation): Boolean = computation match {
    case Evaluation(_, resultType, _) => isUnboundType(resultType)
    case CountAggregation(_, _) => true
    case CustomAggregation(typ, _, _, _, _, _) => isUnboundType(typ)
  }

  def isUnboundType(typeAnno: Type): Boolean = typeAnno match {
    case TScala(_) => true
    case _ => false
  }
}

trait TrackUnbounded {

  type PatEnv = Map[String, Pattern]

  def transformPattern(pat: Pattern)(implicit pats: PatEnv): Pattern = {
    // collect unbound vars
    val unbounded = pat.bodies.map(transformBody)
    // set param to unbound if it occurs in unbound vars
    val unboundedNames = unbounded.flatten.distinct.collect { case Var(name) => name }
    val updatedParams = pat.params.map { p =>
      if (unboundedNames.contains(p.name)) {
        // avoid nesting TUnbounded
        val ty = p.typ match {
          case TScala(_) => p.typ
          case _ => TScala(Scala(p.typ.asScala))
        }
        Param(p.name, ty)
      } else p
    }
    Pattern(pat.vis, pat.name, updatedParams, pat.bodies)
  }

  def transformBody(body: Body)(implicit pats: PatEnv): Set[Term] = {
    body.atoms.foldLeft(Set[Term]()) { case (res, atom) =>
      res ++ transformAtom(atom, res)
    }
  }

  def transformAtom(atom: Atom, seen: Set[Term])(implicit pats: PatEnv): Set[Term] = atom match {
    case c: Compare => transformCompare(c, seen)
    case c:Call => transformCall(c, seen)
    case ht: HasType => transformHasType(ht, seen)
    case nht: NotHasType => transformNotHasType(nht, seen)
    case p: Path => transformPath(p, seen)
    case np: NoPath => transformNoPath(np, seen)
    case c: Computed => transformComputed(c, seen)
  }

  // essential to collect every alias for an unbounded var
  def transformCompare(compare: Compare, seen: Set[Term])(implicit pats: PatEnv): Set[Term] = {
    val rhsUnbounded = if (seen.contains(compare.lhs)) Some(compare.rhs) else None
    val lhsUnbounded = if (seen.contains(compare.rhs)) Some(compare.lhs) else None
    seen ++ rhsUnbounded ++ lhsUnbounded
  }
  def transformCall(call: Call, seen: Set[Term])(implicit pats: PatEnv): Set[Term] = Set()
  def transformHasType(hasType: HasType, seen: Set[Term])(implicit pats: PatEnv): Set[Term] = Set()
  def transformNotHasType(notHasType: NotHasType, seen: Set[Term])(implicit pats: PatEnv): Set[Term] = Set()
  def transformPath(path: Path, seen: Set[Term])(implicit pats: PatEnv): Set[Term] = Set()
  def transformNoPath(noPath: NoPath, seen: Set[Term])(implicit pats: PatEnv): Set[Term] = Set()
  def transformComputed(computed: Computed, seen: Set[Term])(implicit pats: PatEnv): Set[Term] = Set()

}