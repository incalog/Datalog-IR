package inca.ir.analysis.base.values

import inca.ir.analysis.RelationOps
import inca.ir.analysis.base.ordering.BaseEqOps
import inca.ir.analysis.base.effect.Failure.{ AntiJoinError, UnionError, EquiJoinError, EmptyVariable, MaybeEquiJoinError, MaybeFilterError, RenameError }
import inca.ir.extension.arithmetic.analysis.ordering.EqOps
import sturdy.effect.EffectStack
import sturdy.effect.failure.Failure
import sturdy.values.Join

class RelationValueOps(using effects: EffectStack, j: Join[RelationValue], eqOps: BaseEqOps, failure: Failure) 
  extends RelationOps[Value, RelationValue, VBool, String]:

  override def makeRelation(cols: Vector[String], vals: Seq[Vector[Value]]): RelationValue =
    if vals.isEmpty then
      empty
    else if vals.length < 2 then
      RelationValue(cols, vals.head)
    else
      effects.joinFold(vals.map(vec => RelationValue(cols, vec)), identity)

  override def getCols(rel: RelationValue): Vector[String] = rel.columns

  override def scan[A](rel: RelationValue)(f: Vector[Value] => A): Seq[A] = Seq(f(rel.values))

  override def unionFold[A](itr: Iterable[A])(f: A => RelationValue): RelationValue = itr.size match
    case 0 => empty
    case 1 => f(itr.head)
    case _ => effects.joinFold(itr, f)

  def union(rel1: RelationValue, rel2: RelationValue): RelationValue =
    if rel1.hasNoColumns then
      rel2
    else if rel2.hasNoColumns then
      rel1
    else if rel1.containsColumns(rel2.columns) && rel2.containsColumns(rel1.columns) then
      if rel1.hasNoValues then
        rel2
      else if rel2.hasNoValues then
        rel1
      else
        val rel2Map = rel2.valueMap
        effects.joinComputations(rel1) {
          RelationValue(rel1.columns, rel1.columns.map(s => rel2Map.getOrElse(s, Top)))
        }
    else
      failure(UnionError, s"${rel1} could not be united with ${rel2}")

  override def projection(rel: RelationValue, cols: Vector[String]): RelationValue =
    if rel.hasNoColumns then
      RelationValue(cols, Vector())
    else if rel.hasNoValues then
      RelationValue(cols, Vector())
    // We project a column that does not exist in the relation (happens e.g. when atoms are failing)
    else if !cols.forall(s => rel.columns.contains(s)) then
      RelationValue(cols, Vector())
    else
      val (rCols, rValues) = rel.valueMap.collect {
        case (s, v) if cols.contains(s) => (s, v)
      }.unzip
      RelationValue(rCols.toVector, rValues.toVector)

  // Returns the rows of the first relation where there exists no match in the second table
  override def antiJoin(rel1: RelationValue, rel2: RelationValue): RelationValue =
    if rel2.hasNoColumns then
      rel1
    else
      val rel1Map = rel1.valueMap
      val rel2Map = rel2.valueMap
      val filtered = rel2Map.filter((s, v) => rel1Map.contains(s))
      val eqList = filtered.map((s, v) => (s, eqOps.equ(v, rel1Map(s))))
      val topList = eqList.filter((_, b) => b == VBool.Top)
      val containsFalse = eqList.values.toSet.contains(VBool.False)

      if !containsFalse && topList.isEmpty then
        RelationValue(rel1.columns, Vector())
      else if !containsFalse then
        topList.foreach {
          case (s, _) => effects.joinWithFailure(identity) {
            failure(AntiJoinError, s"Comparing $s while anti joining $rel1 and $rel2 yields top")
          }
        }
        rel1
      else
        rel1

  private def crossProduct(rel1: RelationValue, rel2: RelationValue): RelationValue =
    val newCols = rel1.columns.appendedAll(rel2.columns.map(s => if rel1.columns.contains(s) then "rel2." + s else s))
    if (rel1.hasColumns && rel1.hasNoValues) || (rel2.hasColumns && rel2.hasNoValues) then
      RelationValue(newCols, Vector())
    else
      RelationValue(newCols, rel1.values.appendedAll(rel2.values))

  private def equiJoin(rel1: RelationValue, ix1: Vector[String], rel2: RelationValue, ix2: Vector[String]): RelationValue =
    if ix1.length == ix2.length then
      val nonJoinColsRel1 = rel1.columns.filter(s => !ix1.contains(s))
      val nonJoinColsRel2 = rel2.columns.filter(s => !ix2.contains(s))
      val newCols = nonJoinColsRel1.appendedAll(nonJoinColsRel2).appendedAll(ix1)
      if rel1.hasNoValues || rel2.hasNoValues then
        RelationValue(newCols, Vector())
      else
        val rel1Map = rel1.valueMap
        val rel2Map = rel2.valueMap
        val joinPairs = ix1.zip(ix2)
        val eqList = joinPairs.map { (s1, s2) => 
          eqOps.equ(
            rel1Map.getOrElse(s1, failure(EmptyVariable, s"$s1 is not bound in $rel1")), 
            rel2Map.getOrElse(s2, failure(EmptyVariable, s"$s2 is not bound in $rel2"))
          )
        }.toSet
        if eqList.contains(VBool.False) then
          RelationValue(newCols, Vector())
        else 
          if eqList.contains(VBool.Top) then
            effects.joinWithFailure(identity)(failure(MaybeEquiJoinError, s"Found top while equi join of $rel1 and $rel2"))
          val vals = nonJoinColsRel1.map(rel1Map)
            .appendedAll(nonJoinColsRel2.map(rel2Map))
            .appendedAll(joinPairs.zip(eqList).map {
              case (j, b) if b == VBool.True => rel1Map(j._1)
              case _ => Top
            })
          RelationValue(newCols, vals)
      else
        failure(EquiJoinError, s"Illegal equiJoin between $rel1 and $rel2 with $ix1 and $ix2")
  
  override def natJoin(rel1: RelationValue, rel2: RelationValue): RelationValue =
    val commonCols = getCols(rel1).filter(c => getCols(rel2).contains(c))
    if commonCols.isEmpty then
      crossProduct(rel1, rel2)
    else
      equiJoin(rel1, commonCols, rel2, commonCols)

  override def subset(rel1: RelationValue, rel2: RelationValue): VBool =
    val rel1Map = rel1.valueMap
    val rel2Map = rel2.valueMap
    if !rel2Map.keys.map(s => rel1Map.contains(s)).forall(identity) then
      VBool.False
    else
      val eqRes = rel2Map.map((s, v) => eqOps.equ(rel1Map(s), v)).toSet
      if eqRes.contains(VBool.False) then
        VBool.False
      else if eqRes.contains(VBool.Top) then
        VBool.Top
      else
        VBool.True

  override def filter(rel: RelationValue, f: Vector[Value] => VBool): RelationValue = f(rel.values) match
    case VBool.True => RelationValue(rel.columns, Vector())
    case VBool.False => rel
    case VBool.Top =>
      effects.joinWithFailure(RelationValue(rel.columns, rel.columns.map(_ => Top))) {
        failure(MaybeFilterError, s"filtering $rel with $f may result in an empty Table")
      }

  override def rename(rel: RelationValue, cols: Vector[String], newCols: Vector[String]): RelationValue =
    if rel.hasNoColumns then
      empty
    else if cols.length == newCols.length then
      if cols.forall(s => rel.columns.contains(s)) then
        val replacedCols = rel.columns.map(s => if cols.contains(s) then newCols(cols.indexOf(s)) else s)
        RelationValue(replacedCols, rel.values)
      else
        failure(RenameError, s"$rel didn't have columns $cols")
    else
      failure(RenameError, s"${rel.columns} and $cols have not the same length")