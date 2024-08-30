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

  override def makeRelation(cols: Vector[String], vals: Seq[Vector[Value]]): RelationValue = vals match
    case Seq() => RelationValue(cols, Vector())
    case Seq(vec) => RelationValue(cols, vec)
    case _ => effects.joinFold(vals.map(vec => RelationValue(cols, vec)), identity)

  override def getCols(rel: RelationValue): Vector[String] = rel.columns

  override def scan[A](rel: RelationValue)(f: Vector[Value] => A): Seq[A] = Seq(f(rel.values))

  override def unionFold[A](itr: Iterable[A])(f: A => RelationValue): RelationValue = itr.size match
    case 0 => unit
    case 1 => f(itr.head)
    case _ => effects.joinFold(itr, f)

  def union(rel1: RelationValue, rel2: RelationValue): RelationValue =
    if rel1.isUnitTable then
      rel2
    else if rel2.isUnitTable then
      rel1
    else if rel1.containsColumns(rel2.columns) && rel2.containsColumns(rel1.columns) then
      if rel1.isEmptyTable then
        rel2
      else if rel2.isEmptyTable then
        rel1
      else
        effects.joinComputations(rel1) {
          RelationValue(rel1.columns, rel1.columns.map(s => rel2.getOrElse(s, Top)))
        }
    else
      failure(UnionError, s"${rel1} could not be united with ${rel2}")

  override def projection(rel: RelationValue, cols: Vector[String]): RelationValue =
    if rel.isUnitTable then // unit table
      empty(cols)
    else if rel.isEmptyTable then  // empty Table
      empty(cols)
    // We project a column that does not exist in the relation (happens e.g. when atoms are failing)
    else if !cols.forall(rel.columns.contains) then
      empty(cols)
    else
      val (rCols, rValues) = rel.columns.collect {
        case s if cols.contains(s) => (s, rel(s))
      }.unzip
      RelationValue(rCols, rValues)

  // Returns the rows of the first relation where there exists no match in the second table
  override def antiJoin(rel1: RelationValue, rel2: RelationValue): RelationValue =
    if rel2.isUnitTable then
      rel1
    else
      val eqList = rel2.columns.collect {
        case s if rel2.hasValueForColumn(s) && rel2.hasValueForColumn(s) => (s, eqOps.equ(rel2(s), rel1(s)))
      }
      val topList = eqList.filter((_, b) => b == VBool.Top)
      val containsFalse = eqList.map(_._2).toSet.contains(VBool.False)

      if !containsFalse && topList.isEmpty then
        empty(rel1.columns)
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
    if (rel1.hasColumns && rel1.isEmptyTable) || (rel2.hasColumns && rel2.isEmptyTable) then
      empty(newCols)
    else
      RelationValue(newCols, rel1.values.appendedAll(rel2.values))

  private def equiJoin(rel1: RelationValue, ix1: Vector[String], rel2: RelationValue, ix2: Vector[String]): RelationValue =
    if ix1.length == ix2.length then
      val nonJoinColsRel1 = rel1.columns.filter(s => !ix1.contains(s))
      val nonJoinColsRel2 = rel2.columns.filter(s => !ix2.contains(s))
      val newCols = nonJoinColsRel1.appendedAll(nonJoinColsRel2).appendedAll(ix1)
      if rel1.isEmptyTable || rel2.isEmptyTable then
        empty(newCols)
      else
        val joinPairs = ix1.zip(ix2)
        val eqList = joinPairs.map { (s1, s2) =>
          eqOps.equ(
            rel1.getOrElse(s1, failure(EmptyVariable, s"$s1 is not bound in $rel1")),
            rel2.getOrElse(s2, failure(EmptyVariable, s"$s2 is not bound in $rel2"))
          )
        }
        if eqList.contains(VBool.False) then
          empty(newCols)
        else
          if eqList.contains(VBool.Top) then
            effects.joinWithFailure(identity)(failure(MaybeEquiJoinError, s"Found top while equi join of $rel1 and $rel2"))
          val vals = nonJoinColsRel1.map(rel1.apply)
            .appendedAll(nonJoinColsRel2.map(rel2.apply))
            .appendedAll(joinPairs.zip(eqList).map {
              case ((j, _), VBool.True) => rel1(j)
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
    if rel2.columns.exists(s => !rel1.columns.contains(s)) then
      VBool.False
    else
      val eqRes = rel2.columns.map {
        case s if rel1.hasValueForColumn(s) && rel2.hasValueForColumn(s) => eqOps.equ(rel1(s), rel2(s))
        case _ => VBool.False
      }.toSet
      if eqRes.contains(VBool.False) then
        VBool.False
      else if eqRes.contains(VBool.Top) then
        VBool.Top
      else
        VBool.True

  override def filter(rel: RelationValue, f: Vector[Value] => VBool): RelationValue = f(rel.values) match
    case VBool.True => empty(rel.columns)
    case VBool.False => rel
    case VBool.Top =>
      effects.joinWithFailure(RelationValue(rel.columns, rel.columns.map(_ => Top))) {
        failure(MaybeFilterError, s"filtering $rel with $f may result in an unit Table")
      }

  override def rename(rel: RelationValue, cols: Vector[String], newCols: Vector[String]): RelationValue =
    if rel.isUnitTable then
      unit
    else if cols.length == newCols.length then
      if cols.forall(s => rel.columns.contains(s)) then
        val replacedCols = rel.columns.map(s => if cols.contains(s) then newCols(cols.indexOf(s)) else s)
        RelationValue(replacedCols, rel.values)
      else
        failure(RenameError, s"$rel didn't have columns $cols")
    else
      failure(RenameError, s"${rel.columns} and $cols have not the same length")