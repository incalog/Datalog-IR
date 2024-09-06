package inca.ir.analysis

import inca.ir.analysis.base.effect.Failure
import sturdy.values.booleans.BooleanOps

trait RelationOps[C, V, B, RV]:
  type Row = Seq[V]
  type I[Row] <: IterableOnce[Row]


  def makeColumnName(c: String): C

  def columns(rv: RV): Seq[C]
  def entries(rv: RV): I[Row]

  // Unit
  def make(cols: Seq[C], vals: Seq[Row]): RV
  def unit: RV

  // Empty
  def empty(cols: Seq[C]): RV
  def isEmpty(rv: RV): B

  def rename(rv: RV, subst: Map[C, C]): RV
  def project(rv: RV, cols: Seq[C]): RV
  def projectAndRename(rv: RV, subst: Map[C, C]): RV
  def cartesian(rv: RV, other: RV): RV
  def select(rv: RV)(f: Row => Boolean): RV
  def naturalJoin(rv: RV, other: RV): RV
  def antiJoin(rv: RV, other: RV): RV
  def union(rv: RV, other: RV): RV
