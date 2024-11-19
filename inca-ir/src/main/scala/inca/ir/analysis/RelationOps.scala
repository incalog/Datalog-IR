package inca.ir.analysis

import sturdy.data.MayJoin
import sturdy.values.booleans.BooleanOps

trait RelationOps[V, B, RV]:
  type Row = Seq[V]
  
  def unit: RV

  def make(cols: Seq[String], vals: Seq[Row]): RV

  def rename(rv: RV, subst: Map[String, String]): RV

  def project(rv: RV, cols: Seq[String]): RV

  def projectAndRename(rv: RV, subst: Map[String, String]): RV

  def cartesian(rv: RV, other: RV): RV

  def filter(rv: RV)(f: Row => B): RV

  def map(rv: RV, columnName: String)(f: Row => V): RV

  def naturalJoin(rv: RV, other: RV): RV

  def antiJoin(rv: RV, other: RV): RV

  def union(rv: RV, other: RV): RV
