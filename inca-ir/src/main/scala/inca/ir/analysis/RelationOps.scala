package inca.ir.analysis

import sturdy.values.booleans.BooleanOps

trait RelationOps[V, B, RV]:
  type Row = Seq[V]
  type I[Row] <: IterableOnce[Row]
  

  def columns(rv: RV): Seq[String]
  def entries(rv: RV): I[Row]

  def make(cols: Seq[String], vals: Seq[Row]): RV
  
  def rename(rv: RV, subst: Map[String, String]): RV
  def project(rv: RV, cols: Seq[String]): RV
  def projectAndRename(rv: RV, subst: Map[String, String]): RV
  def cartesian(rv: RV, other: RV): RV
  def filter(rv: RV)(f: Row => B): RV
  def map[A](rv: RV)(f: Row => A): I[A]
  def naturalJoin(rv: RV, other: RV): RV
  def antiJoin(rv: RV, other: RV): RV
  def union(rv: RV, other: RV): RV
