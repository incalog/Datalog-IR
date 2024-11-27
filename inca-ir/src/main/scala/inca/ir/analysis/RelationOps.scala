package inca.ir.analysis

import sturdy.data.MayJoin
import sturdy.values.booleans.BooleanOps

trait RelationOps[V, B, RV]:
  type Row = Seq[V]
  
  def unit: RV
  
  // might produce empty table
  def make(cols: Seq[String], vals: Seq[Row]): RV
  
  def rename(rv: RV, subst: Map[String, String]): RV

  def project(rv: RV, cols: Seq[String]): RV
  
  def projectAndRename(rv: RV, subst: Map[String, String]): RV

  def cartesian(rv: RV, other: RV): RV

  // might produce empty table
  def filter(rv: RV)(f: Row => B): RV

  def map(rv: RV, columnName: String)(f: Row => V): RV

  def foreach(rv: RV)(f: Row => Unit): Unit

  def naturalJoin(rv: RV, other: RV): RV

  // might produce empty table
  def antiJoin(rv: RV, other: RV): RV

  def union(rv: RV, other: RV): RV
  
  def isEmpty(rv: RV): B

  def hasColumn(rv: RV, column: String): B