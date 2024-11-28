package inca.ir.analysis

import sturdy.data.MayJoin
import sturdy.values.booleans.BooleanOps

trait RelationOps[V, B, RV]:
  type Row = Seq[V]
  
  def unit: RV

  def isEmpty(rv: RV): B

  def hasColumn(rv: RV, column: String): Boolean

  def columns(rv: RV): Seq[String]

  def columnIndex(rv: RV, column: String): Int =
    columns(rv).indexOf(column)
    
  // might produce empty table
  def make(cols: Seq[String], vals: Seq[Row]): RV
  
  def rename(rv: RV, subst: Map[String, String]): RV

  def project(rv: RV, cols: Seq[String]): RV

  def projectAndRename(rv: RV, subst: Map[String, String]): RV

  def map(rv: RV, columnName: String)(f: Row => V): RV

  // might produce empty table
  def filter(rv: RV)(f: Row => B): RV

  def filterNot(rv: RV)(f: Row => B): RV

  def foreach(rv: RV)(f: Row => Unit): Unit

  def cartesian(rv: RV, other: RV): RV

  def union(rv: RV, other: RV): RV

  // might produce empty table
  def naturalJoin(rv: RV, other: RV): RV

  // might produce empty table
  def antiJoin(rv: RV, other: RV): RV
  
  def copyColumn(rv: RV, from: String, to: String): RV =
    val fromIx = columnIndex(rv, from)
    map(rv, to) { row => row(fromIx) }
    