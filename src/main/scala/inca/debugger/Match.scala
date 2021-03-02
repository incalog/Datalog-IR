package inca.debugger

import inca.runtime.Query

case class Match(mat: Query.Match, cols: Seq[Column]) {
  override def toString: String = prettyPrint()

  def prettyPrint(): String = cols.map {
    c: Column => "\"" + c.name + "[" + c.ty + "]" +  "\"=" + c.value.prettyPrint()
  }.mkString("Match { ", ", ", " }")

  def inputs: Seq[Column] = cols.filter(c => c.ty == Input)

  def outputs: Seq[Column] = cols.filter(c => c.ty == Output)

}
