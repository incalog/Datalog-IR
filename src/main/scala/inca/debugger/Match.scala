package inca.debugger

import inca.runtime.Query

case class Match(mat: Query.Match, params: Seq[Parameter]) { // TODO ASK out only 1? mat unneeded

  override def toString: String = toString(true)

  def toString(withTag: Boolean): String = params.map {
    p: Parameter => "\"" + p.name + /*"[" + p.io + "]" + */ "\"=" + p.value.toString(withTag)
  }.mkString("Match { ", ", ", " }")

  def inputs: Seq[Parameter] = params.filter(p => p.io == InputParam)

  def outputs: Seq[Parameter] = params.filter(p => p.io == OutputParam)

}
