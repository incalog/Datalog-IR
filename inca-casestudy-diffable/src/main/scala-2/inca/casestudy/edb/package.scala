package inca.casestudy

import truechange.NodeMetaInfo

package object edb {
  val expNodes: Seq[NodeMetaInfo] = Seq(
    Exp, Var, Num, Add
  )

  val stmtNodes: Seq[NodeMetaInfo] = Seq(
    Stmt, Assign, Skip, Sequence
  )

  val allNodes: Seq[NodeMetaInfo] = expNodes ++ stmtNodes
}
