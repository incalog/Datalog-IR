package inca.casestudy.interval

import truechange.NodeMetaInfo

package object edb {
  val expNodes: Seq[NodeMetaInfo] = Seq(
    Exp, Var, Num, Add, GT
  )

  val stmtNodes: Seq[NodeMetaInfo] = Seq(
    Stmt, Assign, Skip, Sequence, While, Exit
  )

  val allNodes: Seq[NodeMetaInfo] = expNodes ++ stmtNodes
}
