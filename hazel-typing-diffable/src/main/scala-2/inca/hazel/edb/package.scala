package inca.hazel

import truechange.NodeMetaInfo

package object edb {
  val typeAnnoNodes: Seq[NodeMetaInfo] = Seq(
    TypeAnno,
    TAUnknown,
    TANum,
    TABool,
    TAArrow,
    TAProd
  )

  val expNodes: Seq[NodeMetaInfo] = Seq(
    Exp,
    EHole,
    EVar,
    ELam,
    EAp,
    ELet,
    ENum,
    EPlus,
    ETrue,
    EFalse,
    EIf,
    EPair,
    EProjL,
    EProjR
  )

  val allNodes: Seq[NodeMetaInfo] = typeAnnoNodes ++ expNodes
}
