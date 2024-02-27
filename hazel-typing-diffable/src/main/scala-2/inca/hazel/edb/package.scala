package inca.hazel

import truechange.NodeMetaInfo

package object edb {
  val typeAnnoNodes: Seq[NodeMetaInfo] = Seq(
    TypeAnno, TAUnknown, TANum, TABool, TAArrow, TAProd
  )

  val allNodes: Seq[NodeMetaInfo] = typeAnnoNodes // ++ ...
}
