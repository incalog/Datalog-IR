package inca.ir.datamodel

import inca.ir.Type
import inca.util.TupleOps.transClosure

import scala.collection.immutable.MultiDict

class DataModel(val types: Set[Type], _directSupertypes: MultiDict[Type, Type]) {
  val directNodeSupertypes: MultiDict[Type, Type] = _directSupertypes
  lazy val directNodeSubtypes: MultiDict[Type, Type] = {
    var res = MultiDict[Type, Type]()
    directNodeSupertypes.foreach { case (ty, sty) =>
      res += sty -> ty
    }
    res
  }

  /** maps subtype to supertypes */
  lazy val nodeSupertypes: MultiDict[Type, Type] = transClosure(directNodeSupertypes)
  /** maps supertype to subtypes */
  lazy val nodeSubtypes: MultiDict[Type, Type] = transClosure(directNodeSubtypes)
}
