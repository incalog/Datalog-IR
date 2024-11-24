package inca.ir.valueNumbering

import inca.ir.Term

import scala.collection.mutable



trait VNAnalysisResults {

  var VNs: ValueIds[Term] = ValueIds[Term]()
  var congruenceClasses: mutable.Map[ValueId, CongruenceClass] = mutable.Map()

}
