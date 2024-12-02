package inca.ir.valueNumbering

import inca.ir.{Name, Term}

import scala.collection.mutable



trait VNAnalysisResults {
  var VNs: ValueIds[Term] = ValueIds[Term]()
  var congruenceClasses: CongrClassesTable[Term] = CongrClassesTable[Term]()
}


trait VNAnalysisResultsParams {
  type ParamName = Name
  var paramLeaders: Map[ParamName, Term] = Map()
}
