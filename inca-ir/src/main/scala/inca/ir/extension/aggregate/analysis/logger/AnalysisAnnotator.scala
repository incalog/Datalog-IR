package inca.ir.extension.aggregate.analysis.logger

import inca.ir.*
import inca.ir.extension.aggregate.{Aggregate, AggregateColumnArg}
import inca.ir.analysis.base.logger.BaseAnalysisAnnotator

trait AnalysisAnnotator[V, RV, TV] extends BaseAnalysisAnnotator[V, RV, TV]