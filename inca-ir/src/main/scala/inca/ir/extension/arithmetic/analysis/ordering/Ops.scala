package inca.ir.extension.arithmetic.analysis.ordering

import inca.ir.analysis.base.values.{VBool, Value}
import inca.ir.extension.arithmetic.analysis.values.{asDouble, asInt}
import sturdy.values.ordering.LiftedOrderingOps
import sturdy.values.integer.given_OrderingOps_Int_Boolean
import sturdy.values.floating.given_OrderingOps_Double_Boolean

class IntVOrderingOps extends LiftedOrderingOps[Value, VBool, Int, Boolean](_.asInt, VBool.apply)
class DoubleVOrderingOps extends LiftedOrderingOps[Value, VBool, Double, Boolean](_.asDouble, VBool.apply)
