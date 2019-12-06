package org.inca.core.constraints

// todo how to implement enums?
trait CompareFeature
case class EqualityCompareFeature() extends CompareFeature
case class InequalityCompareFeature() extends CompareFeature