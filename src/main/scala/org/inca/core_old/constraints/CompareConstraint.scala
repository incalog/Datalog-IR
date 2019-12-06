package org.inca.core_old.constraints

import org.inca.core_old.ITypeConstraintProvider
import org.inca.core_old.content.IPatternBodyContent
import org.inca.core_old.values.IValue

abstract class CompareConstraint(feature: CompareFeature,left: IValue, right: IValue)
  extends IPatternBodyContent
    with ITypeConstraintProvider
