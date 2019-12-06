package org.inca.core.constraints

import org.inca.core.ITypeConstraintProvider
import org.inca.core.content.IPatternBodyContent
import org.inca.core.values.IValue

abstract class CompareConstraint(feature: CompareFeature,left: IValue, right: IValue)
  extends IPatternBodyContent
    with ITypeConstraintProvider
