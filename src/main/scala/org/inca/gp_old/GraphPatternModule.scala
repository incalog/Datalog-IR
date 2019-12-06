package org.inca.gp_old

import org.inca.core.content.IPatternModuleContent
import org.inca.core.{IIncaModuleImport, IPatternModule}

case class GraphPatternModule(contents: List[IPatternModuleContent],
                              imports: List[IIncaModuleImport],
                              name: String)
  extends IPatternModule
