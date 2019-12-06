package org.inca.gp_old

import org.inca.core_old.content.IPatternModuleContent
import org.inca.core_old.{IIncaModuleImport, IPatternModule}

case class GraphPatternModule(contents: List[IPatternModuleContent],
                              imports: List[IIncaModuleImport],
                              name: String)
  extends IPatternModule
