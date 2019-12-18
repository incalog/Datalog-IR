package org.inca

import org.inca.lang.core.Content.IPatternModuleContent
import org.inca.lang.core.{AbstractIncaModuleImport, IIncaModule, IIncaModuleImport, IPatternModule}

package object gp {
  case class GraphPatternModuleImport(module: IIncaModule) extends AbstractIncaModuleImport
  case class GraphPatternModule(name: String, imports: List[IIncaModuleImport], contents: List[IPatternModuleContent])
    extends IPatternModule
}
