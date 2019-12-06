package org.inca

import org.inca.core.{AbstractIncaModuleImport, IIncaModule, IIncaModuleImport, IPatternModule}
import org.inca.core.content.IPatternModuleContent

package object gp {
  case class GraphPatternModuleImport(module: IIncaModule) extends AbstractIncaModuleImport
  case class GraphPatternModule(name: String, imports: List[IIncaModuleImport], contents: List[IPatternModuleContent])
    extends IPatternModule
}
