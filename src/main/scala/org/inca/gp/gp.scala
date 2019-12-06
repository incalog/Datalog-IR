package org.inca

import org.inca.core_old.{AbstractIncaModuleImport, IIncaModule, IIncaModuleImport, IPatternModule}
import org.inca.core_old.content.IPatternModuleContent

package object gp {
  case class GraphPatternModuleImport(module: IIncaModule) extends AbstractIncaModuleImport
  case class GraphPatternModule(name: String, imports: List[IIncaModuleImport], contents: List[IPatternModuleContent])
    extends IPatternModule
}
