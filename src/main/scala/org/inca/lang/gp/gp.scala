package org.inca.lang

import org.inca.lang.core.Content.IPatternModuleContent
import org.inca.lang.core._

package object gp {
  case class GraphPatternModuleImport(module: IIncaModule) extends AbstractIncaModuleImport
  case class GraphPatternModule(name: String, imports: List[IIncaModuleImport], contents: List[IPatternModuleContent])
    extends IPatternModule
}
