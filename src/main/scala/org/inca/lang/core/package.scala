package org.inca.lang

import org.inca.lang.core.Content.{IGenNameProvider, INamedConcept, IPatternModuleContent}

package object core {
  trait ITypeConstraintProvider
  trait IIncaModule extends INamedConcept {
    val imports: Seq[IIncaModuleImport]
  }
  trait IIncaModuleImport {
    val module: IIncaModule
  }
  trait IPatternModule extends INamedConcept with IGenNameProvider with IIncaModule {
    val contents: Seq[IPatternModuleContent]
  }

  abstract class AbstractIncaModuleImport extends IIncaModuleImport
}
