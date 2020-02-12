package org.inca.gen.gp.sdk.queryspecification

import GeneratedQueryClass.generateParentClass
import GeneratedQueryObject.generateParentObject
import org.inca.gen.gp.util.ImportItem
import org.inca.gen.gp.util.Util.importToImporter
import org.inca.lang.gp.Content.GraphPattern

import scala.meta._

object QuerySpecificationGenerator {

  def generateQuerySpeicfication(pattern: GraphPattern): Source =
    source"""
            ..${createImportStatements()}
            ${generateParentClass(pattern)}
            ${generateParentObject(pattern)}
          """

  // todo fetch them dynamically and move this to a pipeline step maybe
  def createImportStatements(): List[Stat] = {
    val importStatements = List(
      ImportItem("org.inca.generators.gp.model.conceptKeys", List("ClassKey", "PlaceholderConceptKey")), // todo change depending on helper classes location
      ImportItem("org.inca.generators.gp", List("AbstractPQuery", "ScalaPatternMatcher", "ScalaQuerySpecification")), // todo change depending on helper classes location
      //      ImportItem("org.eclipse.viatra.query.runtime.api", List("ViatraQueryException")),
      ImportItem("org.eclipse.viatra.query.runtime.api", List("ViatraQueryEngine")),
      ImportItem("org.eclipse.viatra.query.runtime.api.scope", List("QueryScope")),
      // ImportItem("org.eclipse.viatra.query.runtime", "exception", List("_")),
      ImportItem("org.eclipse.viatra.query.runtime.matchers.psystem.queries", List("PParameter")),
      ImportItem("org.eclipse.viatra.query.runtime.matchers.psystem", List("PBody", "PVariable")),
      ImportItem("org.eclipse.viatra.query.runtime.matchers.tuple", List("Tuples")),
      ImportItem("org.eclipse.viatra.query.runtime.matchers.psystem.queries", List("QueryInitializationException")),
      ImportItem("org.eclipse.viatra.query.runtime.matchers.psystem.basicdeferred", List("ExportedParameter")),
      ImportItem("org.eclipse.viatra.query.runtime.matchers.psystem.basicenumerables", List("TypeConstraint")),

    )
    importToImporter(importStatements)

    List()
  }

}
