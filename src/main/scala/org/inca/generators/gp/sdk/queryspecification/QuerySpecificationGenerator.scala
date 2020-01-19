package org.inca.generators.gp.sdk.queryspecification

import org.inca.generators.gp.sdk.queryspecification.ParentClass.generateParentClass
import org.inca.generators.gp.sdk.queryspecification.ParentObject.generateParentObject
import org.inca.generators.gp.util.ImportItem
import org.inca.generators.gp.util.Util.importToImporter
import org.inca.lang.gp.Content.GraphPattern

import scala.meta._

// todo should this be a class?
object QuerySpecificationGenerator {

  def generateQuerySpeicfication(pattern: GraphPattern, collectionName: String): Source =
    source"""
            ..${createImportStatements()}
            ${generateParentClass(pattern, collectionName)}
            ${generateParentObject(pattern, collectionName)}
          """

  def classTypeName(pattern: GraphPattern, collectionName: String): Type.Name =
    Type.Name(s"${pattern.name}_${collectionName}QuerySpecification")
  def classTermName(pattern: GraphPattern, collectionName: String): Term.Name =
    Term.Name(s"${pattern.name}_${collectionName}QuerySpecification")


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
  }



}
