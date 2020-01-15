package org.inca.generators.gp.util

import org.eclipse.viatra.query.runtime.matchers.psystem.PBody
import org.inca.lang.core.Content.{IParameter, IPatternBody, TemporaryVariable}
import org.inca.lang.gp.Constraints.PatternCompositionConstraint
import org.inca.lang.gp.Content.GraphPattern

import scala.meta._

object sdkStuff {


  def createDoGetContainedBodies(pattern: GraphPattern): Term.Apply = {
    val graphPatternBodies = createGraphPatternBodies(pattern)
    q"Set(..$graphPatternBodies)"
  }

  def createOverrideFuns(pattern: GraphPattern, collectionName: String): List[Defn.Def] = {

    val pFullyQualifiedName = Lit.String(s"$collectionName.${pattern.name}")
    val pGetFullyQualifiedName = q"override def getFullyQualifiedName(): String = $pFullyQualifiedName"

    val pParamPNames = for (param <- pattern.parameters.toList) yield {
      Term.Name(s"p_${param.name}")
    }
    val pGetParameters = q"override def getParameters(): List[PParameter] = List(..$pParamPNames)"

    val pParamNamesString = for (param <- pattern.parameters.toList) yield {
      Lit.String(param.name)
    }
    val pGetParameterNames = q"override def getParameterNames(): List[String] = List(..$pParamNamesString)"

    List(pGetFullyQualifiedName, pGetParameterNames, pGetParameters)
  }


  private def createGraphPatternBodies(pattern: GraphPattern) = for (body <- pattern.bodies.toList) yield {
    val pPBody = q"val body: PBody = new PBody(this)"

    val tempVars: Map[String, String] = getTemporaryVariables(body)

    val pBody = List(pPBody) ++
      createLocalGlobalVariables(pattern.parameters) ++
      createTemporaryVariables(tempVars) ++
      createTypeConstraints(pattern.parameters)
    q"{ ..$pBody }"
  }

  //  private def createTypeConstraints(temporaryVariables: Map[String, String]) =
  //    for ((name, typ) <- temporaryVariables) yield {
  //      val tempVarTyp = Lit.String(typ.substring(1))
  //      val tempVarName = Term.Name(s"var__$name")
  //
  //      q"TypeConstraint(body, Tuples.flatTupleOf($tempVarName), EdgeKey()"
  //    }
  private def createTypeConstraints(graphParameters: Seq[IParameter]) =

    for (graphParameter <- graphParameters) yield {
      val paramName = Lit.String(graphParameter.typ.get.toString)
      val paramVarName = Term.Name(s"var_${graphParameter.name}")

      val params = List(q"body", q"Tuples.flatTupleOf($paramVarName)", q"new ClassKey($paramName)")

      q"new TypeConstraint(..$params)"
    }

  private def createTemporaryVariables(temporaryVariables: Map[String, String]) =
    for ((name, _) <- temporaryVariables) yield {
      val tempVarValue = Lit.String(name)
      val tempVarName = Pat.Var(Term.Name(s"var__$name"))

      q"val $tempVarName: PVariable = body.getOrCreateVariableByName($tempVarValue)"
    }

  private def createLocalGlobalVariables(graphParameters: Seq[IParameter]): Seq[Defn] =
    for (graphParameter <- graphParameters) yield {
      val paramName = Lit.String(graphParameter.name)
      val param_var_name = Pat.Var(Term.Name(s"var_${graphParameter.name}"))

      q"val $param_var_name: PVariable = body.getOrCreateVariableByName($paramName)"
    }

  def getGeneratedPQuery(graphParameters: Seq[IParameter]): Seq[Defn] =
    for (graphParameter <- graphParameters) yield {
      val pParamString = s"p_${graphParameter.name}"
      val pParamName = Pat.Var(Term.Name(pParamString))
      val pParamNameString = Lit.String(pParamString)
      val pParamFullyQualifiedName = Lit.String(graphParameter.typ.get.toString.substring(1))

      val pConceptKey = q"ConceptKey()"
      val pPParameter = q"new PParameter($pParamNameString, $pParamFullyQualifiedName, $pConceptKey)"
      val pGeneratedPQueryParameter = q"val $pParamName: PParameter = $pPParameter"
      pGeneratedPQueryParameter
    }

  private def getTemporaryVariables(body: IPatternBody): Map[String, String] = body.contents.collect {
    case p: PatternCompositionConstraint => p.call.arguments.collect {
      case t: TemporaryVariable => Map[String, String](t.name -> t.typ.get.toString)
    }
  }.flatten.flatten.toMap


  def createImportStatements(): List[Stat] = {
    val statementsAsStrings = List(
      ImportItem("org.inca.generator.gp", "conceptKeys", List("ClassKey")), // todo change depending on helper classes location
      ImportItem("org.eclipse.viatra.query.runtime", "api", List("ViatraQueryException")),
      // ImportItem("org.eclipse.viatra.query.runtime", "exception", List("_")),
      ImportItem("org.eclipse.viatra.query.runtime.matchers.psystem", "queries", List("PParameter")),
      ImportItem("org.eclipse.viatra.query.runtime.matchers", "psystem", List("PBody", "PVariable")),
      ImportItem("org.eclipse.viatra.query.runtime.matchers", "tuple", List("Tuples")),
      ImportItem("org.eclipse.viatra.query.runtime.matchers.psystem", "queries", List("QueryInitializationException")),
      ImportItem("org.eclipse.viatra.query.runtime.matchers.psystem", "basicdeferred", List("ExportedParameter")),
      ImportItem("org.eclipse.viatra.query.runtime.matchers.psystem", "basicenumerables", List("TypeConstraint")),

    )

    val imports: List[Stat] = for (item <- statementsAsStrings) yield {
      Import(List(
        Importer(Term.Select(Term.Name(item.qualifier), Term.Name(item.name)),
          for (imp <- item.imports) yield {
            Importee.Name(Name.Indeterminate(imp))
          }
        )
      )
      )
    }
    imports
  }

}

