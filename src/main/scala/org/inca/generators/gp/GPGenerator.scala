package org.inca.generators.gp

import org.inca.lang.core.Content.{IParameter, IPatternBody, TemporaryVariable}
import org.inca.lang.gp.Constraints.PatternCompositionConstraint
import org.inca.lang.gp.Content.GraphPattern

import scala.meta._


class GPGenerator {
  val filename = "test.scala"

  /**
   * Generates a ast with quasiquotes of scalameta from a IncA graph pattern
   * @param pattern        GraphPattern which will be transformed into scalameta tree
   * @param collectionName Name of the file (mps) where pattern is saved
   * @return
   */
  def generate(pattern: GraphPattern, collectionName: String): Defn.Class = {
    val className = Type.Name(s"${pattern.name}_${collectionName}QuerySpecification")

    val pDoGetContainedBodies = createDoGetContainedBodies(pattern, collectionName)
    val doGetContainedBodiesContent = q"{..$pDoGetContainedBodies}"
    val doGetContainedBodies = q"override def doGetContainedBodies(): Set[PBody] = $doGetContainedBodiesContent"
    val pGeneratedPQuery = getGeneratedPQuery(pattern.parameters).toList ++ List(doGetContainedBodies)
    val pGeneratePQueryClass = q"class GeneratedPQuery extends AbstractPQuery { ..$pGeneratedPQuery }"

    val rootClass = q"class $className extends ScalaQuerySpecification { $pGeneratePQueryClass }"

    println(rootClass)

    // return ast
    rootClass
  }

  private def createDoGetContainedBodies(pattern: GraphPattern, collectionName: String): List[Stat] = {
    val pFullyQualifiedName = Lit.String(s"$collectionName.${pattern.name}")
    val pGetFullyQualifiedName = q"override def getFullyQualifiedName(): String = $pFullyQualifiedName"

    val pParamPNames = for (param <- pattern.parameters.toList) yield { Term.Name(s"p_${param.name}") }
    val pGetParameters = q"override def getParameters(): List[PParameter] = List(..$pParamPNames)"

    val pParamNamesString = for (param <- pattern.parameters.toList) yield { Lit.String(param.name) }
    val pGetParameterNames = q"override def getParameterNames(): List[String] = List(..$pParamNamesString)"

    val pReturnBodies = q"bodies"

    val pBodies = q"val bodies: Set[PBody] = SetSequence.fromSet(HashSet[PBody]())"

    List(pBodies) ++
      createGraphPatternBodies(pattern) ++
      List(pGetFullyQualifiedName, pGetParameters, pGetParameterNames, pReturnBodies)
  }

  private def createGraphPatternBodies(pattern: GraphPattern) = for (body <- pattern.bodies.toList) yield {
    val pPBody = q"val body: PBody = PBody(this)"

    val tempVars: Map[String, String] = getTemporaryVariables(body)

    val pBody = List(pPBody) ++
      createTemporaryVariables(tempVars) ++
      createTypeConstraints(tempVars) ++
      createLocalGlobalVariables(pattern.parameters)
    q"{ ..$pBody }"
  }

  private def createTypeConstraints(temporaryVariables: Map[String, String]) =
    for ((name, typ) <- temporaryVariables) yield {
      val tempVarTyp = Lit.String(typ.substring(1))
      val tempVarName = Term.Name(s"var__$name")

      q"TypeConstraint(body, Tuples.flatTupleOf($tempVarName), ConceptKey(MetaAdapterFactory.getConcept($tempVarTyp)))"
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

  private def getGeneratedPQuery(graphParameters: Seq[IParameter]): Seq[Defn] =
    for (graphParameter <- graphParameters) yield {
      val pParamString = s"p_${graphParameter.name}"
      val pParamName = Pat.Var(Term.Name(pParamString))
      val pParamNameString = Lit.String(pParamString)
      val pParamFullyQualifiedName = Lit.String(graphParameter.typ.get.toString.substring(1))

      val pConceptKey = q"ConceptKey()"
      val pPParameter = q"PParameter($pParamNameString, $pParamFullyQualifiedName, $pConceptKey)"
      val pGeneratedPQueryParameter = q"val $pParamName: PParameter = $pPParameter"
      pGeneratedPQueryParameter
    }

  private def getTemporaryVariables(body: IPatternBody): Map[String, String] = body.contents.collect {
    case p: PatternCompositionConstraint => p.call.arguments.collect {
      case t: TemporaryVariable => Map[String, String](t.name -> t.typ.get.toString)
    }
  }.flatten.flatten.toMap
}


