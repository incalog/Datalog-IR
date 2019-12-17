package org.inca.generators.gp

import org.inca.core.Content.{IParameter, IPatternBody, TemporaryVariable}
import org.inca.gp.Constraints.PatternCompositionConstraint
import org.inca.gp.Content.GraphPattern

import scala.meta._


class GPGenerator {
  val filename = "test.scala"

  /**
   *
   * @param pattern        GraphPattern which will be transformed into scalameta tree
   * @param collectionName Name of the file (mps) where pattern is saved
   * @return
   */
  def generate(pattern: GraphPattern, collectionName: String): Defn.Class = {

    val pDoGetContainedBodies: List[Stat] = createGraphPatternBodies(pattern)

    val doGetContainedBodies_body = q"{..$pDoGetContainedBodies}"

    val doGetContainedBodies = q"override def doGetContainedBodies(): Set[PBody] = $doGetContainedBodies_body"

    val pGeneratedPQuery = getGeneratedPQuery(pattern.parameters).toList ++ List(doGetContainedBodies)

    /*
     * GeneratedPQuery
     */
    val innerClass = q"class GeneratedPQuery extends AbstractPQuery { ..$pGeneratedPQuery }"

    /*
     * class
     */
    val className = Type.Name(s"${pattern.name}_${collectionName}QuerySpecification")
    val rootClass = q"class $className extends ScalaQuerySpecification { $innerClass }"

    println(rootClass)

    // return ast
    rootClass
  }

  private def createGraphPatternBodies(pattern: GraphPattern) = for (body <- pattern.bodies.toList) yield {
    val PBody_body = q"val body: PBody = PBody(this)"

    val tempVars: Map[String, String] = getTemporaryVariables(body)

    val qBody = List(PBody_body) ++
      createTemporaryVariables(tempVars) ++
      createTypeConstraints(tempVars) ++
      createLocalGlobalVariables(pattern.parameters)
    q"{ ..$qBody }"
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

      val qConceptKey = q"ConceptKey()"
      val qPParameter = q"PParameter($pParamNameString, $pParamFullyQualifiedName, $qConceptKey)"
      val pGeneratedPQueryParameter = q"val $pParamName: PParameter = $qPParameter"
      pGeneratedPQueryParameter
    }

  private def getTemporaryVariables(body: IPatternBody): Map[String, String] = body.contents.collect {
    case p: PatternCompositionConstraint => p.call.arguments.collect {
      case t: TemporaryVariable => Map[String, String](t.name -> t.typ.get.toString)
    }
  }.flatten.flatten.toMap
}


