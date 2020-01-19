package org.inca.generators.gp.sdk.queryspecification

import org.inca.generators.gp.util.Util.classPathToTypeSelect
import org.inca.lang.core.Content.{IParameter, IPatternBody, TemporaryVariable}
import org.inca.lang.gp.Constraints.PatternCompositionConstraint
import org.inca.lang.gp.Content.GraphPattern

import scala.meta._

object ParentObject {

  def generateParentObject(pattern: GraphPattern, collectionName: String): Stat = {

    // {} is necessary that the above line won't be interpreted
    // as a modifier for the below line #lifehacks
    q"""
      object ${classTermName(pattern.name, collectionName)} {
        final class GeneratedPQuery extends AbstractPQuery {
            private val that = this
            ..${pparams(pattern.parameters)}
            {}
            override protected def doGetContainedBodies(): Set[PBody] = {
              val bodies: Set[PBody] = Set.of(
                ..${createGraphPatternBodies(pattern)}
              )
              bodies
            }
            ..${overrideFunctions(pattern, collectionName)}
        }
      }"""
  }

  private def createGraphPatternBodies(pattern: GraphPattern): List[Term] = for (body <- pattern.bodies.toList) yield {
    q"""{
          val body: PBody = new PBody(that)
          ..${createLocalGlobalVariables(pattern.parameters)}
          ..${createTemporaryVariables(getTemporaryVariables(body))}
          ..${createTypeConstraints(pattern.parameters)}
          body
        }
        """
  }

  private def createTypeConstraints(graphParameters: Seq[IParameter]): List[Stat] =
    (for (graphParameter <- graphParameters) yield {
      val paramVarName = Term.Name(s"var_${graphParameter.name}")

      val paramTypeName = classPathToTypeSelect(graphParameter.typ.get.toString)
      val paramType = q"classOf[$paramTypeName]"

      val params = List(q"body", q"Tuples.flatTupleOf($paramVarName)", q"new ClassKey($paramType)")

      q"new TypeConstraint(..$params)"
    }).toList

  private def createTemporaryVariables(temporaryVariables: Map[String, String]): List[Stat] =
    (for ((name, _) <- temporaryVariables) yield {
      val tempVarValue = Lit.String(name)
      val tempVarName = Pat.Var(Term.Name(s"var__$name"))

      q"val $tempVarName: PVariable = body.getOrCreateVariableByName($tempVarValue)"
    }).toList

  private def createLocalGlobalVariables(graphParameters: Seq[IParameter]): List[Stat] =
    (for (graphParameter <- graphParameters) yield {
      val paramName = Lit.String(graphParameter.name)
      val param_var_name = Pat.Var(Term.Name(s"var_${graphParameter.name}"))

      q"val $param_var_name: PVariable = body.getOrCreateVariableByName($paramName)"
    }).toList


  private def getTemporaryVariables(body: IPatternBody): Map[String, String] = body.contents.collect {
    case p: PatternCompositionConstraint => p.call.arguments.collect {
      case t: TemporaryVariable => Map[String, String](t.name -> t.typ.get.toString)
    }
  }.flatten.flatten.toMap

  private def overrideFunctions(pattern: GraphPattern, collectionName: String): List[Stat] = {

    val pFullyQualifiedName = Lit.String(s"$collectionName.${pattern.name}")
    val pGetFullyQualifiedName = q"override def getFullyQualifiedName(): String = $pFullyQualifiedName"

    val pParamPNames = for (param <- pattern.parameters.toList) yield {
      Term.Name(s"p_${param.name}")
    }
    val pGetParameters = q"override def getParameters(): List[PParameter] = List.of(..$pParamPNames)"

    val pParamNamesString = for (param <- pattern.parameters.toList) yield {
      Lit.String(param.name)
    }
    val pGetParameterNames = q"override def getParameterNames(): List[String] = List.of(..$pParamNamesString)"

    List(pGetFullyQualifiedName, pGetParameterNames, pGetParameters)
  }

  private def pparams(graphParameters: Seq[IParameter]): List[Stat] =
    (for (graphParameter <- graphParameters) yield {
      val pParamString = s"p_${graphParameter.name}"
      val pParamName = Pat.Var(Term.Name(pParamString))
      val pParamNameString = Lit.String(pParamString)
      val pParamFullyQualifiedName = Lit.String(graphParameter.typ.get.toString)

      val pConceptKey = q"new PlaceholderConceptKey()"
      val pPParameter = q"new PParameter($pParamNameString, $pParamFullyQualifiedName, $pConceptKey)"
      val pGeneratedPQueryParameter = q"private val $pParamName: PParameter = $pPParameter"
      pGeneratedPQueryParameter
    }).toList

  private def classTermName(patternName: String, collectionName: String) =
    Term.Name(s"${patternName}_${collectionName}QuerySpecification")

}
