package org.inca.generators.gp.sdk.queryspecification

import org.inca.generators.gp.sdk.queryspecification.QuerySpecificationGenerator._
import org.inca.generators.gp.sdk.queryspecification.TypeConstraints._
import org.inca.generators.gp.sdk.queryspecification.util._
import org.inca.lang.core.Content.{IParameter, IPatternBodyContent, TemporaryVariable}
import org.inca.lang.gp.Constraints.PathExpressionConstraint
import org.inca.lang.gp.Content.GraphPattern
import org.inca.lang.gp.Element.GeneratedParameter
import Variables._

import scala.meta._

object GeneratedQueryObject {

  def generateParentObject(pattern: GraphPattern, collectionName: String): Stat = {

    // {} is necessary that the above line won't be interpreted
    // as a modifier for the below line #lifehacks
    q"""
      object ${classTermName(pattern, collectionName)} {
        final class GeneratedPQuery extends AbstractPQuery {
            private val that = this
            ..${pparams(pattern.parameters)}
            {}
            override protected def doGetContainedBodies(): util.Set[PBody] = {
              val bodies: util.Set[PBody] = util.Set.of(
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
          ..${createTemporaryVariables(getTemporaryVariables(body.contents))}
          ..${createContextPointers(getGeneratedTemporaryVariables(body.contents))}
          ..${primitivesToParams(collectUniquePrimitives(body.contents))}
          ..${createTypeConstraintsParameters(pattern.parameters)}
          ..${createTypeConstraints(body.contents)}
          body
        }
        """
  }

  // todo refactor everything below
  private def createContextPointers(names: List[String]): List[Stat] = {
    for (name <- names) yield {
      q"""new TypeConstraint(
         body,
         Tuples.flatTupleOf(${tempVarName(name)}),
         new ClassKey(NodeType(classOf[org.inca.lang.core.Constraints.ContextPointer]))
       )"""
    }
  }

  private def overrideFunctions(pattern: GraphPattern, collectionName: String): List[Stat] = {

    val pFullyQualifiedName = Lit.String(s"$collectionName.${pattern.name}")
    val pGetFullyQualifiedName = q"override def getFullyQualifiedName: String = $pFullyQualifiedName"

    val pParamPNames = for (param <- pattern.parameters.toList) yield {
      Term.Name(s"p_${param.name}")
    }
    val pGetParameters = q"override def getParameters: util.List[PParameter] = util.List.of(..$pParamPNames)"

    val pParamNamesString = for (param <- pattern.parameters.toList) yield {
      Lit.String(param.name)
    }
    val pGetParameterNames = q"override def getParameterNames: util.List[String] = util.List.of(..$pParamNamesString)"

    List(pGetFullyQualifiedName, pGetParameterNames, pGetParameters)
  }

  private def pparams(graphParameters: Seq[IParameter]): List[Stat] =
    (for (graphParameter <- graphParameters) yield {
      val pParamString = s"p_${graphParameter.name}"
      val pParamName = Pat.Var(Term.Name(pParamString))
      val pParamNameString = Lit.String(pParamString)
      val pParamFullyQualifiedName = Lit.String(graphParameter.typ.get.toString)

      val pConceptKey = q"new PlaceholderConceptKey()"
      q"private val $pParamName: PParameter = new PParameter($pParamNameString, $pParamFullyQualifiedName, $pConceptKey)"
    }).toList
}
