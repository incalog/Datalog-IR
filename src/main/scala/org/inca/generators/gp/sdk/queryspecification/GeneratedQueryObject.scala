package org.inca.generators.gp.sdk.queryspecification

import org.inca.generators.gp.sdk.queryspecification.QuerySpecificationGenerator._
import org.inca.generators.gp.sdk.queryspecification.TypeConstraints._
import org.inca.generators.gp.sdk.queryspecification.Variables._
import org.inca.lang.core.Content.IParameter
import org.inca.lang.gp.Content.GraphPattern

import VariableDissolver._

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

  private def createGraphPatternBodies(pattern: GraphPattern): List[Term] =
    pattern.bodies.map { body =>
      q"""{
          val body: PBody = new PBody(that)
          ..${createLocalGlobalVariables(pattern.parameters)}
          ..${createTemporaryVariables(getTemporaryVariables(body.contents))}
          ..${createContextPointers(getGeneratedTemporaryVariables(body.contents))}
          ..${primitivesToParams(collectUniquePrimitives(body.contents))}
          ..${createTypeConstraintsParameters(pattern.parameters)}
          ..${createTypeConstraints(body.contents)}
          body
        }"""
    }.toList

  // todo refactor everything below

  // todo check if even necessary
  private def createContextPointers(names: List[String]): List[Stat] =
    names.map { name =>
      q"""new TypeConstraint(
         body,
         Tuples.flatTupleOf(${asVar(name).toTerm}),
         new ClassKey(NodeType(classOf[org.inca.lang.core.Constraints.ContextPointer]))
       )"""
    }

  private def overrideFunctions(pattern: GraphPattern, collectionName: String): List[Stat] = {

    val pFullyQualifiedName = Lit.String(s"$collectionName.${pattern.name}")
    val pGetFullyQualifiedName = q"override def getFullyQualifiedName: String = $pFullyQualifiedName"

    val pParamPNames = pattern.parameters.map { p => Term.Name(s"p_${p.name}")}
    val pGetParameters = q"override def getParameters: util.List[PParameter] = util.List.of(..$pParamPNames)"

    val pParamNamesString = pattern.parameters.map { p => Lit.String(p.name)}
    val pGetParameterNames = q"override def getParameterNames: util.List[String] = util.List.of(..$pParamNamesString)"

    List(pGetFullyQualifiedName, pGetParameterNames, pGetParameters)
  }

  private def pparams(graphParameters: Seq[IParameter]): List[Stat] =
    graphParameters.map { gp =>
      val pParamString = s"p_${gp.name}"
      val pParamName = Pat.Var(Term.Name(pParamString))
      val pParamNameString = Lit.String(pParamString)
      val pParamFullyQualifiedName = Lit.String(gp.typ.get.toString)

        // todo rm PlaceholderConceptKey
      val pConceptKey = q"new PlaceholderConceptKey()"
      q"private val $pParamName: PParameter = new PParameter($pParamNameString, $pParamFullyQualifiedName, $pConceptKey)"
    }.toList
}
