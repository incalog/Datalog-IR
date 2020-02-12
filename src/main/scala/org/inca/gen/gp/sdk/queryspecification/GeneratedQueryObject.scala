package org.inca.gen.gp.sdk.queryspecification

import TypeConstraints._
import Variables._
import org.inca.lang.core.Content.IParameter
import org.inca.lang.gp.Content.GraphPattern

import VariableDissolver._

import scala.meta._

object GeneratedQueryObject {

  def generateParentObject(pattern: GraphPattern): Stat = {

    val fileNameTerm = Term.Name(pattern.name)
    val fileNameType = Type.Name(pattern.name)

    // {} is necessary that the above line won't be interpreted
    // as a modifier for the below line #lifehacks
    q"""
      object $fileNameTerm {

        def instance(): $fileNameType = LazyHolder.INSTANCE

        private final class LazyHolder
        private final object LazyHolder {
          val INSTANCE: $fileNameType = make()
          def make(): $fileNameType = new $fileNameType()
        }


        final class GeneratedPQuery extends BasePQuery(PVisibility.PUBLIC) {
            private val that = this
            ..${pparams(pattern.parameters)}
            {}
            override protected def doGetContainedBodies(): util.Set[PBody] = {
              val bodies: util.Set[PBody] = util.Set.of(
                ..${createGraphPatternBodies(pattern)}
              )
              bodies
            }
            ..${overrideFunctions(pattern)}
        }

        final object GeneratedPQuery {
          val INSTANCE = new GeneratedPQuery
        }
      }"""
  }

  private def createGraphPatternBodies(pattern: GraphPattern): List[Term] =
    pattern.bodies.map { body =>
      q"""{
          val body: PBody = new PBody(that)
          ..${createLocalGlobalVariables(pattern.parameters)}
          ()
          val exportedParams = new util.ArrayList[ExportedParameter]()
          ..${createExportedParams(pattern.parameters)}
          body.setSymbolicParameters(exportedParams)

          ..${createTemporaryVariables(getTemporaryVariables(body.contents))}
          ..${createContextPointers(getGeneratedTemporaryVariables(body.contents))}
          ..${primitivesToParams(collectUniquePrimitives(body.contents))}
          ..${createTypeConstraintsParameters(pattern.parameters)}
          ..${createTypeConstraints(body.contents)}
          body
        }"""
    }.toList

  // todo refactor everything below

  private def createExportedParams(graphParameters: Seq[IParameter]) =
    graphParameters.map { gp =>
      val bodyVar = Term.Name(s"var_${gp.name}")
      val param = Term.Name(s"p_${gp.name}")

      q"exportedParams.add(new ExportedParameter(body, $bodyVar, $param))"
    }.toList

  // todo check if even necessary
  private def createContextPointers(names: List[String]): List[Stat] =
    names.map { name =>
      q"""new TypeConstraint(
         body,
         Tuples.flatTupleOf(${asVar(name).toTerm}),
         new ClassKey(NodeType(classOf[org.inca.lang.core.Constraints.ContextPointer]))
       )"""
    }

  private def overrideFunctions(pattern: GraphPattern): List[Stat] = {

    val pFullyQualifiedName = Lit.String(pattern.name)
    val pGetFullyQualifiedName = q"override def getFullyQualifiedName: String = $pFullyQualifiedName"

    val pParamPNames = pattern.parameters.map { p => Term.Name(s"p_${p.name}")}.toList
    val pGetParameters = q"override def getParameters: util.List[PParameter] = util.List.of(..$pParamPNames)"

    val pParamNamesString = pattern.parameters.map { p => Lit.String(p.name)}.toList
    val pGetParameterNames = q"override def getParameterNames: util.List[String] = util.List.of(..$pParamNamesString)"

    List(pGetFullyQualifiedName, pGetParameterNames, pGetParameters)
  }

  private def pparams(graphParameters: Seq[IParameter]): List[Stat] =
    graphParameters.map { gp =>
      val pParamString = s"p_${gp.name}"
      val pParamName = Pat.Var(Term.Name(pParamString))
      val pParamNameString = Lit.String(pParamString)
      val pParamFullyQualifiedName = Lit.String(gp.typ.get.toString.tail)
      val primitiveTypeName = gp.typ.get.toString.toClassPath

        // todo rm PlaceholderConceptKey
      val pConceptKey = q"new TFInputKey.NodeTypeKey(MetaElements.NodeType(classOf[$primitiveTypeName]))"


      q"""private val $pParamName: PParameter =
            new PParameter($pParamNameString, $pParamFullyQualifiedName, $pConceptKey)"""
    }.toList
}
