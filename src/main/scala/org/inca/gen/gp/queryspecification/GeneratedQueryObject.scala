package org.inca.gen.gp.queryspecification

import TypeConstraints._
import Variables._
import org.inca.lang.core.Content.IParameter
import org.inca.lang.gp.Content.GraphPattern

import scala.meta._
import Util._

object GeneratedQueryObject {

  def createGraphPatternBodies(pattern: GraphPattern): List[Term] =
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
         Tuples.flatTupleOf(${Term.Name(s"var__$name")}),
         new ClassKey(NodeType(classOf[org.inca.lang.core.Constraints.ContextPointer]))
       )"""
    }


  def pparams(graphParameters: Seq[IParameter]): List[Stat] =
    graphParameters.map { gp =>
      val pParamString = s"p_${gp.name}"
      val pParamName = Pat.Var(Term.Name(pParamString))
      val pParamNameString = Lit.String(pParamString)
      val pParamFullyQualifiedName = Lit.String(gp.typ.get.toString.tail)
      val primitiveTypeName = classPathToTypeSelect(gp.typ.get.toString)

        // todo rm PlaceholderConceptKey
      val pConceptKey = q"new TFInputKey.NodeTypeKey(MetaElements.NodeType(classOf[$primitiveTypeName]))"


      q"""private val $pParamName: PParameter =
            new PParameter($pParamNameString, $pParamFullyQualifiedName, $pConceptKey)"""
    }.toList
}
