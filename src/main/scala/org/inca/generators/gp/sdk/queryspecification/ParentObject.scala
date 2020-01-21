package org.inca.generators.gp.sdk.queryspecification

import org.inca.generators.gp.util.Util.classPathToTypeSelect
import org.inca.lang.core.Content.{IParameter, IPatternBody, IPatternBodyContent, TemporaryVariable}
import org.inca.lang.gp.Constraints.{PathExpressionConstraint, PatternCompositionConstraint}
import org.inca.lang.gp.Content.{GraphPattern, GraphPatternParameter}
import org.inca.generators.gp.sdk.queryspecification.QuerySpecificationGenerator._
import org.inca.lang.core.Reference.VariableReference

import scala.meta._

object ParentObject {

  def generateParentObject(pattern: GraphPattern, collectionName: String): Stat = {

    // {} is necessary that the above line won't be interpreted
    // as a modifier for the below line #lifehacks
    q"""
      object ${classTermName(pattern, collectionName)} {
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
          ..${createTemporaryVariables(getTemporaryVariables(body.contents))}
          ..${createTypeConstraintsParameters(pattern.parameters)}
          ..${createTypeConstraintsPathExpressions(body.contents)}
          body
        }
        """
  }

  private def createTypeConstraintsPathExpressions(pathExpressions: Seq[IPatternBodyContent]): List[Stat] =
    pathExpressions.collect {
      case pxc: PathExpressionConstraint => {
        val src = pxc.src.variable match {
          case t: TemporaryVariable =>
            Term.Name(s"var__${t.name}")
          case gpp: GraphPatternParameter =>
            Term.Name(s"var_${gpp.name}")
        }
        val trg = pxc.trg match {
          case vr: VariableReference =>
            Term.Name(s"var__${vr.variable.name}")

          case tv: TemporaryVariable =>
            Term.Name(s"var__${tv.name}")
        }
        q"""new TypeConstraint(
             body,
             Tuples.staticArityFlatTupleOf($src, $trg),
             new LinkKey(NodeType(classOf[${classPathToTypeSelect(pxc.typ.toString)}])(${Lit.String(pxc.element.link.toString)}))
           )"""
      }
    }.toList

  private def getTemporaryVariables(body: Seq[IPatternBodyContent]): List[String] =
    body.collect {
      case p: PathExpressionConstraint => {
        val trg = p.trg match {
          case t: TemporaryVariable => t.name
          case r: VariableReference => r.variable.name
        }
        trg
      }
    }.toList.distinct

  private def createTypeConstraintsParameters(graphParameters: Seq[IParameter]): List[Stat] =
    (for (graphParameter <- graphParameters) yield {
      q"""new TypeConstraint(
         body,
         Tuples.flatTupleOf(${Term.Name(s"var_${graphParameter.name}")}),
         new ClassKey(classOf[${classPathToTypeSelect(graphParameter.typ.get.toString)}])
       )"""
    }).toList

  private def createTemporaryVariables(names: List[String]): List[Stat] = for (name <- names) yield {
    val tempVarValue = Lit.String(name)
    val tempVarName = Pat.Var(Term.Name(s"var__$name"))

    q"val ${tempVarName}: PVariable = body.getOrCreateVariableByName(${tempVarValue})"
  }

  private def createLocalGlobalVariables(graphParameters: Seq[IParameter]): List[Stat] =
    (for (graphParameter <- graphParameters) yield {
      val paramName = Lit.String(graphParameter.name)
      val param_var_name = Pat.Var(Term.Name(s"var_${graphParameter.name}"))

      q"val $param_var_name: PVariable = body.getOrCreateVariableByName($paramName)"
    }).toList


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
      q"private val $pParamName: PParameter = new PParameter($pParamNameString, $pParamFullyQualifiedName, $pConceptKey)"
    }).toList


}
