package org.inca

import java.lang.reflect.Field

import org.inca.core_old.constraints.PatternCall
import org.inca.core_old.content.TemporaryVariable
import org.inca.core_old.reference.VariableReference
import org.inca.core_old.typ.compileTime.ConceptReferenceType
import org.inca.gp.Constraints.{PathExpressionConstraint, PatternCompositionConstraint}
import org.inca.gp.Content.{GraphPattern, GraphPatternBody, GraphPatternParameter}
import org.inca.gp.Element.PathElement
import org.inca.meta.NodeType
import org.inca.mps.{INamedConcept, InterfacePart}

/**
  * IncA is a DSL designed to improve incremental problem solving.
  *
  * This is a port from MPS to Scala to provide easier maintainability.
  *
  * For more general information see readme.md.
  *
  * @author Paul Hempel
  * @version 0.0.0.1-pre-alpha
  */
object Main extends App {
  println("Hello, Scala developer!")

  case class Node(name: String) extends INamedConcept
  case class Edge(from: Node, to: Node)
  case class Graph(nodes: Seq[Node], edges: Seq[Edge])

  val nodeType = NodeType(classOf[Node])
  val edgeType = NodeType(classOf[Edge])
  val edgeToNodeLink = edgeType("to")
  val srcGraphParam = GraphPatternParameter("src", Some(nodeType))
  val trgGraphParam = GraphPatternParameter("trg", Some(nodeType))
  val intermediate = TemporaryVariable("inter", Some(nodeType))

  val path = GraphPattern(
    "Path",
    Seq(
      srcGraphParam,
      trgGraphParam
    ),
    Seq(
      GraphPatternBody(Seq(
        PathExpressionConstraint(
          VariableReference(srcGraphParam),
          VariableReference(trgGraphParam),
          PathElement(None, edgeToNodeLink),
          edgeType
        )
      )),
      GraphPatternBody(Seq(
        // todo `neg` should be call
        PatternCompositionConstraint(
          neg = false,
          PatternCall(
            transitive = false,
            Seq(
              VariableReference(srcGraphParam),
              intermediate
            ),
            path
          )
        ),
        PathExpressionConstraint(
          VariableReference(intermediate),
          VariableReference(trgGraphParam),
          PathElement(None, edgeToNodeLink),
          edgeType
        )
      )
    )),
    None
  )
}
