package inca.runtime

import inca.runtime.data.MockURI
import inca.runtime.db.{DBValue, DatabaseInspector}
import org.eclipse.viatra.query.runtime.api.ViatraQueryEngine
import org.eclipse.viatra.query.runtime.api.impl.{BaseMatcher, BasePatternMatch, BaseQuerySpecification}
import org.eclipse.viatra.query.runtime.api.scope.QueryScope
import org.eclipse.viatra.query.runtime.matchers.psystem.queries.PQuery
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuple
import truechange.{EditScript, URI}

import java.util
import scala.jdk.CollectionConverters._

object Query {
  trait ChangeFeed {
    def processEditScript(edits: EditScript): Unit
    def insertExtensionalTuple(relName: String, tuple: Tuple): Unit
    def deleteExtensionalTuple(relName: String, tuple: Tuple): Unit
    def loadPrimitive(a: Any): Unit
    def unloadPrimitive(a: Any): Unit
  }

  class Specification(query: PQuery) extends BaseQuerySpecification[Matcher](query) {
    override def getPreferredScopeClass: Class[_ <: QueryScope] = classOf[context.QueryScope]

    override def instantiate(): Matcher =
      new Matcher(this)

    override def instantiate(engine: ViatraQueryEngine): Matcher =
      engine.getMatcher(this)

    override def newEmptyMatch(): Match =
      Match(this, Array.ofDim(getParameters.size()), isMutable = true)

    override def newMatch(parameters: AnyRef*): Match =
      Match(this, parameters.toArray, isMutable = false)
  }


  class Matcher(spec: Specification) extends BaseMatcher[Match](spec) {
    /** Converts the array representation of a pattern match to an immutable Match object. */
    protected def arrayToMatch(parameters: Array[AnyRef]): Match =
      Match(spec, parameters, isMutable = false)

    /** Converts the array representation of a pattern match to a mutable Match object. */
    protected def arrayToMatchMutable(parameters: Array[AnyRef]): Match =
      Match(spec, parameters, isMutable = true)

    protected def tupleToMatch(t: Tuple): Match =
      Match(spec, t.getElements, isMutable = false)

    def getAllMatchArrays: Iterable[Array[AnyRef]] =
      getAllMatches.asScala.map(_.toArray)
  }


  case class Match(spec: Specification, private var values: Array[AnyRef], isMutable: Boolean) extends BasePatternMatch {
    override def specification(): Specification = spec

    override def get(parameterName: String): Any =
      Option(spec.getPositionOfParameter(parameterName)) match {
        case Some(i) => values(i)
        case None => null
      }

    override def set(parameterName: String, newValue: AnyRef): Boolean = {
      if (!isMutable)
        throw new UnsupportedOperationException
      Option(spec.getPositionOfParameter(parameterName)) match {
        case Some(i) => values(i) = newValue; true
        case None => false
      }
    }

    override def toArray: Array[AnyRef] =
      util.Arrays.copyOf(values, values.length)

    override def toImmutable: Match =
      if (isMutable)
        Match(spec, toArray, isMutable = false)
      else
        this

    override def prettyPrint(): String = {
      val builder = new StringBuilder
      for (i <- 0 until values.length) {
        if (i != 0) builder.append(", ")
        builder.append("\"" + parameterNames.get(i) + "\"=" + BasePatternMatch.prettyPrintValue(values(i)))
      }
      builder.toString
    }

    def deepPrettyPrint(db: DatabaseInspector): String = {
      val builder = new StringBuilder
      for (i <- 0 until values.length) {
        if (i != 0) builder.append(", ")
        builder.append("\"" + parameterNames.get(i) + "\"=" + DBValue.prettyPrint(values(i), db))
      }
      builder.toString
    }
  }
}

