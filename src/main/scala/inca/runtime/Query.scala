package inca.runtime

import inca.runtime.db.DatabaseInspector
import org.eclipse.viatra.query.runtime.api.impl.BaseMatcher
import org.eclipse.viatra.query.runtime.api.impl.BasePatternMatch
import org.eclipse.viatra.query.runtime.api.impl.BaseQuerySpecification
import org.eclipse.viatra.query.runtime.api.scope.QueryScope
import org.eclipse.viatra.query.runtime.api.ViatraQueryEngine
import org.eclipse.viatra.query.runtime.matchers.psystem.queries.PQuery
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuple
import scala.jdk.CollectionConverters._
import truechange.EditScript

object Query {
  trait ChangeFeed {
    def processEditScript(edits: EditScript): Unit
    def insert(relName: String, tuple: Tuple): Unit
    def delete(relName: String, tuple: Tuple): Unit
  }

  class Specification(query: PQuery) extends BaseQuerySpecification[Matcher](query) {
    override def getPreferredScopeClass: Class[_ <: QueryScope] = classOf[context.QueryScope]

    override def instantiate(): Matcher =
      new Matcher(this)

    override def instantiate(engine: ViatraQueryEngine): Matcher =
      engine.getMatcher(this)

    override def newEmptyMatch(): Match =
      Match(this, Array.ofDim[Any](getParameters.size()), isMutable = true)

    override def newMatch(parameters: Any*): Match =
      Match(this, parameters.toArray, isMutable = false)
  }

  class Matcher(spec: Specification) extends BaseMatcher[Match](spec) {

    /** Converts the array representation of a pattern match to an immutable Match object. */
    protected def arrayToMatch(parameters: Array[Any]): Match =
      Match(spec, parameters, isMutable = false)

    /** Converts the array representation of a pattern match to a mutable Match object. */
    protected def arrayToMatchMutable(parameters: Array[Any]): Match =
      Match(spec, parameters, isMutable = true)

    protected def tupleToMatch(t: Tuple): Match =
      Match(spec, t.getElements, isMutable = false)

    def getAllMatchArrays: Iterable[Array[Any]] =
      getAllMatches.asScala.map(_.toArray)
  }

  case class Match(spec: Specification, private var values: Array[Any], isMutable: Boolean)
      extends BasePatternMatch {
    override def specification(): Specification = spec

    override def get(parameterName: String): Any =
      Option(spec.getPositionOfParameter(parameterName)) match {
        case Some(i) => values(i)
        case None => null
      }

    override def set(parameterName: String, newValue: Any): Boolean = {
      if (!isMutable)
        throw new UnsupportedOperationException
      Option(spec.getPositionOfParameter(parameterName)) match {
        case Some(i) => values(i) = newValue; true
        case None => false
      }
    }

    override def toArray: Array[Any] = values
    // util.Arrays.copyOf(values, values.length)

    override def toImmutable: Match =
      if (isMutable)
        Match(spec, toArray, isMutable = false)
      else
        this

    override def prettyPrint(): String = {
      val builder = new StringBuilder
      for (i <- 0 until values.length) {
        if (i != 0) builder.append(", ")
        builder.append(
          "\"" + parameterNames.get(i) + "\"=" + BasePatternMatch.prettyPrintValue(values(i))
        )
      }
      builder.toString
    }

    def deepPrettyPrint(db: DatabaseInspector): String = {
      val builder = new StringBuilder
      for (i <- 0 until values.length) {
        if (i != 0) builder.append(", ")
        builder.append("\"" + parameterNames.get(i) + "\"=" + db.prettyPrint(values(i)))
      }
      builder.toString
    }
  }
  object Match {
    def apply(spec: Specification, keyValuePairs: Map[String, Any], isMutable: Boolean): Match = {
      val m = Match(spec, Array.ofDim[Any](keyValuePairs.size), isMutable = true)
      keyValuePairs.foreach { case (k, v) =>
        m.set(k, v)
      }
      if (isMutable) m
      else m.toImmutable
    }
  }
}
