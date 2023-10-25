import org.eclipse.viatra.query.runtime.api.{GenericPatternMatcher, ViatraQueryEngine}
import org.eclipse.viatra.query.runtime.api.scope.QueryScope as ViatraQueryScope
import org.eclipse.viatra.query.runtime.matchers.psystem.{PBody, PVariable}
import org.eclipse.viatra.query.runtime.matchers.psystem.queries.{BasePQuery, PParameter, PVisibility}
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuples
import org.eclipse.viatra.query.runtime.matchers.psystem.basicdeferred.ExportedParameter
import org.eclipse.viatra.query.runtime.matchers.context.common.JavaTransitiveInstancesKey

import java.util
import org.eclipse.viatra.query.runtime.matchers.psystem.basicdeferred.*
import org.eclipse.viatra.query.runtime.matchers.psystem.basicenumerables.*
import inca.viatra.compile.PSystem
import inca.viatra.runtime.Query.Specification
import inca.viatra.runtime.index.NamedRelationKey
import inca.viatra.runtime.aggregate.builtin
import org.eclipse.viatra.query.runtime.matchers.psystem.basicdeferred.AggregatorConstraint
import org.eclipse.viatra.query.runtime.matchers.psystem.aggregations.BoundAggregator

import scala.util.Try

object Plus extends PSystem.Module {
  trait Nat
  case object Zero extends Nat
  case class Succ(param_0: Nat) extends Nat

  /*enum Nat {
  case Zero()
  case Succ(param_0: Any)
  }*/

  override val patterns: Map[String, () => Specification] = Map("main" -> (() => main.instance))

  object main {
    lazy val instance: Specification = new Specification(generatedPQuery)

    private object generatedPQuery extends BasePQuery(PVisibility.PUBLIC) {
      private val param_main_result$0: PParameter = new PParameter("main_result$0")

      override protected def doGetContainedBodies(): util.Set[PBody] = util.Set.of({
        val body: PBody = new PBody(this)
        val var_main_result$0: PVariable = body.getOrCreateVariableByName("main_result$0")
        val exportedParams = new util.ArrayList[ExportedParameter]()
        exportedParams.add(new ExportedParameter(body, var_main_result$0, param_main_result$0))

        body.setSymbolicParameters(exportedParams)

        val eval_out$0: PVariable = body.getOrCreateVariableByName("out$0")
        val eval_out$1: PVariable = body.getOrCreateVariableByName("out$1")

        new ExpressionEvaluation(body, new org.eclipse.viatra.query.runtime.matchers.psystem.IExpressionEvaluator {
          override def getShortDescription: String = "eval(`Nat.Zero`)"
          override def getInputParameterNames: java.lang.Iterable[String] = java.util.Arrays.asList()
          override def evaluateExpression(env: org.eclipse.viatra.query.runtime.matchers.psystem.IValueProvider): Any = {
            (Zero)
          }
        }, eval_out$0)

        new ExpressionEvaluation(body, new org.eclipse.viatra.query.runtime.matchers.psystem.IExpressionEvaluator {
          override def getShortDescription: String = "eval(`(Nat.Succ)(`Nat.Zero`)`)"
          override def getInputParameterNames: java.lang.Iterable[String] = java.util.Arrays.asList("out$0")
          override def evaluateExpression(env: org.eclipse.viatra.query.runtime.matchers.psystem.IValueProvider): Any = {
            (Succ)(env.getValue("out$0").asInstanceOf[Nat])

            // You can use Try here to actually see the error messages, otherwise viatra just fails the body
            /*Try(
              env.getValue("out$0") match {
                case Nat.Zero => "A"
                case Nat.Succ(_) => "B"
              }
            )*/
          }
        }, eval_out$1)
        new TypeConstraint(body, Tuples.flatTupleOf(), NamedRelationKey("ext_main$input", 0))
        new Equality(body, var_main_result$0, eval_out$1)
        body
      })

      override def getFullyQualifiedName: String = "Plus_main"
      override def getParameters: util.List[PParameter] = util.List.of(param_main_result$0)
      override def getParameterNames: util.List[String] = util.List.of("main_result$0")
    }
  }
}