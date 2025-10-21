package inca.ir.analysis

import inca.ir
import inca.ir.{ExtensionalRelation, Name, Param}
import inca.ir.analysis.base.interpreter.BaseGenericInterpreter
import inca.ir.analysis.base.values.{AbstractRelation, Value}
import inca.ir.analysis.{AbstractEdbConfig, EdbConfig}
import inca.ir.optimize.{BaseIROptimizer, Optimizer}
import inca.ir.visitors.IRVisitor
import inca.util.collectGarbage
import sturdy.effect.failure.AFallible

trait IRMeasureInterpreter[V, RV] extends IRVisitor with Optimizer:
  val warmups: Int = 10
  val runs: Int = 10

  val edbConfig: EdbConfig[RV]

  def freshAbstractInterpreter(): BaseGenericInterpreter[V, ?, RV, ?, ?]

  private def measure(modules: Seq[ir.Module]) =
    val abstractInterpreter = freshAbstractInterpreter()
    // fill edb
    modules.foreach { m =>
      m.entries.foreach {
        case (_, ExtensionalRelation(n, params)) =>
          val aRel = edbConfig.abstractExtensionalRelation(n, params)
          abstractInterpreter.insertEDB(n.name, aRel)
        case _ => // nothing
      }
    }

    val optStart = System.currentTimeMillis()
    val analysisRes = abstractInterpreter.failure.fallible {
      abstractInterpreter.evalProgram(modules)
    }
    val optTime = System.currentTimeMillis() - optStart

    analysisRes match {
      case AFallible.Failing(failures) =>
        val msg = failures.map { (kind, message) =>
          s"[$kind]: $message"
        }.set.mkString("\n")
        throw AnalysisFailed(msg)
      case AFallible.Diverging(recur) =>
        throw IllegalStateException(s"Diverging: $recur")
      case _ => // nothing
    }
    optTime

  override def analyzeProgram(modules: Seq[ir.Module]): Unit =
    if (!isClosedWorld)
      ()
    else
      for (i <- 0 until warmups) {
        val t = measure(modules)
        println(s"Warmup: $i :: $t")
        collectGarbage()
      }
      val execTimes = for (i <- 0 until runs) yield {
        val t = measure(modules)
        println(s"Run: $i :: $t")
        collectGarbage()
        t
      }
      println(s"Analysis times: $execTimes")
      System.exit(1)


class IRMeasureConstantAnalysis extends IRMeasureInterpreter[Value, AbstractRelation]:
  override val edbConfig: EdbConfig[AbstractRelation] =
    // Assume everything in the edb is top
    AbstractEdbConfig.default

  override def freshAbstractInterpreter(): BaseGenericInterpreter[Value, ?, AbstractRelation, ?, ?] =
    new IRConstantAbstractInterpreter(logTraversalTrace = false, logControlEvents = false, interRelational = true)


class IRMeasureTypeAnalysis extends IRMeasureInterpreter[Value, AbstractRelation]:
  override val edbConfig: EdbConfig[AbstractRelation] = TypeEdbConfig.default

  override def freshAbstractInterpreter(): BaseGenericInterpreter[Value, ?, AbstractRelation, ?, ?] =
    new IRTypeAbstractInterpreter(logTraversalTrace = false, interRelational = true)
    
class IRMeasureDataKindAnalysis extends IRMeasureInterpreter[Value, AbstractRelation]:
  override val edbConfig: EdbConfig[AbstractRelation] =
    // Assume everything in the edb is top
    AbstractEdbConfig.default

  override def freshAbstractInterpreter(): BaseGenericInterpreter[Value, ?, AbstractRelation, ?, ?] =
    new IRDataKindAbstractInterpreter(logTraversalTrace = false, logControlEvents = false, interRelational = true)
  