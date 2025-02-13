package inca.ir.optimize

import inca.ir.Module
import inca.ir.visitors.BaseIRVisitor

import scala.collection.mutable

trait Optimizer extends BaseIRVisitor:
  val stats: mutable.Map[String, Any] = mutable.Map.empty

  def statsString: String = stats.mkString("{", ", ", "}")

  def logOptimizationStat[A](key: String, default: A, update: A => A): Unit =
    stats.get(key) match
      case None => stats.put(key, default)
      case Some(a) => stats.put(key, update(a.asInstanceOf[A]))

  def analyzeProgram(modules: Seq[Module]): Unit = 
    // println(s"[Info:] $name does not implement an analysis phase!")
    () // default is nothing
