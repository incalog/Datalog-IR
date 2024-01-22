package inca.foreign.scala.visitors

import inca.ir.Module
import inca.ir.visitors.StatisticsCollector

class ScalaStatisticsCollector extends StatisticsCollector with ScalaVisitor

object ScalaStatisticsCollector:
  def printStatistics(m: Module, hint: String): Unit =
    val s = new ScalaStatisticsCollector
    s.visitProgram(Seq(m))
    println(
      s"""Statistics for ${m.name} $hint:
         |\tRelations: ${s.relations}
         |\tBodies:    ${s.bodies}
         |\tAtoms:     ${s.atoms}
         |\tTerms:     ${s.terms}""".stripMargin)