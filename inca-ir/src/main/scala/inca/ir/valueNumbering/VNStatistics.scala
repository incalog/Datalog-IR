package inca.ir.valueNumbering

import inca.ir.{Module, Term}
import inca.ir.visitors.IRVisitor
import inca.util.{Classic, Markdown, Tabulator}


class VNStatistics(input: Module, output: Module) {

  def printStatistics(): Unit = {
    val relations = Seq("Number of Relations") ++ numberOfRelations()
    val bodies = Seq("Number of Bodies") ++ numberOfBodies()
    val atoms = Seq("Number of Atoms") ++ numberOfAtoms()
    val vars = Seq("Number of Vars") ++ numberOfVars() // number of different variables in each body
    val terms = Seq("Number of Terms") ++ numberOfTerms()
    println(
      Tabulator.format("VN Statistics", Seq("", "before", "after:"), Seq(relations, bodies, atoms, vars, terms), Classic)
    )
  }


  private def countRelations(module: Module): Int = module.relations.size

  private def numberOfRelations(): Seq[Int] = {
    val countRelationsBefore = countRelations(input)
    val countRelationsAfter = countRelations(output)
    Seq(countRelationsBefore, countRelationsAfter)
  }


  private def countBodies(module: Module): Int = module.relations.values.map {
    _.bodies.size
  }.sum

  private def numberOfBodies(): Seq[Int] = {
    val countBodiesBefore = countBodies(input)
    val countBodiesAfter = countBodies(output)
    Seq(countBodiesBefore, countBodiesAfter)
  }


  private def countAtoms(module: Module): Int = module.relations.values.map {
    _.bodies.map(_.atoms.size).sum
  }.sum

  def numberOfAtoms(): Seq[Int] = {
    val countAtomsBefore = countAtoms(input)
    val countAtomsAfter = countAtoms(output)
    Seq(countAtomsBefore, countAtomsAfter)
  }


  private def countVars(module: Module): Int = module.relations.values.map {
    _.bodies.map(_.atoms.flatMap(_.vars).distinct.size).sum
  }.sum

  def numberOfVars(): Seq[Int]  = {
    val countVarsBefore = countVars(input)
    val countVarsAfter = countVars(output)
    Seq(countVarsBefore, countVarsAfter)
  }


  def numberOfTerms(): Seq[Int] = {
    class TermCounter extends IRVisitor {
      var count = 0
      override def visitTerm(term: Term): Seq[Term] = {
        count += 1
        super.visitTerm(term)
      }
      def countTerms(module: Module): Int = {
        count = 0
        visitModule(module)
        count
      }
    }
    val counter = TermCounter()
    val countTermsBefore = counter.countTerms(input)
    val countTermsAfter = counter.countTerms(output)
    Seq(countTermsBefore, countTermsAfter)
  }





}
