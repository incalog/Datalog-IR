package inca.ir.visitors
import inca.ir.{Atom, Body, Module, Relation, Term}

object StatisticsCollector:
  def printStatistics(m: Module, hint: String): Unit =
    val s = new StatisticsCollector
    s.visitProgram(Seq(m))
    println(
      s"""Statistics for ${m.name} $hint:
         |\tRelations: ${s.relations}
         |\tBodies:    ${s.bodies}
         |\tAtoms:     ${s.atoms}
         |\tTerms:     ${s.terms}""".stripMargin)
    println()

class StatisticsCollector extends IRVisitor {
  var relations: Int = 0
  var bodies: Int = 0
  var atoms: Int = 0
  var terms: Int = 0

  override def visitRelation(relation: Relation): Seq[Relation] =
    relations += 1
    super.visitRelation(relation)

  override def visitBody(body: Body): Seq[Body] =
    bodies += 1
    super.visitBody(body)

  override def visitAtom(atom: Atom): Seq[Atom] =
    atoms += 1
    super.visitAtom(atom)

  override def visitTerm(term: Term): Seq[Term] =
    terms += 1
    super.visitTerm(term)
}
