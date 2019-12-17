package org.inca.generators.gp

import org.inca.gp.Content.GraphPattern

import scala.meta._

class GPGenerator {
  val filename = "test.scala"

  def generate(pattern: GraphPattern, collectionName: String): Defn.Class = {


    val fullyQualifiedName = s"""$collectionName.${pattern.name}"""
    // todo maybe refactor


    /*
     * GeneratedPQuery -> PParameter
     */
    var generatedPQuery_body: List[Stat] = List()
    for (param <- pattern.parameters) {
      val paramName = Pat.Var(Term.Name(s"p_${param.name}"))
      val paramNameString = Lit.String(s"p_${param.name}")
      val paramFullyQualifiedName = Lit.String(param.typ.get.toString.substring(1))
      val paramConceptKey = q"ConceptKey()"
      val paramValue = q"PParameter($paramNameString, $paramFullyQualifiedName, $paramConceptKey)"
      val parVal = q"val $paramName: PParameter = $paramValue"

      generatedPQuery_body = generatedPQuery_body ++ List(parVal)
    }

    /*
     * GeneratedPQuery -> doGetContainedBodies
     */
    var doGetContainedBodies_bodies: List[Stat] = List()

    for (body <- pattern.bodies) {
      var bodyList: List[Stat] = List()
      val PBody_body = q"val body: PBody = PBody(this)"
      bodyList = bodyList ++ List(PBody_body)


      for (param <- pattern.parameters) {
        val paramName =Lit.String(param.name)
        val param_var_name = Pat.Var(Term.Name(s"var_${param.name}"))

        val pVariable_var_list_entry = q"val $param_var_name: PVariable = body.getOrCreateVariableByName($paramName)"

        bodyList = bodyList ++ List(pVariable_var_list_entry)
      }

      val body_content = q"{ ..$bodyList }"
      doGetContainedBodies_bodies = doGetContainedBodies_bodies ++ List{body_content}
    }

    val doGetContainedBodies_body = q"{..$doGetContainedBodies_bodies}"

    val doGetContainedBodies = q"override def doGetContainedBodies(): Set[PBody] = $doGetContainedBodies_body"

    generatedPQuery_body = generatedPQuery_body ++ List(doGetContainedBodies)

    /*
     * GeneratedPQuery -> INSTANCE Variable
     */

    //    val instanceValName = Type.Name(s"${pattern.name}_${collectionName}QuerySpecification.GeneratedPQuery")
    //    val instanceVal = q"val INSTANCE: $instanceValName = ${className}.GeneratedPQuery()"

    /*
     * GeneratedPQuery
     */
    val innerClass = q"class GeneratedPQuery extends AbstractPQuery { ..$generatedPQuery_body }"

    /*
     * class
     */
    val className = Type.Name(s"${pattern.name}_${collectionName}QuerySpecification")
    val rootClass = q"class $className extends ScalaQuerySpecification { $innerClass }"

    println(rootClass)

    rootClass
  }
}
