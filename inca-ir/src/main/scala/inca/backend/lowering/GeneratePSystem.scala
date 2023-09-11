package inca.backend.lowering

import inca.ir.{Module, Relation}
import inca.runtime.Query.Specification
import org.eclipse.viatra.query.runtime.matchers.psystem.PBody
import org.eclipse.viatra.query.runtime.matchers.psystem.queries.{BasePQuery, PParameter, PVisibility}

import java.util
import scala.quoted.*

object GeneratePSystem {
  val PARAMPREFIX = "param_"
  val VARPREFIX = "var_"
  val LITPREFIX = "lit_"
  val EVALPREFIX = "eval_"

  /** Maps rule name to the name of the module that defines it. */
  type RuleEnvironment = Map[String, String]
  type MetaModule = Quotes ?=> Expr[Any]
  type MetaRelation = Quotes ?=> Expr[Any]

  def compileModules(modules: Seq[Module]): Seq[MetaModule] = {
    val env: RuleEnvironment = modules.flatMap(m => m.relations.map(p => p.name.name -> m.name.name)).toMap
    modules.map { m =>
      compileModule(m)(using env)
    }
  }

  def compileModule(module: Module)(implicit env: RuleEnvironment): MetaModule = {
    // makes sure this module's names are found first
    val myenv = env ++ module.relations.map(r => r.name.name -> module.name.name)
    val funs = module.relations.map(r => compileRelation(module.name.name, r)(using env))

    val code = '{
      //import inca.runtime.Query.Specification

      ${funs.reduce((a, b) => '{$a;$b})}
    }

    println("The code...")

    import quotes.reflect.asTerm
    println(code.asTerm.show)
    code
  }

  private def compileRelation(moduleName: String, relation: Relation)(implicit env: RuleEnvironment): MetaRelation = {
    val qname = s"${moduleName}_${relation.name}"

    //val paramNames = relation.params.map(_.name)
    val relName: String = qname//.toLowerCase()

    import quotes.reflect.*
    import quotes.reflect.asTerm

    val body = '{
      new Specification(new BasePQuery(PVisibility.PUBLIC) {
        override def doGetContainedBodies(): util.Set[PBody] = ???
        override def getFullyQualifiedName: String = ???
        override def getParameters: util.List[PParameter] = ???
      })
    }
    val valSymbol = Symbol.newVal(Symbol.spliceOwner, relName, TypeRepr.of[Specification], Flags.EmptyFlags, Symbol.noSymbol)
    val valDef = ValDef(valSymbol, Some(body.asTerm))

    val relCode: Quotes ?=> Expr[Any] = Block(
        List(valDef),
        '{()}.asTerm
        //Ref(valSymbol)
      ).asExpr

    /*val relCode: Quotes ?=> Expr[Any] = '{
      ${Block(
        List(valDef),
        Ref(valSymbol)
      ).asExpr}
    }*/

    relCode
  }

}
