package inca.backend.lowering

import inca.ir.{Module, Param, Relation, TAny, Type}
import inca.runtime.Query.Specification
import org.eclipse.viatra.query.runtime.matchers.context.IInputKey
import org.eclipse.viatra.query.runtime.matchers.psystem.PBody
import org.eclipse.viatra.query.runtime.matchers.psystem.queries.{BasePQuery, PParameter, PVisibility}

import java.util
import scala.collection.mutable
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

    var code = '{
      //import inca.runtime.Query.Specification
      //import org.eclipse.viatra.query.runtime.matchers.psystem.queries.{BasePQuery, PParameter}

      ${funs.reduce((a, b) => '{$a;$b})}
    }

    /*code = '{
      var x = scala.collection.mutable.Map[String, Int]()
      x("a") = 5
    }*/

    println("The code...")

    /*code = '{
      new BasePQuery(PVisibility.PUBLIC):
        override def doGetContainedBodies(): util.Set[PBody] = ???
        override def getFullyQualifiedName: String = ???
        override def getParameters: util.List[PParameter] = ???
    }*/

    import quotes.reflect.asTerm
    println(code.asTerm.show)
    code
  }

  /*private def genInputKeyAndType(typ: Datalog.Type): Option[(meta.Term, meta.Term)] = typ match {
    case TAny => None
    case _: TScala => None
    case _: TData => None
    case tlit@TLiteral(litType) =>
      litType match {
          case JavaLitType(cl) =>
          val gentyp = q"$oPrimitiveType(classOf[${tlit.asScala}])"
          Some(q"$oPrimitiveKey($gentyp)", gentyp)
        case _ => throw new UnsupportedOperationException
      }
    case _: TLinked =>
      val gentyp = genNodeType(typ)
      Some(q"$oNodeTypeKey($gentyp)", gentyp)
  }*/

  inline private def genInputKeyAndType(typ: Type)(implicit quotes: Quotes): Option[(Expr[IInputKey], Expr[String])] = typ match {
    case TAny => None
    /*case tlit@TLiteral(litType) =>
      litType match {
        case JavaLitType(cl) =>
          val gentyp = q"$oPrimitiveType(classOf[${tlit.asScala}])"
          Some(q"$oPrimitiveKey($gentyp)", gentyp)
        case _ => throw new UnsupportedOperationException
      }
    case _: TLinked =>
      val gentyp = genNodeType(typ)
      Some(q"$oNodeTypeKey($gentyp)", gentyp)*/
  }

  inline private def genPParam(param: Param)(using quotes: Quotes): Expr[PParameter] = { // quotes.reflect.ValDef = { // Expr[PParameter] = {
    val paramName = Expr(param.name.name)
    val pparam = genInputKeyAndType(param.ty) match {
      case Some((key, gentyp)) =>
        '{ new PParameter($paramName, $gentyp, $key) }
      case None =>
        '{ new PParameter($paramName) }
    }

    pparam

    //import quotes.reflect.*
    //val valSymbol = Symbol.newVal(Symbol.spliceOwner, s"$PARAMPREFIX${param.name}", TypeRepr.of[PParameter], Flags.EmptyFlags, Symbol.noSymbol)
    //ValDef(valSymbol, Some(pparam.asTerm))
  }

  private def compileRelation(moduleName: String, relation: Relation)(implicit env: RuleEnvironment): MetaRelation = {
    val qname = s"${moduleName}_${relation.name}"

    val relName: String = qname//.toLowerCase()

    import quotes.reflect.*
    import quotes.reflect.asTerm

    val paramDefs = relation.params.map(p => Expr(p.name.name) -> genPParam(p)).toList

    val body = '{
      new Specification(new BasePQuery(PVisibility.PUBLIC) {
        // I have not found a way to insert code without a block aka directly into the class
        // body. Therefore we use a map and fill it inside a block
        val pparams: Map[String, PParameter] = Map[String, PParameter](
          ${ paramDefs.map { case (p, pparam) =>
            '{ ($p, $pparam) }
          }.reduce((a, b) => '{$a;$b}) }
        )

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
