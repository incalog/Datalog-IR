package inca.caseStudies.typing

import inca.runtime.context.DataModel
import truechange.{JavaLitType, SortType}
import truediff.Diffable
import truediff.macros.diffable

import scala.collection.immutable.MultiDict

sealed trait Type
object Type {
  case object Int extends Type
  case class Fun(t1: Type, t2: Type) extends Type
}

sealed trait Context
object Context {
  case object Empty extends Context
  case class Bind(name: String, ty: Type, rest: Context) extends Context
}

@diffable
case class Prog(e: Exp) extends Diffable
object Prog {
  val progTag = classOf[Prog].getCanonicalName

  val dataModel: DataModel = {
    val expType = SortType(Exp.expTag)
    val intLitType = SortType(Exp.intLitTag)
    val addType = SortType(Exp.addTag)
    val varType = SortType(Exp.varTag)
    val lamType = SortType(Exp.lamTag)
    val appType = SortType(Exp.appTag)
    val letType = SortType(Exp.letTag)

    val typeExpType = SortType(TypeExp.typeExpTag)
    val intType = SortType(TypeExp.intTag)
    val funType = SortType(TypeExp.funTag)

    new DataModel(
      Set(expType, intLitType, addType, varType, lamType, appType, letType, typeExpType, intType, funType),
      MultiDict[SortType, SortType](
        intLitType -> expType,
        varType -> expType,
        addType -> expType,
        lamType -> expType,
        appType -> expType,
        letType -> expType,
        intType -> typeExpType,
        funType -> typeExpType
      ),
      Map(
        (Prog.progTag -> "e") -> expType,
        (Exp.addTag->"e1") -> expType,
        (Exp.addTag->"e2") -> expType,
        (Exp.lamTag->"ty") -> typeExpType,
        (Exp.lamTag->"body") -> expType,
        (Exp.appTag->"e1") -> expType,
        (Exp.appTag->"e2") -> expType,
        (Exp.letTag->"bound") -> expType,
        (Exp.letTag->"body") -> expType,
        (TypeExp.funTag->"ty1") -> typeExpType,
        (TypeExp.funTag->"ty2") -> typeExpType,
      ),
      Map(
        (Exp.intLitTag->"value") -> JavaLitType(classOf[java.lang.Integer]),
        (Exp.varTag -> "name") -> JavaLitType(classOf[java.lang.String]),
        (Exp.lamTag -> "name") -> JavaLitType(classOf[java.lang.String]),
        // (Exp.lamTag -> "ty") -> JavaLitType(classOf[Type]),
        (Exp.letTag -> "name") -> JavaLitType(classOf[java.lang.String]),
      )
    )
  }
}

@diffable
sealed trait TypeExp extends Diffable
object TypeExp {
  case class Int() extends TypeExp
  case class Fun(ty1: TypeExp, ty2: TypeExp) extends TypeExp
  val typeExpTag = classOf[TypeExp].getCanonicalName
  val intTag = classOf[Int].getCanonicalName
  val funTag = classOf[Fun].getCanonicalName
}
@diffable
sealed trait Exp extends Diffable
object Exp {
  case class IntLit(value: Int) extends Exp
  case class Add(e1: Exp, e2: Exp) extends Exp
  case class Var(name: String) extends Exp
  case class Lam(name: String, ty: TypeExp, body: Exp) extends Exp
  case class App(e1: Exp, e2: Exp) extends Exp
  case class Let(name: String, bound: Exp, body: Exp) extends Exp



  val expTag = classOf[Exp].getCanonicalName
  val intLitTag = classOf[IntLit].getCanonicalName
  val addTag = classOf[Add].getCanonicalName
  val varTag = classOf[Var].getCanonicalName
  val lamTag = classOf[Lam].getCanonicalName
  val appTag = classOf[App].getCanonicalName
  val letTag = classOf[Let].getCanonicalName
}
