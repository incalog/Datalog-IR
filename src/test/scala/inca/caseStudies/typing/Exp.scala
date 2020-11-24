package inca.caseStudies.typing

import inca.runtime.context.LanguageMetaInfo
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
sealed trait Exp extends Diffable
object Exp {
  case class Int(value: Int) extends Exp
  case class Add(e1: Exp, e2: Exp) extends Exp
  case class Var(name: String) extends Exp
  case class Lam(name: String, ty: Type, body: Exp) extends Exp
  case class App(e1: Exp, e2: Exp) extends Exp
  case class Let(name: String, bound: Exp, body: Exp) extends Exp

  val expTag = classOf[Exp].getCanonicalName
  val intTag = classOf[Int].getCanonicalName
  val addTag = classOf[Add].getCanonicalName
  val varTag = classOf[Var].getCanonicalName
  val lamTag = classOf[Lam].getCanonicalName
  val appTag = classOf[App].getCanonicalName
  val letTag = classOf[Let].getCanonicalName
  val languageMetaInfo: LanguageMetaInfo = {
    val expType = SortType(expTag)
    val intType = SortType(intTag)
    val addType = SortType(addTag)
    val varType = SortType(varTag)
    val lamType = SortType(lamTag)
    val appType = SortType(appTag)
    val letType = SortType(letTag)
    new LanguageMetaInfo(
      MultiDict[SortType, SortType](
        intType -> expType,
        varType -> expType,
        addType -> expType,
        lamType -> expType,
        appType -> expType,
        letType -> expType
      ),
      Map(
        (addTag->"e1") -> expType,
        (addTag->"e2") -> expType,
        (lamTag->"body") -> expType,
        (appTag->"e1") -> expType,
        (appTag->"e2") -> expType,
        (letTag->"bound") -> expType,
        (letTag->"body") -> expType,
      ),
      Map(
        (intTag->"value") -> JavaLitType(classOf[java.lang.Integer]),
        (varTag -> "name") -> JavaLitType(classOf[java.lang.String]),
        (lamTag -> "name") -> JavaLitType(classOf[java.lang.String]),
        (lamTag -> "ty") -> JavaLitType(classOf[Type]),
        (letTag -> "name") -> JavaLitType(classOf[java.lang.String]),
      )
    )
  }
}