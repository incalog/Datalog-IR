package inca.analyzedLangs

import inca.runtime.context.DataModel
import truechange.{JavaLitType, ListType, SortType}
import truediff.Diffable
import truediff.macros.diffable

import scala.collection.immutable.MultiDict

@diffable
trait Exp extends Diffable
object Exp {
  case class BooleanLit(value: Boolean) extends Exp
  case class IntegerLit(value: Int) extends Exp
  case class LongLit(value: Long) extends Exp
  case class Mul(lhs: Exp, rhs: Exp) extends Exp
  case class Add(lhs: Exp, rhs: Exp) extends Exp
  case class Not(e: Exp) extends Exp
  case class And(lhs: Exp, rhs: Exp) extends Exp
  case class Or(lhs: Exp, rhs: Exp) extends Exp
  case class Many(exps: List[Exp]) extends Exp
  case class Let(name: String, bound: Exp, body: Exp) extends Exp

  val expTag = classOf[Exp].getCanonicalName
  val intTag = classOf[IntegerLit].getCanonicalName
  val longTag = classOf[LongLit].getCanonicalName
  val boolTag = classOf[BooleanLit].getCanonicalName
  val addTag = classOf[Add].getCanonicalName
  val multTag = classOf[Mul].getCanonicalName
  val andTag = classOf[And].getCanonicalName
  val orTag = classOf[Or].getCanonicalName
  val notTag = classOf[Not].getCanonicalName
  val manyTag = classOf[Many].getCanonicalName
  val letTag = classOf[Let].getCanonicalName

  val model: DataModel = {
    val expType = SortType(expTag)
    val intType = SortType(intTag)
    val longType = SortType(longTag)
    val boolType = SortType(boolTag)
    val addType = SortType(addTag)
    val multType = SortType(multTag)
    val andType = SortType(andTag)
    val orType = SortType(orTag)
    val notType = SortType(notTag)
    val manyType = SortType(manyTag)
    val letType = SortType(letTag)
    new DataModel(
      Set(expType, intType, longType, boolType, multType, addType, andType, orType, notType, manyType, letType),
      MultiDict[SortType, SortType](
        intType -> expType,
        longType -> expType,
        boolType -> expType,
        multType -> expType,
        addType -> expType,
        andType -> expType,
        orType -> expType,
        notType -> expType,
        manyType -> expType,
        letType -> expType
      ),
      Map(
        (addTag->"lhs") -> expType,
        (addTag->"rhs") -> expType,
        (multTag->"lhs") -> expType,
        (multTag->"rhs") -> expType,
        (andTag->"lhs") -> expType,
        (andTag->"rhs") -> expType,
        (orTag->"lhs") -> expType,
        (orTag->"rhs") -> expType,
        (notTag->"e") -> expType,
        (manyTag->"exps") -> ListType(expType),
        (letTag -> "bound") -> expType,
        (letTag -> "body") -> expType
      ),
      Map(
        (intTag->"value") -> JavaLitType(classOf[java.lang.Integer]),
        (longTag->"value") -> JavaLitType(classOf[java.lang.Long]),
        (boolTag->"value") -> JavaLitType(classOf[java.lang.Boolean]),
        (letTag -> "name") -> JavaLitType(classOf[java.lang.String])
      )
    )
  }
}