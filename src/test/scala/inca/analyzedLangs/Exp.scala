package inca.analyzedLangs

import inca.runtime.context.LanguageMetaInfo
import truechange.{JavaLitType, ListType, SortType}
import truediff.Diffable
import truediff.macros.diffable

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
  val languageMetaInfo: LanguageMetaInfo = {
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
    new LanguageMetaInfo(
      Map[SortType, Set[SortType]](
        expType -> Set(),
        intType -> Set(expType),
        longType -> Set(expType),
        boolType -> Set(expType),
        multType -> Set(expType),
        addType -> Set(expType),
        andType -> Set(expType),
        orType -> Set(expType),
        notType -> Set(expType),
        manyType -> Set(expType)
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
        (manyTag->"exps") -> ListType(expType)
      ),
      Map(
        (intTag->"value") -> JavaLitType(classOf[java.lang.Integer]),
        (longTag->"value") -> JavaLitType(classOf[java.lang.Long]),
        (boolTag->"value") -> JavaLitType(classOf[java.lang.Boolean])
      )
    )
  }
}