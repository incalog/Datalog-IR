package inca.util

import scala.meta.{Term, Type}
import scala.reflect.ClassTag

object Meta {

  def typeOf[T:ClassTag](implicit tag: ClassTag[T]): Type =
    mkQualTypename(tag.runtimeClass.getCanonicalName)

  def symbolOf[T:ClassTag](implicit tag: ClassTag[T]): Term =
    mkQualName(tag.runtimeClass.getCanonicalName)

  def objectOf(o: Any): Term = {
    val name = o.getClass.getCanonicalName
    mkQualName(name.substring(0, name.length - 1))
  }

  def mkQualName(s: String): Term = {
    val ss = s.split('.')
    var t: Term = Term.Name(ss(0))
    for (i <- 1 until ss.length)
      t = Term.Select(t, Term.Name(ss(i)))
    t
  }

  def mkQualTypename(s: String): Type = {
    val ss = s.split('.')
    if (ss.length == 1)
      return Type.Name(ss(0))

    var qual: Term.Ref = Term.Name(ss(0))
    for (i <- 1 until (ss.length - 1))
      qual = Term.Select(qual, Term.Name(ss(i)))
    Type.Select(qual, Type.Name(ss(ss.length-1)))
  }
}
