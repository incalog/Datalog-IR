package inca.util

import java.util
import java.util.HashMap
import scala.annotation.targetName

trait Decorator[O]:
  val hasInput: Boolean

trait DecoratorWithInput[I, O] extends Decorator[O]:
  override val hasInput: Boolean = true
  val f: I => O
  def apply(input: I): O = f(input)

trait DecoratorWithoutInput[O] extends Decorator[O]:
  override val hasInput: Boolean = false
  val f: () => O
  def apply(): O = f()

trait DecoratorWithOrWithoutInput[I, O] extends Decorator[O]:
  val f: Either[() => O, I => O]
  override val hasInput: Boolean = f.isRight

  def apply(): O = apply(Left(()))
  def apply(input: I): O = apply(Right(input))
  def apply(input: Either[Unit, I]): O = (f, input) match
    case (Left(fun), _) => fun()
    case (Right(fun), Right(args)) => fun(args)
    case (Right(_), Left(_)) => throw IllegalStateException("Argument mismatch")


case class Memoize[I, O](f: I => O, capacity: Int, loadFactor: Float) extends DecoratorWithInput[I, O]:
  var cache: util.HashMap[I, O] = util.HashMap(capacity, loadFactor)

  def clearCache(): Unit = cache.clear()
  def clearCache(input: I): Unit = cache.remove(input)
  override def apply(input: I): O =
    if (cache.containsKey(input))
      cache.get(input)
    else
      val out = super.apply(input)
      cache.put(input, out)
      out


case class TimeIt[I, O](f: Either[() => O, I => O]) extends DecoratorWithOrWithoutInput[I, O]:
  var durationsInNS: Seq[Long] = Seq()
  var lastDurationInNS: Option[Long] = durationsInNS. headOption

  def reset(): Unit = durationsInNS = Seq()
  override def apply(input: Either[Unit, I]): O =
    val startTime = System.nanoTime()
    val result = super.apply(input)
    val endTime = System.nanoTime()
    durationsInNS +:= endTime - startTime
    result


def timeIt[O](f: () => O): TimeIt[Unit, O] = TimeIt(Left(f))
def timeIt[I, O](f: I => O): TimeIt[I, O] = TimeIt(Right(f))
// Scala struggles to choose the correct method when the function has no arguments.
// Either you must add a type annotation or you can use this function here.
def timeItUnit[O](f: () => O): TimeIt[Unit, O] = timeIt[O](f)

def memoize[I, O](f: I => O, capacity: Int = 100, loadFactor: Float = 0.8): Memoize[I, O] = Memoize(f, capacity, loadFactor)
