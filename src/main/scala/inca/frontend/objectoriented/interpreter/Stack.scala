package inca.frontend.objectoriented.interpreter

import scala.annotation.tailrec
import scala.collection.mutable

trait MaybeChanged[Out] {
  def get: Out
}
case class Unchanged[Out](result: Out) extends MaybeChanged[Out] {
  override def get: Out = result
}
case class Changed[Out](result: Out) extends MaybeChanged[Out] {
  override def get: Out = result
}

case object RecurrentCall extends Exception

trait Stack[In, Out] {
  trait PushResult
  case class Recurrent(previousOut: Option[Out]) extends PushResult
  case object Continue extends PushResult

  trait PopResult
  case object Stable extends PopResult
  case object Unstable extends PopResult

  def push(in: In): PushResult
  def pop(in: In, out: Out): PopResult
  def height: Int

  @tailrec
  final def fix(in: In, default: => Out)(f: In => Out): Out = push(in) match {
    case Recurrent(previousOut) =>
      previousOut.getOrElse(default)
    case Continue =>
      val out = f(in)
      pop(in, out) match {
        case Stable => out
        case Unstable => fix(in, default)(f)
      }
  }

  val DEBUG: Boolean = false
  def debug(msg: String): Unit = if (DEBUG) println("  " * height + msg)
}

class StackImpl[In, Out](implicit join: ((Out, Out) => MaybeChanged[Out])) extends Stack[In, Out] {
  private val stack: mutable.Map[In, Int] = mutable.Map()
  private var stackHeight = 0
  private val outCache: mutable.Map[In, Out] = mutable.Map()
  private val corecurrent: mutable.Set[Int] = mutable.BitSet()

  override def height: Int = stackHeight

  override def push(in: In): PushResult = stack.get(in) match {
    case None =>
      debug(s"PUSH $in")
      stack.put(in, stackHeight)
      stackHeight += 1
      Continue
    case Some(originalCall) =>
      corecurrent += originalCall
      val previousOut = outCache.get(in)
      debug(s"Recurrent $in  ->  $previousOut")
      Recurrent(previousOut)
  }

  override def pop(in: In, out: Out): PopResult = {
    stack -= in
    val newStackHeight = stackHeight - 1
    stackHeight = newStackHeight
    val isCorrecurrent = corecurrent.remove(newStackHeight)

    if (!isCorrecurrent) {
      debug(s"POP STABLE $in  ->  $out  (no recurrent call occurred)")
      Stable
    } else {
      outCache.get(in) match {
        case None =>
          debug(s"POP UNSTABLE $in  ->  $out  (first successful run)")
          outCache.put(in, out)
          Unstable
        case Some(previousOut) =>
          join(previousOut, out) match {
            case Unchanged(_) =>
              debug(s"POP STABLE $in  ->  $out  (corecurrent stabilized)")
              Stable
            case Changed(joined) =>
              debug(s"POP UNSTABLE $in  ->  $out  (changed from previous out $previousOut)")
              outCache.put(in, joined)
              Unstable
          }
      }
    }
  }
}