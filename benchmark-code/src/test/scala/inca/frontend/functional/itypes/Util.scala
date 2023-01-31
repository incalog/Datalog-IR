package inca.frontend.functional.itypes

object Util {
  def time[A](f: => A): Long = {
    val start = System.currentTimeMillis()
    f
    val end = System.currentTimeMillis()
    end - start
  }

}
