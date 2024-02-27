package inca.frontend.functional.measurements.itypes

object Util {
  def time[A](f: => A): Long = {
    val start = System.currentTimeMillis()
    f
    val end = System.currentTimeMillis()
    end - start
  }

}
