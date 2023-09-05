package inca.util

object TimeTracker {
  var end: Long = -1
  var start: Long = -1

  def begin(): Unit = {
    start = System.currentTimeMillis()
  }
  def stop(): Unit = {
    end = System.currentTimeMillis()
  }
  def clear(): Unit = {
    start = -1
    end = -1
  }

  // def measurementInNano: Long = end - start
  def measurementInMilli: Long = end - start
  def measurementInSeconds: Long = measurementInMilli / 1000

  def measure[T](name: String, f: () => T): T = {
    val start = System.nanoTime()
    val res = f()
    val end = System.nanoTime()
    println(s"$name: ${(end - start).toDouble / (1000 * 1000)}")
    res
  }
}
