package inca.ir.execution

object ThreadCount:
  def numberOfAvailableThreads(): Int = Runtime.getRuntime.availableProcessors()

enum ThreadCount:
  case Auto // Use the maximum number of available threads
  case Fixed(n: Int)

  def requiresParallelExec: Boolean = this match
    case Auto => true
    case Fixed(n) => n > 1

implicit def int2ThreadCount(n: Int): ThreadCount = ThreadCount.Fixed(n)