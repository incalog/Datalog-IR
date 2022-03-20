package inca.util.measurement

import java.io.IOException

object MemoryUtil {
  def usedMemoryInBytes(): Long = {
    val total = Runtime.getRuntime.totalMemory()
    val free = Runtime.getRuntime.freeMemory()
    total - free
  }

  def collectGarbage(): Unit = {
    System.gc()
    try {
      Thread.sleep(2000)
    } catch {
      case e: IOException => e.printStackTrace()
    }
  }

  def usedMemoryInMBytes(): Long = {
    usedMemoryInBytes() / (1024 * 1024)
  }
}
