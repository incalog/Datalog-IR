package inca.util

import java.io.IOException

object MeasurementUtils {

  def usedMemoryInBytes(): Long = {
    val total = Runtime.getRuntime.totalMemory()
    val free = Runtime.getRuntime.freeMemory()
    total - free
  }

  def usedMemoryInMBytes(): Long = {
    System.gc()
    try {
    Thread.sleep(2000)
    } catch {
      case e: IOException => e.printStackTrace()
    }
    usedMemoryInBytes() / (1024 * 1024)
  }
}
