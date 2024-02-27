package inca.casestudy.util

import inca.util.CSVUtil.CSV

import java.io.IOException

object Util {
  def toCSV(vals: Seq[(String, IndexedSeq[Long])]): CSV = {
    val header = vals.map(_._1).toIndexedSeq
    // we assume that each list has same number of elements
    val rowLength = vals.head._2.size
    val rows = for (i <- 0 until rowLength) yield vals.map(_._2(i)).toIndexedSeq
    header +: rows
  }

  def collectGarbage(): Unit = {
    System.gc()
    try {
      Thread.sleep(2000)
    } catch {
      case e: IOException => e.printStackTrace()
    }
  }
}
