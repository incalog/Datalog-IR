package org.inca.generator

import java.io.{File, FileWriter, PrintWriter}

import scala.meta.Source

object Util {
  def writeClass(source: Source, name: String): Unit = {
    val file = new File(s"src/test/scala/org/inca/generator/generated/$name.scala")
    if (!file.exists) {
      if (file.createNewFile)
        new FileWriter(file) {
          write(source.toString())
          close()
        }
    } else {
      new FileWriter(file, false) {
        write(source.toString())
        close()
      }
    }

  }


  def printClass(source: Source): Unit = {
    println(source)
  }
}
