package org.inca.generator

import java.io.{File, FileWriter}

import scala.meta.Source
import org.inca.gen.Pipeline.generateGraphPattern
import org.inca.lang.Gp.GraphPattern

object Util {
  def writeClass(pattern: GraphPattern): Unit = {
    val source = generateGraphPattern(pattern)
    val name = pattern.name

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
