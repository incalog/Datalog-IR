package org.inca.gen.gp.helper

import java.io.{File, PrintWriter}

import scala.meta._

object Util {

  def writeClass(source: Source, name: String): Unit = {
    val file = new File(s"src/test/scala/org/inca/generatorgenerated/$name.scala")
    if (!file.exists)
      if (file.createNewFile)
        new PrintWriter(file) {
          write(source.toString())
          close()
        }
    else {
      new PrintWriter(file) {
        write(source.toString())
        close()
      }
    }
  }

  def asTypeSelect(path: String): Type.Select =
    checkLength(path.substring(1).split('.').toList)

  private def checkLength(pathList: List[String]): Type.Select =
    if (pathList.length > 2) Type.Select(asTermSelect(pathList.init), Type.Name(pathList.last))
    else Type.Select(Term.Name(pathList.head), Type.Name(pathList.last))

  private def asTermSelect(pathList: List[String]): Term.Select =
    pathList
      .drop(2)
      .foldLeft(Term.Select(Term.Name(pathList.head), Term.Name(pathList.tail.head)))
        { (inner, outer) => Term.Select(inner, Term.Name(outer)) }
}
