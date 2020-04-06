package org.inca.gen.gp.helper

import scala.meta._

// todo move this to typeconstraints or variables file
object Util {
  def asTypeSelect(path: String): Type.Select =
    checkLength(path.substring(1).split('.').toList)

  private def checkLength(pathList: List[String]): Type.Select =
    if (pathList.length > 2) Type.Select(asTermSelect(pathList.init), Type.Name(pathList.last))
    else Type.Select(Term.Name(pathList.head), Type.Name(pathList.last))

  def asTermSelect(pathList: List[String]): Term.Select =
    pathList
      .drop(2)
      .foldLeft(Term.Select(Term.Name(pathList.head), Term.Name(pathList.tail.head)))
        { (inner, outer) => Term.Select(inner, Term.Name(outer)) }
}
