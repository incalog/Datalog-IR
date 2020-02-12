package org.inca.gen.gp.util

import scala.meta._

object Util {

  def classPathToTypeSelect(path: String): Type.Select =
    asTypeSelect(path.substring(1).split('.').toList)

  def importToImporter(importList: List[ImportItem]): List[Stat] =
    importList.map { item =>
      Import(List(
        Importer(innerTermSelect(item.qualifier.split('.').toList),
        item.imports.map { imp => Importee.Name(Name.Indeterminate(imp)) })
      ))
    }

  private def asTypeSelect(pathList: List[String]): Type.Select =
    if (pathList.length > 2) {
      Type.Select(innerTermSelect(pathList.init), Type.Name(pathList.last))
    } else {
      Type.Select(Term.Name(pathList.head), Type.Name(pathList.last))
    }

  private def innerTermSelect(pathList: List[String]): Term.Select = {
    val innerTermSelect = Term.Select(Term.Name(pathList.head),
      Term.Name(pathList.tail.head))

    pathList.drop(2).foldLeft(innerTermSelect) { (inner, outer) =>
      Term.Select(inner, Term.Name(outer))
    }
  }

}

case class ImportItem(qualifier: String, imports: List[String])
