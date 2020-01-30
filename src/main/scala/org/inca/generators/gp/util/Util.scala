package org.inca.generators.gp.util

import scala.meta.{Import, Importee, Importer, Name, Stat, Term, Type}

object Util {

  def classPathToTypeSelect(path: String): Type.Select = {
    val pathList = path.split('.')

    // todo fold
    if (pathList.length > 2) {
      Type.Select(recursiveTermSelect(pathList.toList.dropRight(1)), Type.Name(pathList.last))
    } else {
      Type.Select(Term.Name(pathList.head), Type.Name(pathList.last))
    }
  }

  def importToImporter(importList: List[ImportItem]): List[Stat] = {

    for (item <- importList) yield {
      Import(List(
        Importer(
          recursiveTermSelect(item.qualifier.split('.').toList),
          for (imp <- item.imports) yield {
            Importee.Name(Name.Indeterminate(imp))
          }
        )
      )
      )
    }
  }

  private def recursiveTermSelect(pathList: List[String]): Term.Select = {
    if (pathList.length == 2) {
      Term.Select(Term.Name(pathList.head), Term.Name(pathList.last))
    } else {
      Term.Select(recursiveTermSelect(pathList.dropRight(1)), Term.Name(pathList.last))
    }
  }
}


case class ImportItem(val qualifier: String, val imports: List[String])
