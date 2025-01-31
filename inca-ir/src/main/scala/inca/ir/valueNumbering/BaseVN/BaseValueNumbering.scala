package inca.ir.valueNumbering.BaseVN

import inca.ir.*



/** for value numbering constructs from BaseIR */
trait BaseValueNumbering extends BaseValueNumberingRelations {

  def valueNumbering(module: Module): Module = visitModule(module)

  override def visitModule(module: Module): Module = {
    if (printBeforeAfter) println(s"before VN: \n$module\n")

    val result = super.visitModule(module)

    if (printBeforeAfter) println(s"after VN: \n$result")

    printStatistics(module, result)
    result
  }

}
