package inca.utils

object JSONReader {

  val nodeTypeData: ujson.Value = ujson.read(os.read(os.pwd / "abstract-treesitter-truediff-API" / "src" / "main" / "resources" / "node-types" / "node-types.json"))

  def getFieldEntries(nodeName: String): Array[String] = {
    val node = nodeTypeData.arr.filter { v => v("type").str == nodeName }
    val nodeInfo = node(0).obj
    if (nodeInfo.contains("fields")) {
      if (nodeInfo("fields").obj.nonEmpty) {
        nodeInfo("fields").obj.keys.toArray
      } else {
        Array[String]()
      }
    } else {
      Array[String]()
    }
  }
}
