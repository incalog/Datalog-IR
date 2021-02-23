package inca.debugger

import inca.compiler.CompiledFunModule
import inca.frontend.core.tree.{Param, PatternFunction}
import inca.runtime.Query.Matcher
import inca.runtime.{Database, DatabaseAccessor}
import truechange.{Type, URI}

import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters.ListHasAsScala


class Debugger(feed: Database, matcher: Matcher, module: CompiledFunModule) { // FIXME choose more meaningful names
  private val db = new DatabaseAccessor(feed)
  private val tagDepth = 2 // FIXME Figure out way to set this. Re-init matches (var) on change?

  private val funParams: Map[String, Seq[Param]] = module.fun.content.map({
    case pf: PatternFunction => (pf.name.name, pf.params)
  }).toMap

  val matches: Seq[Match] = {
    val funName = matcher.getPatternName.replace(module.fun.name + "_", "")

    val bufMatches = new ListBuffer[Match]
    matcher.forEachMatch(mat => {
      val cols = mat.parameterNames().asScala.map(colName => {
        val colValue = mat.get(colName) match {
          case uri: URI => ColURI(uri, getNodeTypes(uri), getTag(uri, tagDepth))
          case scalaType => ColScalaType(scalaType)
        }
        val colType = getColType(funName, colName)
        Column(colName, colValue, colType)

      }).toSeq
      bufMatches += Match(mat, cols)
    })

    bufMatches.toList
  }

  private def getColType(funName: String, colName: String): ColumnType = {
    val params = funParams.getOrElse(funName,
      throw new IllegalArgumentException(s"Function $funName not defined in module"))

    if(params.map(p => p.name.name).contains(colName)) // Function funName defines paramName as parameter
      Input
    else
      Output
  }

  private def getNodeTypes(uri: URI): Seq[Type] = {
    db.nodeInstancesByValue(uri).keys.toSeq
  }

  private def getTag(uri: URI, d: Int): String = { // TODO Optimize
    if (d > 0) {
      val ts = db.linkNodeInstancesByValue1(uri).keys.map(link => stripTag(link._1)) // Link is (Tag:String, name:String)

      if(ts.isEmpty) { // Primitive type
        val pts = db.linkPrimitiveInstancesByValue1(uri)
        pts.keys.map(link => stripTag(link._1) + (
          if(d == 1) ""
          else pts.get(link) match { // Get primitive value(s)
              case None => "()"
              case Some(m) => m.index(uri.asInstanceOf[URI]).mkString("(", ", ", ")")
            })
        ).mkString("|")

      } else { // Node-type. Recurse over the linked nodes to get their tags
        val childNodes = db.linkNodeInstancesByValue1(uri).values
          .map(idx => getTag(idx.index.get(uri), d - 1))
          .filter(_.trim.nonEmpty)

        ts.mkString("|") + (if(childNodes.nonEmpty) childNodes.mkString("(", ", ", ")") else "")
      }

    } else ""
  }

  private def stripTag(tag: String): String = tag.substring(tag.findLast(_.equals('.')) match {
    case Some(_) => tag.lastIndexOf('.') + 1
    case None => 0
  })


  def printMatches(printTypes: Boolean = true): String = {
    val builder = new StringBuilder

    builder.append("Matches: " + matches.size)
    for (mat <- matches) {
      builder.append("\n" + mat.prettyPrint(printTypes))
    }

    builder.toString
  }
}
