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

  private val funParams: Map[String, Seq[Param]] = module.fun.content.map({
    case pf: PatternFunction => (pf.name.name, pf.params)
  }).toMap

  val matches: Seq[Match] = {
    val funName = matcher.getPatternName.replace(module.fun.name + "_", "")

    val bufMatches = new ListBuffer[Match]
    matcher.forEachMatch(mat => {
      val params = mat.parameterNames().asScala.map(paramName => {
        val paramValue = mat.get(paramName) match {
          case uri: URI => PURI(uri, getNodeTypes(uri), getTag(uri))
          case scalaType => PScalaType(scalaType)
        }
        val paramIO = getIOType(funName, paramName)
        Parameter(paramName, paramValue, paramIO)

      }).toSeq
      bufMatches += Match(mat, params)
    })

    bufMatches.toList
  }

  private def getIOType(funName: String, paramName: String): ParameterIO = {
    val params = funParams.getOrElse(funName,
      throw new IllegalArgumentException(s"Function $funName not defined in module"))

    if(params.map(p => p.name.name).contains(paramName)) // Function funName defines paramName as parameter
      InputParam
    else
      OutputParam
  }

  private def getNodeTypes(uri: URI): Seq[Type] = {
    db.nodeInstancesByValue(uri).keys.toSeq
  }

  private def getTag(uri: URI): String = {
    val ts = db.linkNodeInstancesByValue1(uri).keys.map(link => stripTag(link._1)) // Link is (Tag:String, name:String)

    if(ts.isEmpty) { // Primitive type
        val pts = db.linkPrimitiveInstancesByValue1(uri)
        pts.keys.map(link => stripTag(link._1) + (pts.get(link) match { // Get primitive value(s)
          case None => "()"
          case Some(m) => m.index(uri.asInstanceOf[URI]).mkString("(", ", ", ")")
        })).mkString("[", ", ", "]")

    } else
      ts.mkString("[", ",", "]")
  }

  private def stripTag(tag: String): String = tag.substring(tag.findLast(_.equals('.')) match {
    case Some(_) => tag.lastIndexOf('.') + 1
    case None => 0
  })

  def printMatches(printTypes: Boolean = true): String = {
    val builder = new StringBuilder

    builder.append("Matches: " + matches.size)
    for (mat <- matches) {
      builder.append("\n" + mat.toString(printTypes))
    }

    builder.toString
  }
}
