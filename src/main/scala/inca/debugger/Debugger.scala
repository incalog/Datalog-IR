package inca.debugger

import inca.runtime.{Database, DatabaseAccessor, Query}
import truechange.URI

import scala.jdk.CollectionConverters.ListHasAsScala

class Debugger(feed: Database) {
  private val db = new DatabaseAccessor(feed)

  def printMatches(matcher: Query.Matcher): String = {
    val builder = new StringBuilder
    builder.append("Matches: " + matcher.countMatches())

    matcher.forEachMatch(mat => {
      val fm = mat.parameterNames().asScala.map(p => "\"" + p + "\"=" + mat.get(p) + ":" + getType(mat.get(p)))
      builder.append(fm.mkString("\n  Match { ", ", ", " }"))
    })

    builder.toString
  }

  def getType(uri: Any): String = {
    val ts = db.linkNodeInstancesByValue1(uri).keys.map(link => stripTag(link._1)) // Link is (Tag:String, name:String)

    if(ts.isEmpty) { // Primitive type
        val pts = db.linkPrimitiveInstancesByValue1(uri)
        pts.keys.map(link => stripTag(link._1) + (pts.get(link) match { // Pretty-print with primitive value(s)
          case None => "()"
          case Some(m) => m.index(uri.asInstanceOf[URI]).mkString("(", ", ", ")")
        })).mkString("[", ", ", "]")

    } else
      ts.mkString("[", ",", "]")
  }

//  def getType(uri: Any): String = { // TODO ASK Alt. p2/dup/eff.
//    val ts = db.nodeInstancesByValue(uri).keys.map(k => stripTag(k.toString))
//    ts.mkString("[", ",", "]")
//  }

  private def stripTag(tag: String): String = tag.substring(tag.findLast(_.equals('.')) match {
    case Some(_) => tag.lastIndexOf('.') + 1
    case None => 0
  })

}
