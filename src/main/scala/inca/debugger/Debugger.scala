package inca.debugger

import inca.compiler.CompiledFunModule
import inca.frontend.core.tree.{Param, PatternFunction}
import inca.runtime.Query.Matcher
import inca.runtime.{Database, DatabaseAccessor}
import truechange.{Type, URI}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters.ListHasAsScala

// FIXME choose more meaningful names, reconsider access modifiers/qualifiers
class Debugger(feed: Database, matcher: Matcher, module: CompiledFunModule) {
  private val db = new DatabaseAccessor(feed)

  private[debugger] val funParams: Map[String, Seq[Param]] = module.fun.content.map({
    case pf: PatternFunction => (pf.name.name, pf.params)
  }).toMap

  private[debugger] val env: mutable.Map[String, Set[ColumnValue]] = mutable.Map()

  val matches: Seq[Match] = {
    val funName = matcher.getPatternName.replace(module.fun.name + "_", "")

    val bufMatches = new ListBuffer[Match]
    matcher.forEachMatch(mat => {
      val cols = mat.parameterNames().asScala.map(colName => {
        val colValue = mat.get(colName) match {
          case uri: URI => URIValue(uri, getNodeTypes(uri))
          case scalaType => ScalaValue(scalaType)
        }
        val colType = getColType(funName, colName)
        Column(colName, colValue, colType)

      }).toSeq
      bufMatches += Match(mat, cols)
    })

    bufMatches.toList
  }

  def load(mat: Match): Unit = { // Not finalized yet! Name should also change
    mat.inputs.foreach(col => {
      env.updateWith(col.name) {
        case Some(v) => Some(v + col.value)
        case None => Some(Set(col.value))
      }
    })

    println(env)
  }

  private def getColType(funName: String, colName: String): ColumnType = {
    val params = funParams.getOrElse(funName,
      throw new IllegalArgumentException(s"Function $funName not defined in module"))

    if(params.map(p => p.name.name).contains(colName)) // Function funName defines paramName as parameter
      Input
    else
      Output
  }

  private def getNodeTypes(uri: URI): Seq[Type] = db.nodeInstancesByValue(uri).keys.toSeq


  def printMatches(tagDepth: Int = 0): String = {
    val builder = new StringBuilder
    builder.append("Matches: " + matches.size)

    matches.foreach(mat => builder.append(printMatch(mat, tagDepth)))
    builder.toString
  }

  def printMatch(mat: Match, tagDepth: Int = 0): String = {
    mat.cols.map {
      col: Column => "\"" + col.name + "[" + col.ty + "]" +  "\"=" + (col.value match {
        case uriVal: URIValue => uriVal.uri + {
          if(tagDepth <= 0) ""
          else ":" + getTag(uriVal.uri, tagDepth)
        }
        case scalaVal: ScalaValue => scalaVal.prettyPrint()
      })
    }.mkString("\nMatch { ", ", ", " }")
  }

  private def getTag(uri: URI, d: Int): String = {
    if(d > 0) {
      val ts = db.linkNodeInstancesByValue1(uri).keys.map(link => stripTag(link._1)) // Link (Tag:String, name:String)

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
}
