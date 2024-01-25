package inca.viatra.edbdata

import inca.viatra.runtime.context.DataModel
import truechange.*

import scala.collection.immutable.MultiDict

enum Nat:
  case Zero()
  case Succ(pred: Nat)

  val uri: URI = new JVMURI
  def tag: Tag = this match
    case Zero() => NamedTag("Zero")
    case Succ(_) => NamedTag("Succ")
  def load(): EditScript =
    val buf = new EditScriptBuffer
    load(buf)
    buf += Attach(uri, tag, RootLink, null, RootTag)
    buf.toEditScript
  def load(buf: EditScriptBuffer): Unit = this match
    case Zero() =>
      buf += Load(uri, tag, Seq(), Seq())
    case Succ(pred) =>
      pred.load(buf)
      buf += Load(uri, tag, Seq("pred" -> pred.uri), Seq())

object Nat:
  val dataModel: DataModel = new DataModel(
    Set(SortType("Nat"), SortType("Succ"), SortType("Zero")),
    MultiDict.from(Iterable(SortType("Succ") -> SortType("Nat"), SortType("Zero") -> SortType("Nat"))),
    Map(("Succ","pred") -> SortType("Nat")),
    Map()
  )