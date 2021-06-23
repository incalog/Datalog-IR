package inca.frontend.datalog.lowering

import inca.frontend.datalog.syntax
import inca.frontend.datalog.syntax._
import inca.runtime.data.DataURI
import inca.util.Scala.symbolOf

import scala.meta.{Type => MetaType, _}

class GenerateScala {
  private var visited: Map[Any, Seq[meta.Stat]] = Map()
  private def createIfNeeded(a: Any)(f: => Seq[meta.Stat]): Unit = visited.get(a) match {
    case None =>
      this.visited += a -> Seq()
      val stats = f
      this.visited += a -> stats
    case Some(_) => // nothing
  }

  def generated: List[meta.Stat] = visited.values.flatten.toList

  private val oDataURI = symbolOf(DataURI)

  def genDataDef(data: DataDef): Unit = createIfNeeded(data) {
    val dataTyp = MetaType.Name(data.name.name)
    val typ = q"sealed trait $dataTyp extends truediff.Diffable"
    val constrs = data.constrs.map {
      case DataConstructor(syntax.Name(name), params) =>
        val scalaParamTypes = params.map(p => p.name.name + "$" -> transType(p.typ))
        val scalaParams = scalaParamTypes.map { case (name, pt) =>
          param"val ${Term.Name(name)}: $pt"
        }.toList
        val children = scalaParamTypes.map { case (name, _) =>
          q"${Lit.String(name.dropRight(1))} -> ${Term.Name(name)}"
        }.toList
        val terms = scalaParamTypes.map { case (name, _) =>
          Term.Name(name)
        }.toList
        val makeChildren = scalaParamTypes.zipWithIndex.map { case ((_,pt), ix) =>
          q"children($ix).asInstanceOf[$pt]"
        }.toList
        q"""case class ${MetaType.Name(name)}(..$scalaParams) extends {} with $dataTyp() with truediff.GenericDiffable() { this =>
              this.withURI($oDataURI($name, ..$terms))

              override def name: String = $name
              override def children: Seq[(String, Any)] = Seq(..$children)
              override def make(children: Seq[Any]): ${MetaType.Name(name)} = ${Term.Name(name)}(..$makeChildren)
            }
           """
    }
    typ +: constrs
  }

  def transType(t: Type): MetaType = t match {
    case TAny =>  t.asScala
    case TNothing =>  t.asScala
    case t: TLiteral => t.asScala
    case d: TData => d.target match {
      case Some(data: DataDef) =>
        genDataDef(data)
        MetaType.Name(data.name.name)
      case Some(t) => throw new IllegalArgumentException(s"Unknown data target $t")
      case _ => throw new IllegalArgumentException(s"Cannot compile unresolved type $d")
    }
    case TScala(t) => t.tree
  }

}
