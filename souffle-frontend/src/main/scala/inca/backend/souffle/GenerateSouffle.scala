package inca.backend.souffle

import inca.backend.hints.DataHints.DataType
import inca.backend.ir.Datalog
import inca.frontend.functional
import inca.frontend.functional.core.{DataConstructor, DataDef, TData}
import inca.frontend.souffle.Syntax._

class GenerateSouffle {

  private var extensionalRelations: Set[String] = Set()

  def compileModule(module: Datalog.Module, datas: Seq[DataDef]): String = {
    val types = datas.map(compileDataDef)
    val rels = module.pats.flatMap(compilePattern)
    val inputRels = generateInputRelations(extensionalRelations)
    s"""
       |${types.mkString("\n")}
       |${inputRels.mkString("\n")}
       |${rels.mkString("\n")}
       |""".stripMargin

  }

  def compileDataDef(data: DataDef): String = {
    val constructors = data.constrs.map { case DataConstructor(name, paramTypes) =>
      val params = paramTypes.zipWithIndex.map(p => s"x${p._2}:${compileFunType(p._1)}")
      s"$name {${params.mkString(",")}}"
    }

    s""".type ${data.name} =
       |  ${constructors.mkString(" | ")}
       |""".stripMargin
  }
  def compileFunType(ty: functional.core.Type): String = ty match {
    case TData(name) => name.name
    case functional.core.TScalaBoolean => "unsigned"
    case functional.core.TScalaInt => "number"
    case functional.core.TScalaDouble => "float"
    case functional.core.TScalaString => "symbol"
    case _ => throw new IllegalArgumentException(ty.toString)
  }

  def compilePattern(pat: Datalog.Pattern): Seq[SouffleContent] = {
    if (pat.hasHint(DataType.key))
      return Seq()

    val decl = RuleSignature(pat.name, pat.params.map(p => RuleParameter(p.name, compileType(p.typ))), output = false)
    Seq(decl)
  }

  def compileType(ty: Datalog.Type): Type = ty match {
    case Datalog.TData(name) => DeclaredType(name)
    case Datalog.TLiteral.Bool => UnsignedType
    case Datalog.TLiteral.Int => NumberType
    case Datalog.TLiteral.Double => FloatType
    case Datalog.TLiteral.String => SymbolType
    case _ => throw new IllegalArgumentException(ty.toString)
  }

  def generateInputRelations(extensionals: Set[String]): Seq[SouffleContent] = {
    Seq()
  }
}
