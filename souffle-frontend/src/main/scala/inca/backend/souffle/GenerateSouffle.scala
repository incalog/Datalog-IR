package inca.backend.souffle

import inca.backend.hints.DataHints.DataType
import inca.backend.ir.IR
import inca.frontend.functional
import inca.frontend.functional.core.{DataConstructor, DataDef, TData}
import inca.frontend.souffle.Syntax._

class GenerateSouffle {

  private var extensionalRelations: Set[String] = Set()

  def compileModule(module: IR.Module, datas: Seq[DataDef]): String = {
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

  def compilePattern(pat: IR.Pattern): Seq[SouffleContent] = {
    if (pat.hasHint(DataType.key))
      return Seq()

    val decl = RuleSignature(pat.name, pat.params.map(p => RuleParameter(p.name, compileType(p.typ))), output = false)
    Seq(decl)
  }

  def compileType(ty: IR.Type): Type = ty match {
    case IR.TData(name) => DeclaredType(name)
    case IR.TLiteral.Bool => UnsignedType
    case IR.TLiteral.Int => NumberType
    case IR.TLiteral.Double => FloatType
    case IR.TLiteral.String => SymbolType
    case _ => throw new IllegalArgumentException(ty.toString)
  }

  def generateInputRelations(extensionals: Set[String]): Seq[SouffleContent] = {
    Seq()
  }
}
