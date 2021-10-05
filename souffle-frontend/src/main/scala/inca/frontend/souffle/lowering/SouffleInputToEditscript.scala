package inca.frontend.souffle.lowering

import inca.frontend.souffle.{Souffle, Util}
import truechange.{EditScript, JVMURI, Load, NamedTag}

import scala.io.Source

// Important: Legacy souffle code .type Type will translate to .type Type <: symbol
class SouffleInputToEditscript() {

  def compile(dir: String, input: Souffle.Input, sig: Souffle.RuleSignature): EditScript = {
    val absolutePath = s"$dir/${input.filename}"
    val src = Source.fromFile(absolutePath)
    val edits = compile(src.getLines(), sig, input.delimiter)
    src.close()
    edits
  }

  def compile(rows: Iterator[String], sig: Souffle.RuleSignature, delimiter: String): EditScript = {
    val preparedRows = rows.map(_.split(delimiter).toSeq).toSeq
    compile(preparedRows, sig)
  }

  def compile(rows: Seq[Seq[String]], sig: Souffle.RuleSignature): EditScript = {
    val tag = NamedTag(sig.name.intern)

    val rowLits = rows.map { row =>
      if (row.size != sig.parameters.size) throw new IllegalArgumentException(s"Number of entries ${row.size} does not match number of parameters ${sig.parameters.size} of signature ${sig.name}")
      val sigTypes = sig.parameters.map(_.typ)
      val compiledColumns = row.zip(sigTypes).map { case (c, t) => compileColumn(c, t) }
      val cleanedNames = sig.parameters.map(p => Util.cleanSouffleName(p.name))
      cleanedNames.zip(compiledColumns)
    }.toSeq

    // remove duplicate tuples
    val edits = rowLits.distinct.map(Load(new JVMURI, tag, Seq(), _))
    EditScript(edits)
  }

  // TODO: For this specific file DeclaredType are always an alias of symbol hence we translate DeclaredType always to String
  def compileColumn(elem: String, typ: Souffle.Type): Any = typ match {
    case Souffle.DeclaredType(_) => elem.intern
    case Souffle.SymbolType => elem.intern
    case Souffle.NumberType => elem.toInt
    case Souffle.UnsignedType => elem.toLong
    case Souffle.FloatType => elem.toDouble
  }
}
