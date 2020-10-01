package inca.souffle

import truechange.{EditScript, JVMURI, Load, NamedTag}

import scala.io.Source

// Important: Legacy souffle code .type Type will translate to .type Type <: symbol
class SouffleInputToEditscript(dir: String) {

  def compile(input: Syntax.Input, sig: Syntax.RuleSignature): EditScript = {
    val absolutePath = s"$dir/${input.filename}"
    val src = Source.fromFile(absolutePath)
    val edits = compile(src.getLines(), sig, input.delimiter)
    src.close()
    edits
  }

  def compile(rows: Iterator[String], sig: Syntax.RuleSignature, delimiter: String): EditScript = {
    val tag = NamedTag(sig.name.intern)

    val edits = rows.map { tuple =>
      val columns = tuple.split(delimiter)
      if (columns.size != sig.parameters.size) throw new IllegalArgumentException(s"Number of entries ${columns.size} does not match number of parameters ${sig.parameters.size} of signature ${sig.name}")
      val sigTypes = sig.parameters.map(_.typ)
      val compiledColumns = columns.zip(sigTypes).map { case (c, t) => compileColumn(c, t) }
      val cleanedNames = sig.parameters.map(p => Util.cleanSouffleName(p.name))
      val lits = cleanedNames.zip(compiledColumns)
      Load(new JVMURI, tag, Seq(), lits)
    }.toSeq
    EditScript(edits)
  }

  // TODO: For this specific file DeclaredType are always an alias of symbol hence we translate DeclaredType always to String
  def compileColumn(elem: String, typ: Syntax.Type): Any = typ match {
    case Syntax.DeclaredType(_) => elem.intern
    case Syntax.SymbolType => elem.intern
    case Syntax.NumberType => elem.toInt
    case Syntax.UnsignedType => elem.toLong
    case Syntax.FloatType => elem.toDouble
  }
}
