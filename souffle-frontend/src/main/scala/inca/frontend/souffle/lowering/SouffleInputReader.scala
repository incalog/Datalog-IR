package inca.frontend.souffle.lowering

import inca.frontend.souffle.Syntax
import inca.frontend.souffle.Util
import inca.runtime.db.DatabaseInput
import scala.io.Source

trait SouffleInputReader {
  def dir: String
  def compileRow(relation: String, row: Seq[(String, Any)]): DatabaseInput

  def compile(inputs: Map[Syntax.Input, Syntax.RuleSignature]): DatabaseInput = {
    inputs.foldLeft(DatabaseInput.empty) { case (dbInput, (input, sig)) =>
      val newDBInput = compile(input, sig)
      dbInput.combine(newDBInput)
    }
  }

  def compile(input: Syntax.Input, sig: Syntax.RuleSignature): DatabaseInput = {
    val absolutePath = s"$dir/${input.filename}"
    val src = Source.fromFile(absolutePath)
    val edits = compile(src.getLines(), sig, input.delimiter)
    src.close()
    edits
  }

  def compile(
      rows: Iterator[String],
      sig: Syntax.RuleSignature,
      delimiter: String
    ): DatabaseInput = {
    val rowLits = rows.map { tuple =>
      val columns = tuple.split(delimiter)
      if (columns.size != sig.parameters.size)
        throw new IllegalArgumentException(
          s"Number of entries ${columns.size} does not match number of parameters ${sig.parameters.size} of signature ${sig.name}"
        )
      val sigTypes = sig.parameters.map(_.typ)
      val compiledColumns = columns.zip(sigTypes).map { case (c, t) => compileColumn(c, t) }
      val cleanedNames = sig.parameters.map(p => Util.cleanSouffleName(p.name))
      cleanedNames.zip(compiledColumns)
    }.toSeq
    // remove duplicate tuples
    val inputs = rowLits.distinct.map(compileRow(sig.name.name, _))
    inputs.foldLeft(DatabaseInput.empty) { case (res, in) =>
      res.combine(in)
    }
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
