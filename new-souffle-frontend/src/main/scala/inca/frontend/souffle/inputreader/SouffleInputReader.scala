package inca.frontend.souffle.inputreader

import inca.frontend.souffle.Syntax
import inca.frontend.souffle.Util
import inca.runtime.db.DatabaseInput
import scala.io.Source

trait SouffleInputReader {
  def dir: String
  def compileRow(relation: String, row: Seq[(String, Any)]): DatabaseInput

  def compile(inputs: Map[Syntax.RelationDecl, Syntax.Directive]): DatabaseInput = {
    inputs.foldLeft(DatabaseInput.empty) { case (dbInput, (input, sig)) =>
      val newDBInput = compile(input, sig)
      dbInput.combine(newDBInput)
    }
  }

  def compile(decl: Syntax.RelationDecl, directive: Syntax.Directive): DatabaseInput = {
    val fileNameDirective = directive.params.getOrElse("filename", Syntax.DirectiveValueString(decl.name))
    val delimiterDirective = directive.params.getOrElse("delimiter", Syntax.DirectiveValueString("\t"))
    val fileNameString = fileNameDirective.asInstanceOf[Syntax.DirectiveValueString].value
    val delimiterString = delimiterDirective.asInstanceOf[Syntax.DirectiveValueString].value
    val absolutePath = s"$dir/$fileNameString"
    val src = Source.fromFile(absolutePath)
    val input = compile(src.getLines(), decl, delimiterString)
    src.close()
    input
  }

  def compile(rows: Iterator[String],
              decl: Syntax.RelationDecl,
              delimiter: String
             ): DatabaseInput = {
    val rowLits = rows.map { tuple =>
      val columns = tuple.split(delimiter)
      if (columns.size != decl.attributes.size)
        throw new IllegalArgumentException(
          s"Number of entries ${columns.size} does not match number of parameters ${decl.attributes.size} of signature ${decl.name}"
        )
      val sigTypes = decl.attributes.map(_.ty)
      val compiledColumns = columns.zip(sigTypes).map { case (c, t) => compileColumn(c, t) }
      val cleanedNames = decl.attributes.map(p => Util.cleanSouffleName(p.name))
      cleanedNames.zip(compiledColumns)
    }.toSeq
    // remove duplicate tuples
    val inputs = rowLits.distinct.map(compileRow(decl.name, _))
    inputs.foldLeft(DatabaseInput.empty) { case (res, in) =>
      res.combine(in)
    }
  }

  // TODO: For this specific file DeclaredType are always an alias of symbol hence we translate DeclaredType always to String
  def compileColumn(elem: String, typ: Syntax.TypeName): Any = typ match {
    case Syntax.DeclaredType(_) => elem.intern
    case Syntax.SymbolType => elem.intern
    case Syntax.NumberType => elem.toInt
    case Syntax.UnsignedType => elem.toLong
    case Syntax.FloatType => elem.toDouble
  }
}
