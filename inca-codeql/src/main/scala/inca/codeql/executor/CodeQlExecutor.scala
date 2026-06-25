package inca.codeql.executor

import inca.codeql.compile.{CodeQlCompilerOptions, CompiledCodeQlUnit}
import inca.codeql.edb.{CodeQlCli, EdbExportQuery}
import _root_.inca.ir.execution.{IRExecutor, Relation}

import java.nio.file.Path
import scala.annotation.targetName

object CodeQlExecutor:
  @targetName("wildcard")
  val ? : Null = null

class CodeQlExecutor(val executor: IRExecutor):
  case class Loaded(engine: executor.Engine, compiled: CompiledCodeQlUnit):
    def query(relation: String, tuples: Seq[Any]*): Relation =
      val declaration = compiled.irModules.flatMap(_.relations).toMap.getOrElse(
        relation,
        throw IllegalArgumentException(s"No CodeQL relation named $relation")
      )
      if tuples.exists(_.size != declaration.params.size) then
        throw IllegalArgumentException(s"Each query tuple for $relation must have arity ${declaration.params.size}")
      val input = Relation.from(relation, declaration.params.map(_.name.name), tuples)
      engine.read(input)

    def select(tuples: Seq[Any]*): Relation = query(CompiledCodeQlUnit.SelectRelationName.name, tuples*)

  def compileCodeQl(source: String, options: CodeQlCompilerOptions = CodeQlCompilerOptions.default): CompiledCodeQlUnit =
    CompiledCodeQlUnit.fromSource(source, options)

  def loadCodeQl(compiled: CompiledCodeQlUnit, inputs: Seq[Relation] = Seq.empty): Loaded =
    val engine = executor.instantiate(compiled)
    inputs.foreach(engine.insert)
    Loaded(engine, compiled)

  def loadCodeQlDatabase(
    compiled: CompiledCodeQlUnit,
    database: Path,
    workDirectory: Path,
    exports: Seq[EdbExportQuery],
    cli: CodeQlCli = CodeQlCli()
  ): Loaded =
    val inputs = exports.map(query => cli.exportRelation(database, workDirectory, compiled, query))
    loadCodeQl(compiled, inputs)
