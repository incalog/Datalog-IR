package inca.souffle.frontend.executor

import inca.ir.Name
import inca.ir.execution.{IRExecutor, Relation}
import inca.souffle.frontend.compile.CompiledSouffleProgram
import inca.util.compileroptions.CompilerOptions

import scala.annotation.targetName

object SouffleExecutor:
  @targetName("wildcard")
  val ? : Null = null

class SouffleExecutor(val exec: IRExecutor):
  case class Loaded(engine: exec.Engine, compiled: CompiledSouffleProgram):
    def insert(edb: Relation): Unit = engine.insert(edb)

    def output(rel: Relation): Relation = engine.read(rel)

    def outputs(): Seq[Relation] = compiled.outputRelations.map(output)

    def query(rel: String, tups: Seq[Any]*): Relation =
      compiled.irModules.flatMap(_.relations).toMap.get(rel) match
        case Some(r) =>
          if tups.exists(_.size != r.params.size) then
            throw IllegalArgumentException(s"Each tuple should have size ${r.params.size} in $tups")
          output(Relation.from(rel, r.params.map(_.name.name), tups))
        case None =>
          throw IllegalStateException(s"No relation found for name $rel")

  def loadSouffle(compiled: CompiledSouffleProgram): Loaded =
    Loaded(exec.instantiate(compiled.mainUnit), compiled)

  def loadSouffle(compiled: CompiledSouffleProgram, inputs: Seq[Relation]): Loaded =
    val loaded = loadSouffle(compiled)
    inputs.foreach(loaded.insert)
    loaded

  def loadSouffle(compiled: CompiledSouffleProgram, inputBaseDir: String): Loaded =
    loadSouffle(compiled, compiled.loadEdbInputs(inputBaseDir))

  def compileSouffle(
    code: String,
    compilerOptions: CompilerOptions = CompilerOptions.default
  ): CompiledSouffleProgram =
    CompiledSouffleProgram.fromSourceCode(Name("SouffleProg"), code, compilerOptions)
