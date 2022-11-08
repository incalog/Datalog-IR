package inca.frontend.runner

import inca.compiler.CompiledModule
import inca.runtime.EnginePool
import inca.runtime.context.QueryScope
import inca.runtime.db.Database
import org.eclipse.viatra.query.runtime.api.AdvancedViatraQueryEngine
import org.eclipse.viatra.query.runtime.rete.matcher.TimelyReteBackendFactory

trait RunnerFactory[I <: Input, R <: Runner[I]] {
  protected def compiled: CompiledModule

  private val scope = new QueryScope(compiled.dataModel)
  protected val (engine, database) = EnginePool.loadEngineAndDatabase(scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)

  def runner(relName: RelationName): R
}

class IRRunnerFactory(override val compiled: CompiledModule) extends RunnerFactory[IRInput, IRRunner] {
  override def runner(relName: RelationName): IRRunner = new IRRunner(relName, compiled, engine, database)
}

/*object Test {
  // Functional
  val inst = FunctionalInstance(mod)
  inst.execute("main", Seq(q"1", q"2", q"""Add(Var("x"), 12)"""))
  // Object Oriented
  val inst = OOInstance(mod)
  inst.execute("main", Seq(q"1", q"2", q"""new X(12, 3)"""))
  // Constaint
  val inst = ConstraintInstance(mod)
  //
  // @diffable trait Exp extends Diffable
  // case class Add(lhs: Exp, rhs: Exp)
  val tree = Add(Var("x"), 12)
  val tree = Add(Var("x"), 12)
  inst.execute("main", tree, edbInserts/Deletions)
  // IR
  val inst = IRInstance(mod)
  inst.execute("main", edbInserts/Deletions)
  // Souffle
  val inst = SouffleInstance(mod)
  inst.execute("main", edbInserts/Deletions OR Editscript)

  val inst = FunctionalInstance(mod)
  inst.
}

trait DatalogInstance[I <: Input, R <: Runner] {
  def engine: AdvancedViatraQueryEngine
  def database: Database

  protected def updateInput(input: I): Unit = {
    val edbChange = input.translate
    engine.delayUpdatePropagation { () =>
      database.processEditScript(edbChange.es)
      edbChange.insertions.foreach { case (name, rel) =>
        rel.entries.foreach {
          database.insert(name, rel.flattenEntry(_))
        }
      }
      edbChange.deletions.foreach { case (name, rel) =>
        rel.entries.foreach {
          database.delete(name, rel.flattenEntry(_))
        }
      }
    }
  }
  protected def query(name: String, rel: Relation): Runner = {
    // get spec
    // get matcher
    // get result
  }
}
trait Runner[I <: Input] {
  def updateInput(input: I): Unit
}
class FunctionalRunner(instance: FunctionalInstance[FunctionalInput, FunctionalRunner]) extends Runner {
  def run(name: String, args: Seq[meta.Scala.Term]): Relation
}
class ObjectOrientedRunner(instance: ObjectOrientedInstance[ObjectOrientedInput, ObjectOrientedRunner]) extends Runner {
  def run(name: String, args: Seq[meta.Scala.Term]): Relation = {
    if (typeCastError) throw TypeCastEx
    else rel
  }
}
class IRRunner(instance: DatalogInstance[IRInput, IRRunner]) extends Runner {
  def run(name: String, rel: Relation): Relation
}
//trait EDBChange {
//  def es: EditScript
//  def insertions: Seq[Insertion]
//  def deletions: Seq[Insertion]
//}
trait Input {
  def translate: EDBChange
}
case class FunctionalInput() extends Input
case class ObjectOrientedInput() extends Input
class FuntionalInstance(...) extends DatalogInstance[FunctionalInput] {

}

// x, y, z
// 1, 2, 3

// 1, 2, 3
// 2, 3, 5
// 7, 3, 9
inst.changeEDB()
inst.changeEDB()
inst.query()

// we need compiled module for these steps
//   engine
//   database
//   queryscope

// oo
// engine
// db
// queryscope
// fill db with inheritance hierarchy
// create runner r
// r.run(main, ....)*/
