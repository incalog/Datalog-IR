package inca.frontend.objectoriented.executor

import inca.frontend.objectoriented.compiler.{CompiledObjectModule, ObjectOptions}

class Results[T](val res: Seq[Seq[T]]) {
  def isEmpty: Boolean = res.isEmpty

  override def equals(obj: Any): Boolean = obj match {
    case expected: Results[T] =>
      res.size == expected.res.size &&
        res.forall(ac => expected.res.exists(ex => sameVals(ac, ex))) &&
        expected.res.forall(ex => res.exists(ac => sameVals(ac, ex)))
    case _ => false
  }

  private def sameVals(actual: Seq[T], expected: Seq[T]): Boolean = actual.size == expected.size &&
    actual.zip(expected).forall { case (x, y) => x == y }

  override def toString: String = s"Results(${res.mkString(", ")})"
}

trait Executor {
  def results[T](res: Seq[Seq[T]]): Results[T] = new Results(res)
  def resultVal[T](res: T): Results[T] = results(Seq(Seq(res)))
  def resultVals[T](res: T*): Results[T] = results(Seq(res))

  def loadFunction(compiled: CompiledObjectModule): Loaded
  def loadFunction(code: String, options: ObjectOptions = ObjectOptions()): Loaded
}

trait Loaded {
  def compiled: CompiledObjectModule
  def execute(main: String, args: Seq[meta.Term], deleteInput: Boolean = false): Results[Any]
  def printAllMatches(): Unit = {}
}