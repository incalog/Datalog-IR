package inca.frontend.objectoriented.integration.core

import inca.frontend.objectoriented.compiler.ObjectOptions
import inca.frontend.objectoriented.executor.Executor
import inca.frontend.objectoriented.integration.TestDefinition
import inca.frontend.objectoriented.integration.TestDefinition.{SetResult, TupleResult}
import inca.util.FileUtil.readFile
import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite

case class ResultError(msg: String)

trait GenericTest extends AnyFunSuite {
  def executor: Executor
  def options: ObjectOptions = ObjectOptions()

  private def flatten(tup: TupleResult[Any]): TupleResult[Any] = tup.flatMap {
    case s: TupleResult[_] => flatten(s)
    case e => TupleResult(e)
  }

  def performTests[O](tests: Seq[TestDefinition[O]]): Seq[Assertion] =
    tests.flatMap(performTest)

  def performTest[O](test: TestDefinition[O]): Seq[Assertion] = {
    val code = readFile(test.filePath)
    val fun = executor.loadFunction(code, options)

    val result = fun.execute(test.main, test.input)
    fun.printAllMatches()
    if (result.res.isEmpty)
      Seq(assert(false, s"Expected a result, but got none."))
    else {
      checkResult(test.expectedResult, result.res).map(err => assert(false, err.msg))
    }
  }

  private def checkResult[O](expectedResult: O, result: Seq[Seq[Any]]): Seq[ResultError] = {
    expectedResult match {
      // match datalog sets
      case expectedSet: SetResult[_] =>
        if (result.size > expectedSet.size)
          Seq(ResultError(s"Expected set with size ${expectedSet.size}, but got ${result.size}."))
        else {
          result.flatMap { actual =>
            val anyErrorFree = expectedSet.exists(checkResult(_, Seq(actual)).isEmpty)
            if (!anyErrorFree)
              Seq(ResultError(s"Can not find $actual in result $expectedSet"))
            else
              Seq()
          }
        }

      // match datalog tuples
      case expectedTuple: TupleResult[_] =>
        // datalog flattens the outpuclass t tuple => flatten the expected result as well
        val flattenTuple = flatten(expectedTuple)
        if (result.size > 1)
          Seq(ResultError(s"Expected single result, but got result set with size ${result.size}."))
        else if (result.head.size != flattenTuple.size)
          Seq(ResultError(s"Expected ${flattenTuple.size} result(s), but got ${result.head.size}."))
        else if (!result.head.zip(flattenTuple).forall { case (actual, expectedValue) => expectedValue.equals(actual) })
          Seq(ResultError(s"""Expectd ${flattenTuple.mkString("(", ", ", ")")}, but got ${result.head.mkString("(", ", ", ")")}"""))
        else
          Seq()

      case value =>
        checkResult(TupleResult(value), result)
    }
  }
}