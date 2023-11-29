package inca.frontend.datalog.clonesTest

import inca.frontend.datalog.executor.DatalogExecutor
import inca.frontend.datalog.executor.DatalogExecutor.?
import inca.util.FileUtil
import org.scalatest.funsuite.AnyFunSuite
import inca.ir.{BaseIR, Language, Name, Module as IRModule}
import inca.ir.extension.*
import inca.ir.extension.arithmetic.*
import inca.ir.*
import inca.ir.extension.arithmetic.TInt


class ClonesDatalogTest extends AnyFunSuite {
  val pipeline = List()
  val exec: DatalogExecutor = new DatalogExecutor(inca.viatra.Executor)

  def performTest(path: String, expectedVNResult: IRModule, expectedQueryResult: Any, relationName: String, argsQuery: Product): Unit = {
    val code = FileUtil.readFileFromResource(path)
    val compiled = exec.compileDatalog(code)
    compiled.setPipeline(pipeline)

    // TODO value numbering -> fill in missing expected results below
    val vnResult: IRModule = compiled.valueNumbering(compiled.lowered) // TODO currently lowered twice: here and in Executor
    println("ValueNumbering Result: ")
    println(vnResult)
    assertResult(expectedVNResult)(vnResult)

    // still computes same result?
    val loadedOriginal = exec.loadDatalog(compiled)
    val resOriginal = loadedOriginal.query(relationName, argsQuery)
    assertResult(expectedQueryResult)(resOriginal.size)
    //assertResult(expectedQueryResult)(exec.loadDatalog(vnResult).execute(relationName, argsQuery).size)
  }


  test("Path original unchanged") {

    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("Edge"), Seq(Param(Name("param$0"), TInt), Param(Name("param$1"), TInt)),
          Seq(
            Body(Seq(Eq(Var(Name("param$0")), IntNum(1)), Eq(Var(Name("param$1")), IntNum(2)))),
            Body(Seq(Eq(Var(Name("param$0")), IntNum(2)), Eq(Var(Name("param$1")), IntNum(3)))),
            Body(Seq(Eq(Var(Name("param$0")), IntNum(3)), Eq(Var(Name("param$1")), IntNum(5)))),
            Body(Seq(Eq(Var(Name("param$0")), IntNum(3)), Eq(Var(Name("param$1")), IntNum(4)))),
            Body(Seq(Eq(Var(Name("param$0")), IntNum(5)), Eq(Var(Name("param$1")), IntNum(4)))),
            Body(Seq(Eq(Var(Name("param$0")), IntNum(6)), Eq(Var(Name("param$1")), IntNum(4)))),
          )),
        Relation(Name("Path"), Seq(Param(Name("param$0"), TInt), Param(Name("param$1"), TInt)),
          Seq(
            Body(Seq(
              Call(Name("Edge"), Seq(Var(Name("X")), Var(Name("Y")))),
              Eq(Var(Name("param$0")), Var(Name("X"))),
              Eq(Var(Name("param$1")), Var(Name("Y")))
            )),
            Body(Seq(
              Call(Name("Edge"), Seq(Var(Name("X")), Var(Name("Z")))),
              Call(Name("Path"), Seq(Var(Name("Z")), Var(Name("Y")))),
              Eq(Var(Name("param$0")), Var(Name("X"))),
              Eq(Var(Name("param$1")), Var(Name("Y")))
            ))
          ))
      ))

    performTest("datalog/clones/PathOriginal.dl", expected, 11, "Path", (?, ?))
  }


  test("Path") {
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR{}, new arithmetic.IR{}, new string.IR{})),
      Seq(
        Relation(Name("Edge"), Seq(Param(Name("param$0"),TInt), Param(Name("param$1"),TInt)),
          Seq(
            Body(Seq(Eq(Var(Name("param$0")),IntNum(1)), Eq(Var(Name("param$1")),IntNum(2)))),
            Body(Seq(Eq(Var(Name("param$0")),IntNum(2)), Eq(Var(Name("param$1")),IntNum(3)))),
            Body(Seq(Eq(Var(Name("param$0")),IntNum(3)), Eq(Var(Name("param$1")),IntNum(5)))),
            Body(Seq(Eq(Var(Name("param$0")),IntNum(3)), Eq(Var(Name("param$1")),IntNum(4)))),
            Body(Seq(Eq(Var(Name("param$0")),IntNum(5)), Eq(Var(Name("param$1")),IntNum(4)))),
            Body(Seq(Eq(Var(Name("param$0")),IntNum(6)), Eq(Var(Name("param$1")),IntNum(4)))),
            Body(Seq(Eq(Var(Name("param$0")),IntNum(6)), Eq(Var(Name("param$1")),IntNum(4))))   // TODO remove duplicated Body
          )),
        Relation(Name("Path"), Seq(Param(Name("param$0"),TInt), Param(Name("param$1"),TInt)),
          Seq(
            Body(Seq(
              Call(Name("Edge"),Seq(Var(Name("X")),Var(Name("Y")))),
              Eq(Var(Name("param$0")),Var(Name("X"))),
              Eq(Var(Name("param$1")),Var(Name("Y")))
            )),
            Body(Seq(
              Call(Name("Edge"), Seq(Var(Name("X")), Var(Name("Z")))),
              Call(Name("Edge"), Seq(Var(Name("X")), Var(Name("Z")))),  // TODO remove redundant call
              Call(Name("Path"), Seq(Var(Name("Z")), Var(Name("Y")))),
              Eq(Var(Name("param$0")), Var(Name("X"))),
              Eq(Var(Name("param$1")), Var(Name("Y")))
            )),
            Body(Seq( // TODO remove redundant body
              Call(Name("Edge"), Seq(Var(Name("X")), Var(Name("Z")))),
              Call(Name("Path"), Seq(Var(Name("Z")), Var(Name("Y")))),
              Eq(Var(Name("param$0")), Var(Name("X"))),
              Eq(Var(Name("param$1")), Var(Name("Y")))
            ))
          ))
      ))
    performTest("datalog/clones/Path.dl", expected, 11, "Path", (?, ?))

//    resOriginal = loadedOriginal.query("Path", (?, 5), (3, ?)) // TODO change def of helper method above so that multiple args are possible
//    assertResult(4)(resOriginal.size)
//
//    resOriginal = loadedOriginal.query("Path", (1, ?))
//    assertResult(4)(resOriginal.size)
//
//    resOriginal = loadedOriginal.query("Path", (2, 5))
//    assertResult(1)(resOriginal.size)
  }

  test("simple repeated body"){
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("A"), Seq(Param(Name("param$0"), TInt), Param(Name("param$1"), TInt)),
          Seq(
            Body(Seq(Eq(Var(Name("param$0")), IntNum(1)), Eq(Var(Name("param$1")), IntNum(2)))),
            Body(Seq(Eq(Var(Name("param$0")), IntNum(2)), Eq(Var(Name("param$1")), IntNum(3)))),
          ))
      ))

    performTest("datalog/clones/BodyRepeatedSimple.dl", expected, 2, "A", (?, ?))
  }

  test("redundant body") {
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("b"), Seq(Param(Name("param$0"), TInt), Param(Name("param$1"), TInt)),
          Seq(
            Body(Seq(Eq(Var(Name("param$0")), IntNum(1)), Eq(Var(Name("param$1")), IntNum(2)))),
            Body(Seq(Eq(Var(Name("param$0")), IntNum(2)), Eq(Var(Name("param$1")), IntNum(3)))),
          )),
        Relation(Name("a"), Seq(Param(Name("param$0"), TInt), Param(Name("param$1"), TInt)),
          Seq(
            Body(Seq(Eq(Var(Name("param$0")), IntNum(2)), Eq(Var(Name("param$1")), IntNum(1)))),  // TODO is redundancy like this recognizable?
            Body(Seq(
              Call(Name("b"), Seq(Var(Name("Y")), Var(Name("X")))),
              Eq(Var(Name("param$0")), Var(Name("X"))),
              Eq(Var(Name("param$1")), Var(Name("Y")))
            )),
          ))
      ))

    performTest("datalog/clones/BodyRedundant.dl", expected, 1, "a", (3, ?))
  }

  test("Relation Redundant"){
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param(Name("param$0"), TInt), Param(Name("param$1"), TInt)),
          Seq(
            Body(Seq(Eq(Var(Name("param$0")), IntNum(1)), Eq(Var(Name("param$1")), IntNum(2))))
          )),
        Relation(Name("b"), Seq(Param(Name("param$0"), TInt), Param(Name("param$1"), TInt)),  // TODO remove redundant relation
          Seq(
            Body(Seq(Eq(Var(Name("param$0")), IntNum(1)), Eq(Var(Name("param$1")), IntNum(2))))
          ))
      ))

    performTest("datalog/clones/RelationRedundant.dl", expected, 1, "a", (1, ?))
  }


  test("Intersection with redundant call") {
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param(Name("param$0"), TInt)),
          (1 to 4).map(i => Body(Seq(Eq(Var(Name("param$0")),IntNum(i)))))
        ),
        Relation(Name("b"), Seq(Param(Name("param$0"), TInt)),
          (2 to 5).map(i => Body(Seq(Eq(Var(Name("param$0")), IntNum(i)))))
        ),
        Relation(Name("intersect"), Seq(Param(Name("param$0"), TInt)),
          Seq(Body(Seq(
            Call(Name("a"),Seq(Var(Name("X")))),
            Call(Name("b"),Seq(Var(Name("X")))),
            Eq(Var(Name("param$0")),Var(Name("X")))
          )))
        )
      ))

    performTest("datalog/clones/Intersection.dl", expected, 1, "intersect", ???)
  }

  test("test") {
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        // TODO
      ))

    performTest("datalog/clones/test.dl", expected, 1, "b", (1,2))
  }

}
