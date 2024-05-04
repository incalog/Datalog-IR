package inca.frontend.datalog.clonesTest

import inca.frontend.datalog.compile.DatalogCompilerOptions
import inca.frontend.datalog.executor.DatalogExecutor
import inca.frontend.datalog.executor.DatalogExecutor.?
import inca.util.FileUtil
import org.eclipse.viatra.query.runtime.rete.matcher.DRedReteBackendFactory
import org.scalatest.funsuite.AnyFunSuite
import inca.ir.{BaseIR, Language, Name, Module as IRModule}
import inca.ir.extension.*
import inca.ir.extension.arithmetic.*
import inca.ir.*
import inca.ir.extension.arithmetic.TInt
import inca.ir.valueNumbering.ConfigVNOld
import inca.viatra.backend.Executor

// TODO fill in missing expected results below

class ClonesDatalogTest extends AnyFunSuite {
  val pipeline = List()
  val options = DatalogCompilerOptions.fromResource("datalog/Options.ini")
  val exec: DatalogExecutor = new DatalogExecutor(new Executor(DRedReteBackendFactory.INSTANCE))

  def performTest(path: String, expectedVNResult: IRModule, expectedQueryResult: Any, relationName: String, argsQuery: Seq[Any], config: ConfigVNOld = ConfigVNOld()): Unit = {
    val code = FileUtil.readFileFromResource(path)
    val compiled = exec.compileDatalog(code, options)
    compiled.setPipeline(pipeline)

    // querying still has expected result?
    val loaded = exec.loadDatalog(compiled)
    val res = loaded.query(relationName, argsQuery)
    println(s"query result: ${res.toSet} with size ${res.size}")
    assertResult(expectedQueryResult)(res.size)
    // VN optimized to expected module?
    assertResult(expectedVNResult)(compiled.valueNumberingResult.head)
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

    performTest("datalog/clones/PathOriginal.dl", expected, 11, "Path", Seq(?, ?))
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

    performTest("datalog/clones/BodyRepeatedSimple.dl", expected, 2, "A", Seq(?, ?))
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
//            Body(Seq(
//              Eq(Var(Name("param$0")), IntNum(2)), 
//              Eq(Var(Name("param$1")), IntNum(1))
//            )),  // TODO is redundancy like this recognizable? -> probably not
            Body(Seq(
              Call(Name("b"), Seq(Var(Name("Y")), Var(Name("X")))),
              Eq(Var(Name("param$0")), Var(Name("X"))),
              Eq(Var(Name("param$1")), Var(Name("Y")))
            )),
          ))
      ))

    performTest("datalog/clones/BodyRedundant.dl", expected, 1, "a", Seq(3, ?))
  }

  test("Relation Redundant"){
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param(Name("param$0"), TInt), Param(Name("param$1"), TInt)),
          Seq(
            Body(Seq(Eq(Var(Name("param$0")), IntNum(1)), Eq(Var(Name("param$1")), IntNum(2))))
          )) //,
//        Relation(Name("b"), Seq(Param(Name("param$0"), TInt), Param(Name("param$1"), TInt)),  // TODO remove redundant relation
//          Seq(
//            Body(Seq(Eq(Var(Name("param$0")), IntNum(1)), Eq(Var(Name("param$1")), IntNum(2))))
//          ))
      ))

    performTest("datalog/clones/RelationRedundant.dl", expected, 1, "a", Seq(1, ?))
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

    performTest("datalog/clones/Intersection.dl", expected, 1, "intersect", Seq(2))
  }


  test("Path") {
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
            //            Body(Seq(Eq(Var(Name("param$0")),IntNum(6)), Eq(Var(Name("param$1")),IntNum(4))))   // remove duplicated Body
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
              //              Call(Name("Edge"), Seq(Var(Name("X")), Var(Name("Z")))),  // remove redundant call
              Call(Name("Path"), Seq(Var(Name("Z")), Var(Name("Y")))),
              Eq(Var(Name("param$0")), Var(Name("X"))),
              Eq(Var(Name("param$1")), Var(Name("Y")))
            ))
            //            Body(Seq( // remove redundant body
            //              Call(Name("Edge"), Seq(Var(Name("X")), Var(Name("Z")))),
            //              Call(Name("Path"), Seq(Var(Name("Z")), Var(Name("Y")))),
            //              Eq(Var(Name("param$0")), Var(Name("X"))),
            //              Eq(Var(Name("param$1")), Var(Name("Y")))
            //            ))
          ))
      ))
    performTest("datalog/clones/Path.dl", expected, 11, "Path", Seq(?, ?))

    //    resOriginal = loadedOriginal.query("Path", (?, 5), (3, ?)) // TODO change def of helper method above so that multiple args are possible
    //    assertResult(4)(resOriginal.size)
    //
    //    resOriginal = loadedOriginal.query("Path", (1, ?))
    //    assertResult(4)(resOriginal.size)
    //
    //    resOriginal = loadedOriginal.query("Path", (2, 5))
    //    assertResult(1)(resOriginal.size)
  }
  

  test("test") {
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt), Param("param$2", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("X")),IntNum(1)),
            Eq(Var(Name("Y")),IntNum(2)),
            Eq(Var(Name("H1")), Add(Var("X"),IntNum(2))),
//            Eq(Var(Name("H2")), Var(Name("H1"))),
            Eq(Var(Name("Z")), Add(Var("H1"),Var("H1"))),
            Eq(Var(Name("param$0")),Var(Name("X"))),
            Eq(Var(Name("param$1")),Var(Name("Y"))),
            Eq(Var(Name("param$2")),Var(Name("Z")))
          ))
        ))
      ))

    performTest("datalog/clones/ExprRedundantInEq.dl", expected, 1, "a", Seq(1,2,?))
  }

  test("PathTwoTwo") {
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("Edge"), Seq(Param(Name("param$0"), TInt), Param(Name("param$1"), TInt)),
          Seq(
            Body(Seq(Eq(Var(Name("param$0")), IntNum(1)), Eq(Var(Name("param$1")), IntNum(2)))),
            Body(Seq(Eq(Var(Name("param$0")), IntNum(2)), Eq(Var(Name("param$1")), IntNum(2)))),
            Body(Seq(Eq(Var(Name("param$0")), IntNum(2)), Eq(Var(Name("param$1")), IntNum(3)))),
            Body(Seq(Eq(Var(Name("param$0")), IntNum(3)), Eq(Var(Name("param$1")), IntNum(5)))),
            Body(Seq(Eq(Var(Name("param$0")), IntNum(3)), Eq(Var(Name("param$1")), IntNum(4)))),
            Body(Seq(Eq(Var(Name("param$0")), IntNum(5)), Eq(Var(Name("param$1")), IntNum(4)))),
            Body(Seq(Eq(Var(Name("param$0")), IntNum(6)), Eq(Var(Name("param$1")), IntNum(4)))),
            //            Body(Seq(Eq(Var(Name("param$0")),IntNum(6)), Eq(Var(Name("param$1")),IntNum(4))))   // remove duplicated Body
          )),
        Relation(Name("PathTwoTwo"), Seq(Param(Name("param$0"), TInt), Param(Name("param$1"), TInt)),
          Seq(
            Body(Seq(
              Call(Name("Edge"), Seq(Var(Name("X")), Var(Name("Y")))),
              Eq(Var(Name("X")), IntNum(2)),
              Eq(Var(Name("Y")), IntNum(2)),
              Eq(Var(Name("param$0")), Var(Name("X"))),
              Eq(Var(Name("param$1")), Var(Name("X")))
            )),
            Body(Seq(
              Call(Name("Edge"), Seq(Var(Name("X")), Var(Name("Z1")))),
              Call(Name("Edge"), Seq(Var(Name("Z1")), Var(Name("Z2")))),
              Call(Name("Edge"), Seq(Var(Name("Z2")), Var(Name("Y")))),
              Eq(Var(Name("Z1")), IntNum(2)),
              Eq(Var(Name("Z2")), IntNum(2)),
              Eq(Var(Name("param$0")), Var(Name("X"))),
              Eq(Var(Name("param$1")), Var(Name("Y")))
            ))
          ))
      ))
    performTest("datalog/clones/PathTwoTwo.dl", expected, 4, "PathTwoTwo", Seq(?, ?)) // (1,2) (1,3) (2,2) (2,3)
  }


}
