//package inca.frontend.functional.clonesTest
//
//import inca.frontend.functional.compile.{CompiledFunctionalModule, FunctionalCompilerOptions}
//import inca.frontend.functional.executor.FunctionalExecutor
//import inca.ir
//import inca.ir.{ExtensionalCall, ExtensionalRelation}
//import inca.util.FileUtil
//import org.scalatest.funsuite.AnyFunSuite
//import inca.ir.{BaseIR, Language, Name, Module as IRModule}
//import inca.ir.extension.*
//import inca.ir.extension.arithmetic.{Add, GT, IntNum, LE, Mul, Sub, TInt}
//import ir.{Body, Call, Eq, Param, Relation, Var}
//import inca.ir.term2Arg
//import inca.ir.valueNumbering.{ConfigVNOld, ValueNumbering}
//import inca.viatra.backend.Executor
//
//// TODO fill in missing expected results below
//
//class ClonesFunctionalTest extends AnyFunSuite{
//  val options = FunctionalCompilerOptions.fromResource("functional/Options.ini")
//  val exec: FunctionalExecutor = new FunctionalExecutor(new Executor)
//
//  def performTest(path: String, expectedVNResult: IRModule, expectedExecResult: Any, argsExec: Seq[Any] = Seq(), functionName: String = "main", config: ConfigVNOld = ConfigVNOld()): Unit = {
//    val code = FileUtil.readFileFromResource(path)
//    val compiled = exec.compileFunction(code, options)
//    compiled.setPipeline(CompiledFunctionalModule.pipeline)
//
//    // execution still has expected result?
//    val loaded = exec.loadFunction(compiled)
//    val res = loaded.execute(functionName, argsExec)
//    assertResult(expectedExecResult)(res.entries.head)
//    println(s"query result: ${res.entries} with size ${res.size}")
//    // VN optimized to expected module?
//    assertResult(expectedVNResult)(compiled.valueNumberingResult.head)
//  }
//
//
//  test("simple redundant let") {
//    val expected = IRModule(Name("Let"),
//      Language(Set(new BaseIR{}, new data.IR{}, new string.IR{}, new aggregate.IR{}, new arithmetic.IR{})),
//      Seq(
//        Relation(Name("main"), Seq(Param(Name("main_result$0"), TInt)),
//          Seq(
//            Body(Seq(
//              Eq(Var(Name("main_result$0")), IntNum(7))))
//          ))
//      )
//    )
//    performTest("functional/clones/letRedundant.finca", expected , 7)
//  }
//
//  test("simple redundant let 2 (with input)") {
//    val expected = IRModule(Name("Let2"),
//      Language(Set(new BaseIR {}, new data.IR {}, new string.IR {}, new aggregate.IR {}, new arithmetic.IR {})),
//      Seq(
//        Relation(Name("main"),
//          Seq(Param(Name("z"), TInt),Param(Name("main_result$0"), TInt)),
//          Seq(Body(Seq(
//            ExtensionalCall(Name("ext_main$input"),Seq(Var(Name("z")))),
////            Eq(Var(Name("x")),Var(Name("z"))),
////            Eq(Var(Name("y")),Var(Name("x"))),
//            Eq(Var(Name("main_result$0")), Mul(Var(Name("z")),Var(Name("z")))
//          ))))),
//        ExtensionalRelation(Name("ext_main$input"),Seq(Param(Name("z"), TInt)))
//      )
//    )
//    performTest("functional/clones/letRedundant2.finca", expected, 1, Seq(1))
//  }
//
//  test("let with addition") {
//    val expected = IRModule(Name("LetAddition"),
//      Language(Set(new BaseIR {}, new data.IR {}, new string.IR {}, new aggregate.IR {}, new arithmetic.IR {})),
//      Seq(
//        Relation(Name("main"),
//          Seq(Param(Name("z"), TInt), Param(Name("main_result$0"), TInt)),
//          Seq(Body(Seq(
//            ExtensionalCall(Name("ext_main$input"), Seq(Var(Name("z")))),
//            Eq(Var(Name("a")), Add(IntNum(10),Var(Name("z")))),
////            Eq(Var(Name("b")), Var(Name("a"))),
////            Eq(Var(Name("c")), Var(Name("a"))),
////            Eq(Var(Name("main_result$0")), Add(Var(Name("a")), Add(Var(Name("a")),Var(Name("a")))))
//            Eq(Var(Name("main_result$0")), Mul(IntNum(3),Var(Name("a"))))
//            )))),
//        ExtensionalRelation(Name("ext_main$input"), Seq(Param(Name("z"), TInt)))
//      )
//    )
//    performTest("functional/clones/letAddition.finca", expected, 33, Seq(1), config = ConfigVNOld(true))
//  }
//
//  test("let with addition (commutativity)") {
//    val expected = IRModule(Name("LetAddition"),
//      Language(Set(new BaseIR {}, new data.IR {}, new string.IR {}, new aggregate.IR {}, new arithmetic.IR {})),
//      Seq(
//        Relation(Name("main"),
//          Seq(Param(Name("z"), TInt), Param(Name("main_result$0"), TInt)),
//          Seq(Body(Seq(
//            ExtensionalCall(Name("ext_main$input"), Seq(Var(Name("z")))),
//            Eq(Var(Name("a")), Add(IntNum(10), Var(Name("z")))),
////            Eq(Var(Name("b")), Var(Name("a"))),
////            Eq(Var(Name("c")), Var(Name("a"))),
////            Eq(Var(Name("main_result$0")), Add(Var(Name("a")), Add(Var(Name("b")), Var(Name("c")))))
//            Eq(Var(Name("main_result$0")), Mul(IntNum(3),Var(Name("a"))))
//          )))),
//        ExtensionalRelation(Name("ext_main$input"), Seq(Param(Name("z"), TInt)))
//      )
//    )
//    performTest("functional/clones/letAddCommutativ.finca", expected, 33, Seq(1), config = ConfigVNOld(true))
//  }
//
//  test("let with addition (associativity)") {
//    val expected = IRModule(Name("LetAddition"),
//      Language(Set(new BaseIR {}, new data.IR {}, new string.IR {}, new aggregate.IR {}, new arithmetic.IR {})),
//      Seq(
//        Relation(Name("main"),
//          Seq(Param(Name("z"), TInt), Param(Name("main_result$0"), TInt)),
//          Seq(Body(Seq(
//            ExtensionalCall(Name("ext_main$input"), Seq(Var(Name("z")))),
//            Eq(Var(Name("a")), Add(IntNum(11), Var(Name("z")))),
////            Eq(Var(Name("b")), Var(Name("a"))),
////            Eq(Var(Name("c")), Var(Name("a"))), // constant folding: 1 + (10 + z) -/-> 11 + z ?
////            Eq(Var(Name("main_result$0")), Add(Var(Name("a")), Add(Var(Name("b")), Var(Name("c")))))
//            Eq(Var(Name("main_result$0")), Mul(IntNum(3),Var(Name("a"))))
//          )))),
//        ExtensionalRelation(Name("ext_main$input"), Seq(Param(Name("z"), TInt)))
//      )
//    )
//    performTest("functional/clones/letAddAssociative.finca", expected, 36, Seq(1), config = ConfigVNOld(true))
//  }
//
//
//
//  test("if Condition simple with Add") {
//    val expected = IRModule(Name("IfSimple"),
//      Language(Set(new BaseIR {}, new data.IR {}, new string.IR {}, new aggregate.IR {}, new arithmetic.IR {})),
//      Seq(
//        Relation(Name("main"),
//          Seq(Param(Name("main_result$0"), TInt)),
//          Seq(Body(Seq(Eq(Var(Name("main_result$0")), IntNum(7))))))
//      )
//    )
//    performTest("functional/clones/ifSimple.finca", expected, 7, Seq())
//  }
//
//  test("if Condition (with input)") {
//    val expected = IRModule(Name("IfCondition"),
//      Language(Set(new BaseIR {}, new data.IR {}, new string.IR {}, new aggregate.IR {}, new arithmetic.IR {})),
//      Seq(
//        Relation(Name("main"),
//          Seq(Param(Name("z"), TInt), Param(Name("main_result$0"), TInt)),
//          Seq(
//            Body(Seq(
//            ExtensionalCall(Name("ext_main$input"), Seq(Var(Name("z")))),
//            Eq(Var(Name("x")), Var(Name("z"))),
//            GT(Var(Name("x")), IntNum(0)),
//            Eq(Var(Name("if_result$0")), Var(Name("x"))),
//            Eq(Var(Name("main_result$0")), Var(Name("if_result$0")))  // TODO treat such redundant assignments?
//            )),
//            Body(Seq(
//              ExtensionalCall(Name("ext_main$input"), Seq(Var(Name("z")))),
//              Eq(Var(Name("x")), Var(Name("z"))),        // TODO how to treat repeated computation like this?
//              LE(Var(Name("x")), IntNum(0)),             // TODO how to treat repeated computation like this?
//              Eq(Var(Name("if_result$0")), Mul(Var(Name("x")),IntNum(-1))),
//              Eq(Var(Name("main_result$0")), Var(Name("if_result$0")))
//            ))
//          )),
//        ExtensionalRelation(Name("ext_main$input"), Seq(Param(Name("z"), TInt)))
//      )
//    )
//    performTest("functional/clones/ifCondition.finca", expected, 7, Seq(7))
//  }
//
//  test("if Condition 2") {
//    val expected = IRModule(Name("IfCondition2"),
//      Language(Set(new BaseIR {}, new data.IR {}, new string.IR {}, new aggregate.IR {}, new arithmetic.IR {})),
//      Seq(
//        Relation(Name("main"),
//          Seq(Param(Name("z"), TInt), Param(Name("main_result$0"), TInt)),
//          Seq( // TODO how to treat these repetitions? <- Conditions and return value of else repeated
//            Body(Seq(
//              ExtensionalCall(Name("ext_main$input"), Seq(Var(Name("z")))),
//              Eq(Var(Name("x")), Var(Name("z"))),
//              Eq(Var(Name("y")), Var(Name("z"))),
//              GT(Var(Name("y")), IntNum(0)),
//              GT(Var(Name("x")), IntNum(0)), // TODO ?
//              Eq(Var(Name("if_result$0")), Var(Name("x"))),
//              Eq(Var(Name("main_result$0")), Var(Name("if_result$0")))
//            )),
//            Body(Seq(
//              ExtensionalCall(Name("ext_main$input"), Seq(Var(Name("z")))),
//              Eq(Var(Name("x")), Var(Name("z"))),
//              Eq(Var(Name("y")), Var(Name("z"))),
//              GT(Var(Name("y")), IntNum(0)),
//              LE(Var(Name("x")), IntNum(0)),
//              Eq(Var(Name("if_result$0")), Mul(Var(Name("x")), IntNum(-1))),
//              Eq(Var(Name("main_result$0")), Var(Name("if_result$0")))
//            )),
//            Body(Seq(
//              ExtensionalCall(Name("ext_main$input"), Seq(Var(Name("z")))),
//              Eq(Var(Name("x")), Var(Name("z"))),
//              Eq(Var(Name("y")), Var(Name("z"))),
//              LE(Var(Name("y")), IntNum(0)),
//              GT(Var(Name("x")), IntNum(0)),
//              Eq(Var(Name("if_result$0")), Mul(Var(Name("x")), IntNum(-1))),
//              Eq(Var(Name("main_result$0")), Var(Name("if_result$0")))
//            )),
//            Body(Seq(
//              ExtensionalCall(Name("ext_main$input"), Seq(Var(Name("z")))),
//              Eq(Var(Name("x")), Var(Name("z"))),
//              Eq(Var(Name("y")), Var(Name("z"))),
//              LE(Var(Name("y")), IntNum(0)),
//              LE(Var(Name("x")), IntNum(0)),
//              Eq(Var(Name("if_result$0")), Mul(Var(Name("x")), IntNum(-1))),
//              Eq(Var(Name("main_result$0")), Var(Name("if_result$0")))
//            ))
//          )),
//        ExtensionalRelation(Name("ext_main$input"), Seq(Param(Name("z"), TInt)))
//      )
//    )
//    performTest("functional/clones/ifCondition2.finca", expected, 7, Seq(7))
//  }
//
//  test("if Redundant") {
//    val expected = IRModule(Name("IfRedundant"),
//      Language(Set(new BaseIR {}, new data.IR {}, new string.IR {}, new aggregate.IR {}, new arithmetic.IR {})),
//      Seq(
//        Relation(Name("main"),
//          Seq(Param(Name("z"), TInt), Param(Name("main_result$0"), TInt)),
//          Seq(
//            //TODO how to treat redundant if-statements (i.e. then and else branch have same result)?
//            //  how to identify that bodies are redundant?
//            Body(Seq(
//              ExtensionalCall(Name("ext_main$input"), Seq(Var(Name("z")))),
//              Eq(Var(Name("x")), Var(Name("z"))),
//              GT(Var(Name("x")), IntNum(0)),
//              Eq(Var(Name("if_result$0")), Var(Name("x"))),
//              Eq(Var(Name("main_result$0")), Var(Name("if_result$0")))
//            )),
//            Body(Seq(
//              ExtensionalCall(Name("ext_main$input"), Seq(Var(Name("z")))),
//              Eq(Var(Name("x")), Var(Name("z"))),
//              LE(Var(Name("x")), IntNum(0)),
//              Eq(Var(Name("if_result$0")), Var(Name("x"))),
//              Eq(Var(Name("main_result$0")), Var(Name("if_result$0")))
//            ))
//          )),
//        ExtensionalRelation(Name("ext_main$input"), Seq(Param(Name("z"), TInt)))
//      )
//    )
//    performTest("functional/clones/IfRedundant.finca", expected, 7, Seq(7))
//  }
//
//  test("if Condition3 (nested with input)") {
//    performTest("functional/clones/ifCondition3.finca", ???, 2, Seq(1))
//  }
//
//  test("fib") { // like existing fib program
//    // TODO could a repeated recursive call be saved somehow?
//    performTest("functional/clones/Fib.finca", ???, 2, Seq(1))
//  }
//
//  test("fact") { // like existing fact program
//    val expected = IRModule(Name("Fact"),
//      Language(Set(new BaseIR {}, new data.IR {}, new string.IR {}, new aggregate.IR {}, new arithmetic.IR {})),
//      Seq(
//        Relation(Name("fact"),Seq(Param(Name("n"), TInt),Param(Name("fact_result$0"), TInt)),
//          Seq(
//            Body(Seq(
//              Call(Name("fact$input"),Seq(Var(Name("n")))),
//              Eq(Var(Name("n")),IntNum(1)),
//              Eq(Var(Name("fact_result$0")),IntNum(1))
//            )),
//            Body(Seq(
//              Call(Name("fact$input"),Seq(Var(Name("n")))),
//              Eq(Var(Name("n")),IntNum(1),true),  // TODO code clones from if condition like in tests above
//              Call(Name("fact"), Seq(Sub(Var(Name("n")),IntNum(1)),Var(Name("fact_call$0")))),
//              Eq(Var(Name("if_result$0")), Mul(Var(Name("n")),Var(Name("fact_call$0")))),
//              Eq(Var(Name("fact_result$0")), Var(Name("if_result$0")))
//            ))
//          )),
//        Relation(Name("main"), Seq(Param(Name("n"), TInt),Param(Name("main_result$0"), TInt)),
//          Seq(
//            Body(Seq(
//              ExtensionalCall(Name("ext_main$input"), Seq(Var(Name("n")))),
//              Call(Name("fact"),Seq(Var(Name("n")),Var(Name("fact_call$1")))),
//              Eq(Var(Name("main_result$0")),Var(Name("fact_call$1")))
//            ))
//          )),
//        ExtensionalRelation(Name("ext_main$input"),Seq(Param(Name("n"), TInt))),
//        Relation(Name("fact$input"),Seq(Param(Name("n$0"), TInt)),  // TODO how to treat copied code from input _$relations ?
//          Seq(
//            Body(Seq(
//            Call(Name("fact$input"),Seq(Var(Name("n")))),
//            Eq(Var(Name("n")),IntNum(1),true),
//            Eq(Var(Name("n$0")),Sub(Var(Name("n")),IntNum(1)))
//            )),
//            Body(Seq(
//              ExtensionalCall(Name("ext_main$input"), Seq(Var(Name("n")))),
//              Eq(Var(Name("n$0")), Var(Name("n")))
//            ))
//          ))
//      ))
//    performTest("functional/clones/Fact.finca", expected, 24, Seq(4))
//  }
//
//
//
//
//
//
//}
