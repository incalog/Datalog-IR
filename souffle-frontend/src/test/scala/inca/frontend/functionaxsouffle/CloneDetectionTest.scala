package inca.frontend.functionaxsouffle

import inca.frontend.functional.compiler.FunctionalOptions
import inca.frontend.functionalxsouffle.executor.FunctionalXSouffleExecutor
import org.scalatest.funsuite.AnyFunSuite

import scala.io.{BufferedSource, Source}
import scala.meta.{Lit, XtensionQuasiquoteTerm}

class CloneDetectionTest extends AnyFunSuite {

  val expAdt =
    s"""data ArgList = NilArg() | ConsArg(Exp, ArgList)
       |data Exp = NumLit(String)
       |         | Var(String)
       |         | BinOp(String, Exp, Exp)
       |         | UnOp(String, Exp)
       |         | Cast(Exp, String)
       |         | InstanceOf(Exp, String)
       |         | StringLit(String)
       |         | Alloc(String, String)
       |         | SpecialInvoke(String, String, ArgList)
       |         | ArrayRead(Exp, Exp)
       |         | InstanceFieldRead(Exp, String)
       |         | StaticFieldRead(String)
       |         | Invoke(Exp, String, ArgList)
       |         | SuperInvoke(Exp, String, ArgList)
       |         | StaticInvoke(String, ArgList)
       |         | DynamicInvoke(String, String, ArgList)
       |         | This()
       |         | Null()
       |         | Phi(ArgList)
       |"""

  val stmAdt: String =
    s"""data CaseList = ConsCase(Int, Int, CaseList) | DefaultCase(Int)
       |data Stm = Assign(String, String)
       |         | InvokeStm(Exp, String, ArgList)
       |         | StaticInvokeStm(String, ArgList)
       |         | ArrayWrite(Exp, Exp, Exp)
       |         | InstanceFieldWrite(Exp, String, Exp)
       |         | StaticFieldWrite(String, Exp)
       |         | ReturnVoid()
       |         | Return(Exp)
       |         | Goto(Int)
       |         | If(String, Exp, Exp, Int) // TODO What is DummyIfVar for?
       |         | TableSwitch(Exp, CaseList)
       |         | LookupSwitch(Exp, CaseList)
       |
       |data StmList = ConsStm(Stm, Int, StmList) | NilStm()
       |"""

  // TODO ClassConstant
  // TODO MethodHandleConstant
  // PolymorphicInvoke
  // How can i generate them? What features do i need to use to get these in jimple?
  val assignExpFun: String =
    s"""
       |def assignExp(v: String): Set[Exp] = {
       |    NumLit(num) | (inst, idx, num, v, meth) in _AssignNumConstant
       |  } ++ {
       |    BinOp(op, left, right) |
       |      (inst, idx, v, meth) in _AssignBinop,
       |      (inst, op) in _OperatorAt,
       |      left in getOperand(inst, 1),
       |      right in getOperand(inst, 2)
       |  } ++ {
       |    UnOp(op, exp) |
       |      (inst, idx, v, meth) in _AssignUnop,
       |      (inst, op) in _OperatorAt,
       |      exp in getOperand(inst, 1)
       |  } ++ {
       |    exp |
       |      count(_AssignLocal(_, _, _, v, _)) == 1,
       |      (inst, idx, from, v, meth) in _AssignLocal,
       |      exp in assignExp(from)
       |  } ++ {
       |    Phi(alts) |
       |      (inst, _, _, v, method) in _AssignLocal,
       |      count(_AssignLocal(_, _, _, v, _)) > 1,
       |      alts in getPhiAlternatives(v, method)
       |  } ++ {
       |    // TODO maybe remove the method prefix from the variable name
       |    Var(v) | (idx, meth, v) in _FormalParam
       |  } ++ {
       |    Cast(exp, ty) |
       |      (inst, idx, from, v, ty, meth) in _AssignCast,
       |      exp in assignExp(from)
       |  } ++ {
       |    InstanceOf(exp, ty) |
       |      (inst, idx, from, v, ty, meth) in _AssignInstanceOf,
       |      exp in assignExp(from)
       |  } ++ {
       |    Null() | (inst, idx, v, meth) in _AssignNull
       |  } ++ {
       |    This() | (meth, v) in _ThisVar
       |  } ++ {
       |    Alloc(heap, ty) |
       |      (inst1, idx1, heap, v, meth, line) in _AssignHeapAllocation,
       |      (v, ty) in _Var_Type,
       |      (inst2, idx2, specialmeth, v, meth) not in _SpecialMethodInvocation
       |  } ++ {
       |    SpecialInvoke(heap, specialmeth, args) |
       |      (inst1, idx1, heap, v, meth, line) in _AssignHeapAllocation,
       |      (inst2, idx2, specialmeth, v, callingMeth) in _SpecialMethodInvocation,
       |      args in getArgs(inst2, 0)
       |  } ++ {
       |    ArrayRead(exp, NumLit(`String.valueOf`(index))) |
       |       (inst, idx, v, from, meth) in _LoadArrayIndex,
       |       exp in assignExp(from),
       |       (inst, index) in _ArrayNumIndex
       |  } ++ {
       |    ArrayRead(exp, indexExp) |
       |       (inst, idx, v, from, meth) in _LoadArrayIndex,
       |       (inst, indexVar) in _ArrayInsnIndex,
       |       exp in assignExp(from),
       |       indexExp in assignExp(indexVar)
       |  } ++ {
       |    Invoke(recvExp, meth, args) |
       |      (inst, v) in _AssignReturnValue,
       |      (inst, idx, meth, recv, callingMeth) in _VirtualMethodInvocation,
       |      recvExp in assignExp(recv),
       |      args in getArgs(inst, 0)
       |  } ++ {
       |    SuperInvoke(recvExp, meth, args) |
       |      (inst, v) in _AssignReturnValue,
       |      (inst, idx, meth, recv, callingMeth) in _SuperMethodInvocation,
       |      recvExp in assignExp(recv),
       |      args in getArgs(inst, 0)
       |  } ++ {
       |    StaticInvoke(meth, args) |
       |      (inst, v) in _AssignReturnValue,
       |      (inst, idx, meth, callingMeth) in _StaticMethodInvocation,
       |      args in getArgs(inst, 0)
       |  } ++ {
       |    DynamicInvoke(bootmeth, dynname, args) |
       |      (inst, v) in _AssignReturnValue,
       |      (inst, idx, bootmeth, dynname, dynretty, dynarity, dynparamtys, tag, callingMeth) in _DynamicMethodInvocation,
       |      args in getArgs(inst, 0)
       |  } ++ {
       |    InstanceFieldRead(recvExp, field) |
       |      (inst, idx, v, recv, field, meth) in _LoadInstanceField,
       |      recvExp in assignExp(recv)
       |  } ++ {
       |    StaticFieldRead(fieldsig) | (inst, idx, v, fieldsig, meth) in _LoadStaticField
       |  }
       |
       |def getOperand(inst: String, pos: Int): Set[Exp] =
       |  { NumLit(num) | (inst, pos, num) in _AssignOperFromConstant } ++
       |  { exp | (inst, pos, var) in _AssignOperFrom, exp in assignExp(var) }
       |
       |def getArgs(inst: String, currentIdx: Int): Set[ArgList] =
       |  {
       |    ConsArg(exp, rest) |
       |      (currentIdx, inst, v) in _ActualParam,
       |      exp in assignExp(v),
       |      rest in getArgs(inst, currentIdx + 1)
       |  } ++ {
       |    NilArg() | (currentIdx, inst, v) not in _ActualParam
       |  }
       |
       |def maxInt(x: Int, y: Int): Int =
       |  if (x > y)
       |    x
       |  else if (x < y)
       |    y
       |  else
       |    x
       |
       |def minInt(x: Int, y: Int): Int =
       |  if (x > y)
       |    y
       |  else if (x < y)
       |    x
       |  else
       |    x
       |
       |def lastIndexOfPhiPrefix(v: String): Int = (v.`lastIndexOf`("phi-assign/")) + 11
       |
       |def indexOfPhiInstruction(inst: String): Int =
       |  let idx = lastIndexOfPhiPrefix(inst) in
       |    let phiIdx = inst.`substring`(idx) in
       |      phiIdx.`toInt`
       |
       |def indicesOfPhiAlternatives(v: String): Set[Int] =
       |  { indexOfPhiInstruction(inst) | (inst, _, _, v, _) in _AssignLocal }
       |
       |def prefixOfPhiInstruction(inst: String): String =
       |  inst.`substring`(0, lastIndexOfPhiPrefix(inst))
       |
       |def instructionPrefixOfPhiAlternatives(v: String): Set[String] =
       |  { prefixOfPhiInstruction(inst) | (inst, _, _, v, _) in _AssignLocal }
       |
       |def minOfPhiIndices(v: String): Int =
       |  fold(-1, minInt, indicesOfPhiAlternatives(v))
       |
       |def maxOfPhiIndices(v: String): Int =
       |  fold(-1, maxInt, indicesOfPhiAlternatives(v))
       |
       |def getPhiAlternatives(v: String, method: String): Set[ArgList] =
       |  let minIdx = minOfPhiIndices(v) in
       |    getPhiAlternativesHelper(v, method, minIdx)
       |
       |def getPhiAlternativesHelper(v: String, method: String, currentIdx: Int): Set[ArgList] =
       |  let maxIdx = maxOfPhiIndices(v) in
       |  let instPrefix = method + "/phi-assign/" in
       |    {
       |      ConsArg(arg, rest) |
       |        currentIdx <= maxIdx,
       |        (instPrefix + currentIdx.`toString`, _, from, v, method) in _AssignLocal,
       |        arg in assignExp(from),
       |        rest in getPhiAlternativesHelper(v, method, currentIdx + 1)
       |    } ++ {
       |      NilArg() | currentIdx > maxIdx
       |    }
       |""".stripMargin

  def module(contents: String*): String = {
    s"""module Programm
       |${contents.mkString("\n")}
       |""".stripMargin
  }

  val genStmFun: String =
    s"""def genStm(inst: String): Set[Stm] =
       |  { ArrayWrite(toExp, NumLit(`String.valueOf`(num)), fromExp) |
       |      (inst, idx, from, to, meth) in _StoreArrayIndex,
       |      fromExp in assignExp(from),
       |      toExp in assignExp(to),
       |      (inst, num) in _ArrayNumIndex
       |  } ++ {
       |    ArrayWrite(toExp, indexExp, fromExp) |
       |      (inst, idx, from, to, meth) in _StoreArrayIndex,
       |      fromExp in assignExp(from),
       |      toExp in assignExp(to),
       |      (inst, index) in _ArrayInsnIndex,
       |      indexExp in assignExp(index)
       |  } ++ {
       |    InstanceFieldWrite(recvExp, field, valExp) |
       |      (inst, idx, val, recv, field, meth) in _StoreInstanceField,
       |      recvExp in assignExp(recv),
       |      valExp in assignExp(val)
       |  } ++ {
       |    StaticFieldWrite(field, valExp) |
       |      (inst, idx, val, field, meth) in _StoreStaticField,
       |      valExp in assignExp(val)
       |  } ++ {
       |    ReturnVoid() | (inst, idx, meth) in _ReturnVoid
       |  } ++ {
       |    Return(exp) |
       |      (inst, idx, v, meth) in _Return,
       |      exp in assignExp(v)
       |  } ++ {
       |    InvokeStm(recvExp, meth, args) |
       |      (inst, v) not in _AssignReturnValue, // an invoke statement does not assign a value to
       |      (inst, idx, meth, recv, callingMeth) in _VirtualMethodInvocation,
       |      recvExp in assignExp(recv),
       |      args in getArgs(inst, 0)
       |  } ++ {
       |    StaticInvokeStm(meth, args) |
       |      (inst, v) not in _AssignReturnValue,
       |      (inst, idx, meth, callingMeth) in _StaticMethodInvocation,
       |      args in getArgs(inst, 0)
       |  } ++ {
       |    Goto(trg) | (inst, idx, trg, meth) in _Goto
       |  } ++ {
       |    If(op, lhs, rhs, trg) |
       |      (inst, idx, trg, _) in _If,
       |      (inst, op) in _OperatorAt,
       |      lhs in getIfOperand(inst, 1),
       |      rhs in getIfOperand(inst, 2)
       |  } ++ {
       |    TableSwitch(matcheeExp, cases) |
       |      (inst, idx, matchee, meth) in _TableSwitch,
       |      matcheeExp in assignExp(matchee),
       |      cases in getTableSwitchCases(inst)
       |  }
       |
       |def getIfOperand(inst: String, pos: Int): Set[Exp] =
       |  { NumLit(num) | (inst, pos, num) in _IfConstant } ++
       |  { exp | (inst, pos, v) in _IfVar, exp in assignExp(v) }
       |
       |// TODO how can I construct a switch statement? We do not have an index for each case
       |// def getTableSwitchVals(inst: String): Set[Int] = { v | (inst, v, trg) in _ TableSwitch_Target }
       |// def getTableSwitchCases(inst: String, v: Int): Set[CaseList] =
       |//   {
       |//     Case(num, trg, rest) |
       |//       (inst, v, trg) in _TableSwitch_Target,
       |//       rest in getTableSwitchCases(inst)
       |//   } ++ {
       |//     Default(trg) | (inst, trg) in _TableSwitch_DefaultTarget
       |//   }
       |
       |def getTableSwitchCases(switch: String): Set[CaseList] =
       |  let minVal = minValueOfCases(switch) in
       |    let valueList = sortedTableSwitchCaseValues(switch, minVal) in
       |      getTableSwitchCasesHelper(switch, valueList)
       |
       |def minValueOfCases(switch: String): Int =
       |  fold(-1, minInt, valuesOfTableSwitch(switch))
       |
       |// FIX GenerateDatalog throws error when we inline valuesOfTableSwitch
       |def maxValueOfCases(switch: String): Int =
       |  fold(-1, maxInt, valuesOfTableSwitch(switch))
       |
       |def valuesOfTableSwitch(switch: String): Set[Int] =
       |  { idx | (switch, idx, _) in _TableSwitch_Target }
       |
       |
       |def sortedTableSwitchCaseValues(switch: String, idx: Int): IntList =
       |  if (idx <= maxValueOfCases(switch))
       |    if ((switch, idx) in TableSwitch_CaseValue)
       |      ConsInt(idx, sortedTableSwitchCaseValues(switch, idx + 1))
       |    else
       |      sortedTableSwitchCaseValues(switch, idx + 1)
       |  else NilInt()
       |
       |
       |def getTableSwitchCasesHelper(switch: String, valueList: IntList): Set[CaseList] = valueList match {
       |  case NilInt() => { DefaultCase(trg) | (switch, trg) in _TableSwitch_DefaultTarget }
       |  case ConsInt(v, r) =>
       |    {
       |      ConsCase(v, trg, rest) |
       |        (switch, v, trg) in _TableSwitch_Target,
       |        rest in getTableSwitchCasesHelper(switch, r)
       |    }
       |}
       |
       |def maxIndexOfInstructions(method: String): Int =
       |  fold(-1, maxInt, indicesOfInstructions(method))
       |
       |def indicesOfInstructions(method: String): Set[Int] =
       |  { index | (inst, method) in Instruction_Method, (inst, index) in Instruction_Index }
       |
       |data IntList = ConsInt(Int, IntList) | NilInt()
       |
       |def sortedInstructionIndexList(method: String, idx: Int): IntList =
       |  if (idx <= maxIndexOfInstructions(method))
       |    if ((method, idx) in Method_Instruction_Index)
       |      ConsInt(idx, sortedInstructionIndexList(method, idx + 1))
       |    else
       |      sortedInstructionIndexList(method, idx + 1)
       |  else NilInt()
       |
       |def getStmList(method: String): Set[StmList] =
       |  let indexList = sortedInstructionIndexList(method, 0) in
       |      getStmListHelper(method, indexList)
       |
       |def getStmListHelper(method: String, indexList: IntList): Set[StmList] = indexList match {
       |  case NilInt() => { NilStm() }
       |  case ConsInt(idx, r) =>
       |    {
       |      ConsStm(stm, idx, rest) |
       |        (inst, method) in Instruction_Method,
       |        (inst, idx) in Instruction_Index,
       |        inst in Stm_Instruction,
       |        stm in genStm(inst),
       |        rest in getStmListHelper(method, r)
       |    } ++ {
       |      rest |
       |        (inst, method) in Instruction_Method,
       |        (inst, idx) in Instruction_Index,
       |        inst not in Stm_Instruction,
       |        rest in getStmListHelper(method, r)
       |    }
       |}
       |""".stripMargin

  val assignExpMain: String = module(expAdt, assignExpFun, "@main def main(v: String): Set[Exp] = { exp | exp in assignExp(v) }")

  val genStmMain: String = module(expAdt, stmAdt, assignExpFun, genStmFun, "@main def main(v: String): Set[Stm] = { stm | stm in genStm(v) }")

  val getStmListMain: String = module(expAdt, stmAdt, assignExpFun, genStmFun, "@main def main(meth: String): Set[StmList] = getStmList(meth)")


  val baseDir = s"souffle-frontend/doop-context-insensitive"

  val souffleSrc: BufferedSource = Source.fromFile(s"$baseDir/souffle-input-schema.dl")
  val souffleCode: String = souffleSrc.getLines().mkString("\n")
  souffleSrc.close()

  def testAssignExp(dir: String, name: meta.Term, expected: meta.Term): Unit = {
    val fun = FunctionalXSouffleExecutor.loadFunction(assignExpMain, souffleCode)
    val res = fun.execute("main", Seq(name), s"$baseDir/$dir", false)
    assert(res == fun.result(expected))
  }

  def testGenStm(dir: String, name: meta.Term, expected: meta.Term): Unit = {
    val fun = FunctionalXSouffleExecutor.loadFunction(genStmMain, souffleCode)
    val res = fun.execute("main", Seq(name), s"$baseDir/$dir", false)
    assert(res == fun.result(expected))
  }

  def testGetStmList(dir: String, name: meta.Term, expected: meta.Term): Unit = {
    val opts = FunctionalOptions()
    val fun = FunctionalXSouffleExecutor.loadFunction(getStmListMain, souffleCode, opts)
    val res = fun.execute("main", Seq(name), s"$baseDir/$dir", false)
    assert(res == fun.result(expected))
  }


  /**
   * Construct Expressions
   */
  test("simple addition example") {
    testAssignExp(
      "database-simple-add",
      q""""<Main: void main(java.lang.String[])>/y#_4"""",
      q"""BinOp("+", NumLit("3"), NumLit("5"))""")
  }

  test("nested addition example") {
    testAssignExp(
      "database-nested-add",
      q""""<Main: void main(java.lang.String[])>/z#_5"""",
      q"""BinOp("+", BinOp("+", NumLit("3"), NumLit("5")), BinOp("+", NumLit("3"), NumLit("5")))""")
  }

  test("cast example") {
    testAssignExp(
      "database-cast",
      q""""<Main: void main(java.lang.String[])>/l2#_4"""",
      q"""Cast(NumLit("12"), "long")""")
  }

  test("instanceof example") {
    testAssignExp(
      "database-instanceof",
      q""""<Main: void main(java.lang.String[])>/l2#_4"""",
      q"""InstanceOf(Alloc("12", "java.lang.String"), "java.lang.String")""")
  }

  test("simple unop")  {
    testAssignExp(
      "database-unop",
      q""""<Main: void main(java.lang.String[])>/l1#_3"""",
      q"""UnOp("len", Var("<Main: void main(java.lang.String[])>/@parameter0"))""")
  }

  test("array read with int") {
    testAssignExp(
      "database-array-read-int",
      q""""<Main: void main(java.lang.String[])>/l1#_3"""",
      q"""ArrayRead(Var("<Main: void main(java.lang.String[])>/@parameter0"), NumLit("12"))""")
  }

  test("array read with complex expression") {
    testAssignExp(
      "database-array-read-var",
      q""""<Main: void main(java.lang.String[])>/l4#_6"""",
      q"""ArrayRead(Var("<Main: void main(java.lang.String[])>/@parameter0"), BinOp("+", BinOp("+", NumLit("1"), NumLit("4")), NumLit("2")))""")
  }

  test("string length method call example") {
    testAssignExp(
      "database-string-length",
      q""""<Main: void main(java.lang.String[])>/l1#_3"""",
      q"""Invoke(ArrayRead(Var("<Main: void main(java.lang.String[])>/@parameter0"), NumLit("0")), "<java.lang.String: int length()>", NilArg())""")
  }

  test("method call example") {
    testAssignExp(
      "database-method-call",
      q""""<Main: void main(java.lang.String[])>/l3#_5"""",
      q"""BinOp("+", Invoke(Alloc("12", "java.lang.String"), "<java.lang.String: int length()>", NilArg()), NumLit("1"))""")
  }

  test("method call with multiple args example") {
    testAssignExp(
      "database-multiple-arg-method-call",
      q""""<Main: void main(java.lang.String[])>/l4#_7"""",
      q"""Invoke(SpecialInvoke("<Main: void main(java.lang.String[])>/new Point/0", "<Point: void <init>(int,int)>", ConsArg(NumLit("1"), ConsArg(NumLit("2"), NilArg()))), "<Point: Point add(int,int)>", ConsArg(NumLit("10"), ConsArg(NumLit("12"), NilArg())))""")
  }

  test("instance field read access ") {
    testAssignExp(
      "database-instance-field-read",
      q""""<Main: void main(java.lang.String[])>/l2#_5"""",
      q"""InstanceFieldRead(SpecialInvoke("<Main: void main(java.lang.String[])>/new Point/0", "<Point: void <init>(int,int)>", ConsArg(NumLit("1"), ConsArg(NumLit("2"), NilArg()))), "<Point: int x>")""")
  }

  test("static field read access ") {
    testAssignExp(
      "database-static-field-read",
      q""""<Main: void main(java.lang.String[])>/l1#_4"""",
      q"""StaticFieldRead("<Point: java.lang.String TY>")""")
  }

  test("static method call") {
    testAssignExp(
      "database-static-method-call",
      q""""<Main: void main(java.lang.String[])>/l1#_4"""",
      q"""StaticInvoke("<Point: Point genPoint(int,int)>", ConsArg(NumLit("1"), ConsArg(NumLit("2"), NilArg())))"""
    )
  }

  test("method call with null argument") {
    testAssignExp(
      "database-null-argument",
      q""""<Main: void main(java.lang.String[])>/l2#_5"""",
      q"""Invoke(StaticInvoke("<Point: Point genPoint(int,int)>", ConsArg(NumLit("1"), ConsArg(NumLit("2"), NilArg()))), "<Point: Point add(Point)>", ConsArg(Null(), NilArg()))""")
  }

  test("constructor call") {
    testAssignExp(
      "database-constructor-call",
      q""""<Main: void main(java.lang.String[])>/l1#_4"""",
      q"""SpecialInvoke("<Main: void main(java.lang.String[])>/new Point/0", "<Point: void <init>(int,int)>", ConsArg(NumLit("1"), ConsArg(NumLit("2"), NilArg())))""")
  }

  test("dynamic invocation (lambda)") {
    val varName = Lit.String("<Main: void main(java.lang.String[])>/$stack5")

    val bootMeth = Lit.String("<java.lang.invoke.LambdaMetafactory: java.lang.invoke.CallSite metafactory(java.lang.invoke.MethodHandles$Lookup,java.lang.String,java.lang.invoke.MethodType,java.lang.invoke.MethodType,java.lang.invoke.MethodHandle,java.lang.invoke.MethodType)>")
    val methName = Lit.String("accept")
    val expected = q"DynamicInvoke($bootMeth, $methName,  NilArg())"

    testAssignExp(
      "database-dynamic-invoke",
      varName,
      expected)
  }

  test("phi expression") {
    val name = Lit.String("<Main: void main(java.lang.String[])>/l2_$$A_2#_11")
    testAssignExp(
      "database-phi",
      name,
      q"""Phi(ConsArg(NumLit("2"), ConsArg(NumLit("3"), NilArg())))""")
  }

  // PhantomInvoke(Exp, String) Phantom invocations are invocations of methods belonging to phantom classes. Phantom classes are classes not part of the analyzed jar
  // Phantom method calls cannot be recovered because the database does not store them
  // The database only lists phantom types and phantom methods that are being used in the program
  // test("phantom method call") {
  //   testAssignExp(
  //     "database-phantom-method-call",
  //     q""""<Main: void main(java.lang.String[])>/l2#_5"""",
  //     q"""Invoke("<Main: void main(java.lang.String[])>/new Point/0", "<Point: void <init>(int,int)>", ConsArg(NumLit("1"), ConsArg(NumLit("2"), NilArg())))""")
  // }

  /**
   * Construct Statements
   */

  test("array write num index") {
    testGenStm(
      "database-array-write-num",
      q""""<Main: void main(java.lang.String[])>/write-array-idx/0"""",
      q"""ArrayWrite(Alloc("<Main: void main(java.lang.String[])>/new int[]/0", "int[]"), NumLit("1"), NumLit("12"))""")
  }

  test("array write complex index") {
    testGenStm(
      "database-array-write-complex",
      q""""<Main: void main(java.lang.String[])>/write-array-idx/0"""",
      q"""ArrayWrite(Alloc("<Main: void main(java.lang.String[])>/new int[]/0", "int[]"), BinOp("+", NumLit("2"), NumLit("2")), NumLit("12"))""")
  }

  test("instance field write") {
    testGenStm(
      "database-instance-field-write",
      q""""<Main: void main(java.lang.String[])>/write-field-x/0"""",
      q"""InstanceFieldWrite(SpecialInvoke("<Main: void main(java.lang.String[])>/new Point/0", "<Point: void <init>(int,int)>", ConsArg(NumLit("1"), ConsArg(NumLit("2"), NilArg()))), "<Point: int x>", NumLit("2"))""")
  }

  test("static field write") {
    testGenStm(
      "database-static-field-write",
      q""""<Main: void main(java.lang.String[])>/write-field-TY/0"""",
      q"""StaticFieldWrite("<Point: java.lang.String TY>", Alloc("POINT", "java.lang.String"))""")
  }

  test("return void") {
    testGenStm(
      "database-static-field-write",
      q""""<Main: void main(java.lang.String[])>/return-void/0"""",
      q"""ReturnVoid()""")
  }

  test("return numlit") {
    testGenStm(
      "database-return",
      q""""<Point: int maxX()>/return/0"""",
      q"""Return(NumLit("10"))""")
  }

  test("return field") {
    testGenStm(
      "database-return",
      q""""<Point: int getX()>/return/0"""",
      q"""Return(InstanceFieldRead(This(), "<Point: int x>"))""")
  }

  test("return complex expression") {
    testGenStm(
      "database-return",
      q""""<Point: Point add(int,int)>/return/0"""",
      q"""Return(SpecialInvoke("<Point: Point add(int,int)>/new Point/0", "<Point: void <init>(int,int)>", ConsArg(BinOp("+", InstanceFieldRead(This(), "<Point: int x>"), Var("<Point: Point add(int,int)>/@parameter0")), ConsArg(BinOp("+", InstanceFieldRead(This(), "<Point: int y>"), Var("<Point: Point add(int,int)>/@parameter1")), NilArg()))))""")
  }

  test("invoke statement") {
    testGenStm(
      "database-invoke-stm",
      q""""<Main: void main(java.lang.String[])>/Point.print/0"""",
      q"""InvokeStm(SpecialInvoke("<Main: void main(java.lang.String[])>/new Point/0", "<Point: void <init>(int,int)>", ConsArg(NumLit("1"), ConsArg(NumLit("2"), NilArg()))), "<Point: void print()>" ,NilArg())""")
  }

  test("goto statement") {
    testGenStm(
      "database-if",
      q""""<Main: void main(java.lang.String[])>/goto/0"""",
      q"""Goto(8)""")
  }

  test("if statement") {
    testGenStm(
      "database-if",
      q""""<Main: void main(java.lang.String[])>/if/0"""",
      q"""If("<=", NumLit("1"), NumLit("2"), 7)""")
  }

  test("table switch statement") {
    testGenStm(
      "database-switch",
      q""""<Main: void main(java.lang.String[])>/table-switch/0"""",
      q"""TableSwitch(BinOp("+", NumLit("2"), NumLit("1")), ConsCase(1, 5, ConsCase(2, 6, ConsCase(3, 7, DefaultCase(11)))))""")
  }

  test("test gen simple stm list") {
    testGetStmList(
      "database-stmt-list",
      q""""<Main: void main(java.lang.String[])>"""",
      q"""ConsStm(If("==", NumLit("1"), NumLit("1"), 5), 2, ConsStm(ReturnVoid(), 4, ConsStm(ReturnVoid(), 6, NilStm())))""")
  }

  // need to account for holes in the sequence of instruction indices
  test("test stmt list with phi") {
    testGetStmList(
      "database-if",
      q""""<Main: void main(java.lang.String[])>"""",
      q"""ConsStm(If("<=", NumLit("1"), NumLit("2"), 7), 4, ConsStm(Goto(8), 6, ConsStm(ReturnVoid(), 11, NilStm())))""")
  }
}
