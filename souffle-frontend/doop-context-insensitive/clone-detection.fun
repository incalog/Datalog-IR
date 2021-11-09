module CloneDetection

data ArgList = NilArg() | ConsArg(Exp, ArgList)
// TODO recover more info about new array and new multi array
data Exp = NumLit(String)
         | Var(String)
         | BinOp(String, Exp, Exp)
         | UnOp(String, Exp)
         | Cast(Exp, String)
         | InstanceOf(Exp, String)
         | StringLit(String)
         | Alloc(String, String)
         | SpecialAlloc(String, String, ArgList)
         | ArrayRead(Exp, Exp)
         | InstanceFieldRead(Exp, String)
         | StaticFieldRead(String)
         | Invoke(Exp, String, ArgList)
         | SpecialInvoke(Exp, String, ArgList)
         | SuperInvoke(Exp, String, ArgList)
         | StaticInvoke(String, ArgList)
         | DynamicInvoke(String, String, ArgList)
         | This()
         | Null()
         | CaughtException(String)
         | DummyVar() // TODO dummy var is always generated and we use it when there is no lhs or rhs for if

data CaseList = ConsCase(Int, Int, CaseList) | DefaultCase(Int)
data CatchList = ConsCatch() | NilCatch()
data Stm = Assign(String, String)
         | InvokeStm(Exp, String, ArgList)
         | SpecialInvokeStm(Exp, String, ArgList)
         | StaticInvokeStm(String, ArgList)
         | ArrayWrite(Exp, Exp, Exp)
         | InstanceFieldWrite(Exp, String, Exp)
         | StaticFieldWrite(String, Exp)
         | ReturnVoid()
         | Return(Exp)
         | Goto(Int)
         | If(String, Exp, Exp, Int)
         | TableSwitch(Exp, CaseList)
         | LookupSwitch(Exp, CaseList)
         | Throw(Exp)
         | ThrowNull()
         | Phi(String, ArgList)

data StmList = ConsStm(Stm, Int, StmList) | NilStm()

// TODO ClassConstant
// TODO MethodHandleConstant
// PolymorphicInvoke
// How can i generate them? What features do i need to use to get these in jimple?
def assignExp(v: String): Set[Exp] = {
    NumLit(num) | (inst, idx, num, v, meth) in _AssignNumConstant
  } ++ {
    BinOp(op, left, right) |
      (inst, idx, v, meth) in _AssignBinop,
      (inst, op) in _OperatorAt,
      left in getOperand(inst, 1),
      right in getOperand(inst, 2)
  } ++ {
    UnOp(op, exp) |
      (inst, idx, v, meth) in _AssignUnop,
      (inst, op) in _OperatorAt,
      exp in getOperand(inst, 1)
  } ++ {
    exp |
      (inst, idx, from, v, meth) in _AssignLocal,
      isPhiInstruction(inst) == false,
      exp in assignExp(from)
  } ++ { // we do not inline results of phi functions
    Var(v) |
      (inst, idx, from, v, meth) in _AssignLocal,
      isPhiInstruction(inst)
  } ++ {
    // TODO maybe remove the method prefix from the variable name
    Var(v) | (idx, meth, v) in _FormalParam
  } ++ {
    Cast(exp, ty) |
      (inst, idx, from, v, ty, meth) in _AssignCast,
      exp in assignExp(from)
  } ++ {
    InstanceOf(exp, ty) |
      (inst, idx, from, v, ty, meth) in _AssignInstanceOf,
      exp in assignExp(from)
  } ++ {
    Null() | (inst, idx, v, meth) in _AssignNull
  } ++ {
    This() | (meth, v) in _ThisVar
  } ++ {
    Alloc(heap, ty) |
      (inst1, idx1, heap, v, meth, line) in _AssignHeapAllocation,
      (v, ty) in _Var_Type,
      (inst2, idx2, specialmeth, v, meth) not in _SpecialMethodInvocation
  } ++ {
    SpecialAlloc(heap, specialmeth, args) |
      (inst1, idx1, heap, v, meth, line) in _AssignHeapAllocation,
      (inst2, idx2, specialmeth, v, callingMeth) in _SpecialMethodInvocation,
      args in getArgs(inst2, 0)
  } ++ {
    ArrayRead(exp, NumLit(`String.valueOf`(index))) |
       (inst, idx, v, from, meth) in _LoadArrayIndex,
       exp in assignExp(from),
       (inst, index) in _ArrayNumIndex
  } ++ {
    ArrayRead(exp, indexExp) |
       (inst, idx, v, from, meth) in _LoadArrayIndex,
       (inst, indexVar) in _ArrayInsnIndex,
       exp in assignExp(from),
       indexExp in assignExp(indexVar)
  } ++ {
    Invoke(recvExp, meth, args) |
      (inst, v) in _AssignReturnValue,
      (inst, idx, meth, recv, callingMeth) in _VirtualMethodInvocation,
      recvExp in assignExp(recv),
      args in getArgs(inst, 0)
  } ++ {
    SuperInvoke(recvExp, meth, args) |
      (inst, v) in _AssignReturnValue,
      (inst, idx, meth, recv, callingMeth) in _SuperMethodInvocation,
      recvExp in assignExp(recv),
      args in getArgs(inst, 0)
  } ++ {
    SpecialInvoke(recvExp, meth, args) |
      (inst, v) in _AssignReturnValue,
      (inst, idx, meth, recv, callingMeth) in _SpecialMethodInvocation,
      recvExp in assignExp(recv),
      args in getArgs(inst, 0)
  } ++ {
    StaticInvoke(meth, args) |
      (inst, v) in _AssignReturnValue,
      (inst, idx, meth, callingMeth) in _StaticMethodInvocation,
      args in getArgs(inst, 0)
  } ++ {
    DynamicInvoke(bootmeth, dynname, args) |
      (inst, v) in _AssignReturnValue,
      (inst, idx, bootmeth, dynname, dynretty, dynarity, dynparamtys, tag, callingMeth) in _DynamicMethodInvocation,
      args in getArgs(inst, 0)
  } ++ {
    InstanceFieldRead(recvExp, field) |
      (inst, idx, v, recv, field, meth) in _LoadInstanceField,
      recvExp in assignExp(recv)
  } ++ {
    StaticFieldRead(fieldsig) | (inst, idx, v, fieldsig, meth) in _LoadStaticField
  } ++ { // TODO order of caught exceptions????
    CaughtException(ex) |
      (handler, v) in ExceptionHandler_FormalParam,
      (handler, _, _, ex, _, _) in _ExceptionHandler
  }


def getOperand(inst: String, pos: Int): Set[Exp] =
  { NumLit(num) | (inst, pos, num) in _AssignOperFromConstant } ++
  { exp | (inst, pos, var) in _AssignOperFrom, exp in assignExp(var) }

def getArgs(inst: String, currentIdx: Int): Set[ArgList] =
  {
    ConsArg(exp, rest) |
      (currentIdx, inst, v) in _ActualParam,
      exp in assignExp(v),
      rest in getArgs(inst, currentIdx + 1)
  } ++ {
    NilArg() | (currentIdx, inst, v) not in _ActualParam
  }

def maxInt(x: Int, y: Int): Int =
  if (x > y)
    x
  else if (x < y)
    y
  else
    x

def minInt(x: Int, y: Int): Int =
  if (x > y)
    y
  else if (x < y)
    x
  else
    x

def lastIndexOfPhiPrefix(v: String): Int = (v.`lastIndexOf`("phi-assign/")) + 11

def indexOfPhiInstruction(inst: String): Int =
  let idx = lastIndexOfPhiPrefix(inst) in
    let phiIdx = inst.`substring`(idx) in
      phiIdx.`toInt`

def indicesOfPhiAlternatives(v: String): Set[Int] =
  { indexOfPhiInstruction(inst) | (inst, _, _, v, _) in _AssignLocal }

def prefixOfPhiInstruction(inst: String): String =
  inst.`substring`(0, lastIndexOfPhiPrefix(inst))

def instructionPrefixOfPhiAlternatives(v: String): Set[String] =
  { prefixOfPhiInstruction(inst) | (inst, _, _, v, _) in _AssignLocal }

def minOfPhiIndices(v: String): Int =
  fold(`Int.MaxValue`, minInt, indicesOfPhiAlternatives(v))

def maxOfPhiIndices(v: String): Int =
  fold(-1, maxInt, indicesOfPhiAlternatives(v))

def getPhiAlternatives(v: String, method: String): Set[ArgList] =
  let minIdx = minOfPhiIndices(v) in
    getPhiAlternativesHelper(v, method, minIdx)

def getPhiAlternativesHelper(v: String, method: String, currentIdx: Int): Set[ArgList] =
  let maxIdx = maxOfPhiIndices(v) in
  let instPrefix = method + "/phi-assign/" in
    {
      ConsArg(exp, rest) |
        currentIdx <= maxIdx,
        (instPrefix + currentIdx.`toString`, _, from, v, _) in _AssignLocal,
        exp in assignExp(from),
        rest in getPhiAlternativesHelper(v, method, currentIdx + 1)
    } ++ {
      NilArg() | currentIdx > maxIdx
    }

def genStm(inst: String): Set[Stm] =
  { ArrayWrite(toExp, NumLit(`String.valueOf`(num)), fromExp) |
      (inst, _, from, to, _) in _StoreArrayIndex,
      fromExp in assignExp(from),
      toExp in assignExp(to),
      (inst, num) in _ArrayNumIndex
  } ++ {
    ArrayWrite(toExp, indexExp, fromExp) |
      (inst, _, from, to, _) in _StoreArrayIndex,
      fromExp in assignExp(from),
      toExp in assignExp(to),
      (inst, index) in _ArrayInsnIndex,
      indexExp in assignExp(index)
  } ++ {
    InstanceFieldWrite(recvExp, field, valExp) |
      (inst, _, val, recv, field, _) in _StoreInstanceField,
      recvExp in assignExp(recv),
      valExp in assignExp(val)
  } ++ {
    StaticFieldWrite(field, valExp) |
      (inst, _, val, field, _) in _StoreStaticField,
      valExp in assignExp(val)
  } ++ {
    ReturnVoid() | (inst, _, _) in _ReturnVoid
  } ++ {
    Return(exp) |
      (inst, _, v, _) in _Return,
      exp in assignExp(v)
  } ++ {
    InvokeStm(recvExp, meth, args) |
      (inst, v) not in _AssignReturnValue, // an invoke statement does not assign a value to
      (inst, _, meth, recv, _) in _VirtualMethodInvocation,
      recvExp in assignExp(recv),
      args in getArgs(inst, 0)
  } ++ {
    SpecialInvokeStm(recvExp, meth, args) |
      (inst, v) not in _AssignReturnValue, // an invoke statement does not assign a value to
      (inst, _, meth, recv, _) in _SpecialMethodInvocation,
      (meth, simplename, _, _, _, _, _) in _Method,
      simplename != "<init>",
      recvExp in assignExp(recv),
      args in getArgs(inst, 0)
  } ++ {
    StaticInvokeStm(meth, args) |
      (inst, v) not in _AssignReturnValue,
      (inst, _, meth, _) in _StaticMethodInvocation,
      args in getArgs(inst, 0)
  } ++ {
    Goto(trg) | (inst, _, trg, _) in _Goto
  } ++ {
    If(op, lhs, rhs, trg) |
      (inst, _, trg, _) in _If,
      (inst, op) in _OperatorAt,
      lhs in getIfOperand(inst, 1),
      rhs in getIfOperand(inst, 2)
  } ++ {
    TableSwitch(matcheeExp, cases) |
      (inst, _, matchee, _) in _TableSwitch,
      matcheeExp in assignExp(matchee),
      cases in getTableSwitchCases(inst)
  } ++ {
    LookupSwitch(matcheeExp, cases) |
      (inst, _, matchee, _) in _LookupSwitch,
      matcheeExp in assignExp(matchee),
      cases in getLookupSwitchCases(inst)
  } ++ {
    Throw(exp) |
      (inst, _, v, _) in _Throw,
      exp in assignExp(v)
  } ++ {
    ThrowNull() |
      (inst, _, _) in _ThrowNull
  } ++ {
    Phi(v, alts) |
      (inst, _, _, v, method) in _AssignLocal,
      isPhiInstruction(inst),
      // count(_AssignLocal(_, _, _, v, _)) > 1,
      alts in getPhiAlternatives(v, method)
  }


def isPhiInstruction(inst: String): Boolean = inst.`contains`("/phi-assign/")

def targetVarOfPhiInstruction(inst: String): Set[String] = {
  { v | (inst, _, _, v, _) in _AssignLocal }
}

def getLongerString(s1: String, s2: String): String =
  if ((s1.`length`) > (s2.`length`))
    s1
  else
    s2

// we only call this function if we are sure the instruction is a phi instruction
def isFirstPhiInstruction(inst: String): Boolean = {
  // FIX the fold is a workaround because we cannot select a single element when accessing a relation
  let v = fold("", getLongerString, targetVarOfPhiInstruction(inst)) in
    minOfPhiIndices(v) == indexOfPhiInstruction(inst)
}

def shouldGenerateStm(inst: String): Boolean = {
  if (inst in stmInstructions())
    if (isPhiInstruction(inst)) isFirstPhiInstruction(inst)
    else true
  else false
}

def getIfOperand(inst: String, pos: Int): Set[Exp] =
  { NumLit(num) | (inst, pos, num) in _IfConstant } ++
  { exp | (inst, pos, v) in _IfVar, exp in assignExp(v) } ++
  { DummyVar() | (inst, _) in _DummyIfVar, (inst, pos, _) not in _IfConstant, (inst, pos, _) not in _IfVar }

def getTableSwitchCases(switch: String): Set[CaseList] =
  let minVal = minValue(() => valuesOfTableSwitch(switch)) in
    if (minVal == `Int.MaxValue`)
      getTableSwitchCasesHelper(switch, NilInt())
    else
      let valueList = sortedTableSwitchCaseValues(switch, minVal) in
        getTableSwitchCasesHelper(switch, valueList)

def getLookupSwitchCases(switch: String): Set[CaseList] =
  // there are no values
  if (count(_LookupSwitch_Target(switch, _, _)) == 0)
    getLookupSwitchCasesHelper(switch, NilInt())
  else
    let minVal = minValue(() => valuesOfLookupSwitch(switch)) in
      let valueList = sortedLookupSwitchCaseValues(switch, minVal) in
        getLookupSwitchCasesHelper(switch, valueList)

def minValue(values: () => Set[Int]): Int =
  fold(`Int.MaxValue`, minInt, values())

def minValueOfLookupSwitch(switch: String): Int =
  fold(`Int.MaxValue`, minInt, valuesOfLookupSwitch(switch))

def minValueOfTableSwitch(switch: String): Int =
  fold(`Int.MaxValue`, minInt, valuesOfTableSwitch(switch))

// FIX GenerateDatalog throws error when we inline valuesOfTableSwitch
def maxValue(values: () => Set[Int]): Int =
  fold(-1, maxInt, values())

def valuesOfTableSwitch(switch: String): Set[Int] =
  { idx | (switch, idx, _) in _TableSwitch_Target }

def valuesOfLookupSwitch(switch: String): Set[Int] =
  { idx | (switch, idx, _) in _LookupSwitch_Target }

def sortedTableSwitchCaseValues(switch: String, idx: Int): IntList =
  if (idx <= maxValue(() => valuesOfTableSwitch(switch)))
    if ((switch, idx) in TableSwitch_CaseValue)
      ConsInt(idx, sortedTableSwitchCaseValues(switch, idx + 1))
    else
      sortedTableSwitchCaseValues(switch, idx + 1)
  else NilInt()

def sortedLookupSwitchCaseValues(switch: String, idx: Int): IntList =
  if (idx <= maxValue(() => valuesOfLookupSwitch(switch)))
    if ((switch, idx) in LookupSwitch_CaseValue)
      ConsInt(idx, sortedLookupSwitchCaseValues(switch, idx + 1))
    else
      sortedLookupSwitchCaseValues(switch, idx + 1)
  else NilInt()

def getTableSwitchCasesHelper(switch: String, valueList: IntList): Set[CaseList] = valueList match {
  case NilInt() => { DefaultCase(trg) | (switch, trg) in _TableSwitch_DefaultTarget }
  case ConsInt(v, r) =>
    {
      ConsCase(v, trg, rest) |
        (switch, v, trg) in _TableSwitch_Target,
        rest in getTableSwitchCasesHelper(switch, r)
    }
}

def getLookupSwitchCasesHelper(switch: String, valueList: IntList): Set[CaseList] = valueList match {
  case NilInt() => { DefaultCase(trg) | (switch, trg) in _LookupSwitch_DefaultTarget }
  case ConsInt(v, r) =>
    {
      ConsCase(v, trg, rest) |
        (switch, v, trg) in _LookupSwitch_Target,
        rest in getLookupSwitchCasesHelper(switch, r)
    }
}

def getCatchClauses(inst: String): Set[CatchList] = { NilCatch() }

def maxIndexOfInstructions(method: String): Int =
  fold(-1, maxInt, indicesOfInstructions(method))

def indicesOfInstructions(method: String): Set[Int] =
  { index | (inst, method) in Instruction_Method, (inst, index) in Instruction_Index }

data IntList = ConsInt(Int, IntList) | NilInt()

def sortedInstructionIndexList(method: String, idx: Int): IntList =
  if (idx <= maxIndexOfInstructions(method))
    if ((method, idx) in Method_Instruction_Index)
      ConsInt(idx, sortedInstructionIndexList(method, idx + 1))
    else
      sortedInstructionIndexList(method, idx + 1)
  else NilInt()

def getStmList(method: String): Set[StmList] =
  let indexList = sortedInstructionIndexList(method, 0) in
      getStmListHelper(method, indexList)

def getStmListHelper(method: String, indexList: IntList): Set[StmList] = indexList match {
  case NilInt() => { NilStm() }
  case ConsInt(idx, r) =>
    {
      ConsStm(stm, idx, rest) |
        (inst, method) in Instruction_Method,
        (inst, idx) in Instruction_Index,
        shouldGenerateStm(inst) == true,
        stm in genStm(inst),
        rest in getStmListHelper(method, r)
    } ++ {
      rest |
        (inst, method) in Instruction_Method,
        (inst, idx) in Instruction_Index,
        shouldGenerateStm(inst) == false,
        rest in getStmListHelper(method, r)
    }
}



// everything used in this function is ground hence it is allowed to avoid generating demand relation
@nodemand def stmInstructions(): Set[String] =
  {
    inst | (inst, _, _, _, _) in _StoreArrayIndex
  } ++ {
    inst | (inst, _, _, _, _) in _StoreStaticField
  } ++ {
    inst | (inst, _, _, _, _, _) in _StoreInstanceField
  } ++ {
    inst | (inst, _, _) in _ReturnVoid
  } ++ {
    inst | (inst, _, _, _) in _Return
  } ++ {
    inst | (inst, _, _, _) in _Goto
  } ++ {
    inst | (inst, _, _, _) in _If
  } ++ {
    inst | (inst, _, _, _) in _TableSwitch
  } ++ {
    inst | (inst, _, _, _) in _LookupSwitch
  } ++ {
    inst | (inst, _, _, _) in _EnterMonitor
  } ++ {
    inst | (inst, _, _, _) in _ExitMonitor
  } ++ {
    inst | (inst, _, _, _) in _Throw
  } ++ {
    inst | (inst, _, _) in _ThrowNull
  } ++ {
    inst | (inst, _, _, _, _) in _VirtualMethodInvocation, (inst, _) not in _AssignReturnValue
  } ++ {
    inst | (inst, _, _, _, _) in _SuperMethodInvocation, (inst, _) not in _AssignReturnValue
  } ++ {
    inst | (inst, _, sig, _, _) in _SpecialMethodInvocation, (inst, _) not in _AssignReturnValue, (sig, name, _, _, _, _, _) in _Method, name != "<init>"
  } ++ {
    inst | (inst, _, from, to, _) in _AssignLocal, inst.`contains`("/phi-assign/")
  }

// TODO implement meaningful simple clonedetection function
// def isCloneLvl1(s1: StmList, s2: StmList): Boolean = true
//
// def getClonedMethods(clonePred: (StmList, StmList) => Boolean): Set[(String, String)] =
//   { (meth1, meth2) |
//       (meth1, _, _, _, _, _, _) in _Method,
//       (meth2, _, _, _, _, _, _) in _Method,
//       meth1 != meth2,
//       stms1 in getStmList(meth1),
//       stms2 in getStmList(meth2),
//       clonePred(stms1, stms2)
//   }
//
// def test(): Set[(String, String)] = getClonedMethods(isCloneLvl1)