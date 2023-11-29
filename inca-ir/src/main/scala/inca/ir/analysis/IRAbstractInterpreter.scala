package inca.ir.analysis

import inca.ir.Name

enum Value:
  case Top
  case Int(i: scala.Int)
  case Double(d: scala.Double)
  case String(s: Predef.String)
  case Data(name: Predef.String, args: Seq[Value])

  def join(that: Value): Value = (this, that) match
    case (Top, _) | (_, Top) => Top
    case (Int(i1), Int(i2)) if i1 == i2 => this
    case (Double(d1), Double(d2)) if d1 == d2 => this
    case (String(s1), String(s2)) if s1 == s2 => this
    case (Data(name1, args1), Data(name2, args2))
      if name1 == name2 && args1.size == args2.size => Data(name1, args1.zip(args2).map(_.join(_)))
    case _ => Top

enum VBool:
  case Top
  case True
  case False
  def join(that: VBool): VBool =
    if (this == that)
      this
    else
      Top

class IRAbstractInterpreter extends BaseAbstractInterpreter[Value, VBool]
  with ArithmeticAbstractInterpreter[Value, VBool]
  with DataAbstractInterpreter[Value, VBool]
  with StringAbstractInterpreter[Value, VBool]
  with AggregateAbstractInterpreter[Value, VBool]:

  override def top: Value = Value.Top
  override def topBool: VBool = VBool.Top


  override val boolOps: BooleanOps[VBool] = new BooleanOps[VBool]:
    import VBool.*
    override def boolLit(b: Boolean): VBool = if (b) True else False
    override def and(v1: VBool, v2: VBool): VBool = (v1, v2) match
      case (True, _) => v2
      case (False, _) => False
      case (Top, False) => False
      case _ => Top
    override def or(v1: VBool, v2: VBool): VBool = (v1, v2) match
      case (True, _) => True
      case (False, _) => v2
      case (Top, True) => True
      case _ => Top
    override def not(v: VBool): VBool = v match
      case Top => Top
      case True => False
      case False => True

  override val eqOps: EqOps[Value, VBool] = new EqOps:
    override def equ(v1: Value, v2: Value): VBool = (v1, v2) match
      case (Value.Int(i1), Value.Int(i2)) => boolOps.boolLit(i1 == i2)
      case (Value.Double(d1), Value.Double(d2)) => boolOps.boolLit(d1 == d2)
      case _ => VBool.Top
    override def nequ(v1: Value, v2: Value): VBool = (v1, v2) match
      case (Value.Int(i1), Value.Int(i2)) => boolOps.boolLit(i1 != i2)
      case (Value.Double(d1), Value.Double(d2)) => boolOps.boolLit(d1 != d2)
      case _ => VBool.Top

  override val intOps: IntegerOps[Int, Value] = new IntegerOps[Int, Value]:
    override def integerLit(i: Int): Value = Value.Int(i)
    override def add(v1: Value, v2: Value): Value = (v1, v2) match
      case (Value.Int(i1), Value.Int(i2)) => Value.Int(i1 + i2)
      case _ => Value.Top
    override def sub(v1: Value, v2: Value): Value = (v1, v2) match
      case (Value.Int(i1), Value.Int(i2)) => Value.Int(i1 - i2)
      case _ => Value.Top
    override def mul(v1: Value, v2: Value): Value = (v1, v2) match
      case (Value.Int(i1), Value.Int(i2)) => Value.Int(i1 * i2)
      case _ => Value.Top
    override def max(v1: Value, v2: Value): Value = (v1, v2) match
      case (Value.Int(i1), Value.Int(i2)) => Value.Int(i1 max i2)
      case _ => Value.Top
    override def min(v1: Value, v2: Value): Value = (v1, v2) match
      case (Value.Int(i1), Value.Int(i2)) => Value.Int(i1 min i2)
      case _ => Value.Top
    override def div(v1: Value, v2: Value): Value = (v1, v2) match
      case (Value.Int(i1), Value.Int(i2)) => Value.Int(i1 / i2)
      case _ => Value.Top
    override def remainder(v1: Value, v2: Value): Value = (v1, v2) match
      case (Value.Int(i1), Value.Int(i2)) => Value.Int(i1 % i2)
      case _ => Value.Top
    override def absolute(v: Value): Value = v match
      case Value.Int(i) => Value.Int(i.abs)
      case _ => Value.Top

  override val doubleOps: FloatOps[Double, Value] = new FloatOps[Double, Value]:
    override def floatingLit(f: Double): Value = Value.Double(f)
    override def randomFloat(): Value = ???
    override def add(v1: Value, v2: Value): Value = (v1, v2) match
      case (Value.Double(i1), Value.Double(i2)) => Value.Double(i1 + i2)
      case _ => Value.Top
    override def sub(v1: Value, v2: Value): Value = (v1, v2) match
      case (Value.Double(i1), Value.Double(i2)) => Value.Double(i1 - i2)
      case _ => Value.Top
    override def mul(v1: Value, v2: Value): Value = (v1, v2) match
      case (Value.Double(i1), Value.Double(i2)) => Value.Double(i1 * i2)
      case _ => Value.Top
    override def max(v1: Value, v2: Value): Value = (v1, v2) match
      case (Value.Double(i1), Value.Double(i2)) => Value.Double(i1 max i2)
      case _ => Value.Top
    override def min(v1: Value, v2: Value): Value = (v1, v2) match
      case (Value.Double(i1), Value.Double(i2)) => Value.Double(i1 min i2)
      case _ => Value.Top
    override def div(v1: Value, v2: Value): Value = (v1, v2) match
      case (Value.Double(i1), Value.Double(i2)) => Value.Double(i1 / i2)
      case _ => Value.Top
    override def absolute(v: Value): Value = v match
      case Value.Double(i) => Value.Double(i.abs)
      case _ => Value.Top

  override val intOrderingOps: OrderingOps[Value, VBool] = new OrderingOps[Value, VBool]:
    override def lt(v1: Value, v2: Value): VBool = (v1, v2) match
      case (Value.Int(i1), Value.Int(i2)) => boolOps.boolLit(i1 < i2)
      case _ => VBool.Top
    override def le(v1: Value, v2: Value): VBool = (v1, v2) match
      case (Value.Int(i1), Value.Int(i2)) => boolOps.boolLit(i1 <= i2)
      case _ => VBool.Top

  override val doubleOrderingOps: OrderingOps[Value, VBool] = new OrderingOps[Value, VBool]:
    override def lt(v1: Value, v2: Value): VBool = VBool.Top
    override def le(v1: Value, v2: Value): VBool = VBool.Top

  override val stringOps: StringOps[Value] = new StringOps[Value]:
    override def stringLit(s: String): Value = Value.String(s)
    override def concat(v1: Value, v2: Value): Value = (v1, v2) match
      case (Value.String(s1), Value.String(s2)) => Value.String(s1 + s2)
      case _ => Value.Top

  override val dataOps: DataOps[Value] = new DataOps[Value]:
    override def construct(name: String, args: Seq[Value]): Value = Value.Data(name, args)
    override def deconstruct(v: Value, name: String, fail: () => AtomResult)
                            (success: Seq[Value] => AtomResult): AtomResult = v match
      case Value.Data(`name`, args) => success(args)
      case Value.Top =>
        val afail = fail()
        val asucc = success(LazyList.continually(top))
        AtomResult(afail.value.join(asucc.value), afail.pure.join(asucc.pure))
      case _ => fail()

