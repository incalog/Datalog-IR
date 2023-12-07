package inca.frontend.oodl.compile

import inca.frontend.oodl.syntax.*

/**
 * Problem: SID and OID do not include information about the class hierarchy. However, we want to generate a scala
 * representation of an SID or OID object that has dynamic dispatching and an inheritance hierarchy, without needing
 * coalescing and uncoalescing.
 *
 * Proposed solution based on an example:
 *
 * class A:
 *  def something(): Unit
 * case class B(var i: Int) extends A
 * case class C (var j: Int) extends A:
 *  override def something(): Unt = ...
 *
 * ~> SID$Int(TString, TInt) used to represent instances of B and C
 *
 * extension (id: SID$Int)
 *  def something(): Unit = id.param$0 match
 *    case "A" => something$A()
 *    case "B" => something$A() // does not override the method
 *    case "C" => something$C()
 *
 * That way we can write code such as:
 * val s = new C(0)
 * s.something()
 * ~>
 * val s = SID$Int("C", 0)
 * s.something()
 */
class GenerateScala:
  type Code = String

  private var visited: Map[Any, Seq[Code]] = Map()
  private def createIfNeeded(a: Any)(f: => Seq[Code]): Unit = visited.get(a) match {
    case None =>
      this.visited += a -> Seq()
      val stats = f
      this.visited += a -> stats
    case Some(_) => // nothing
  }

  def genClassDef(cls: ClassDef): Unit = createIfNeeded(cls) {
    val methods: Seq[Code] = ???
    val constructors: Seq[Code] = ???
    val fields: Seq[Code] = ???
    methods ++ constructors ++ fields
  }

  private def transParam(param: Param): Code = s"${param.name}: ${transType(param.typ)}"

  private def transMethod(method: MethodDef): Code =
    val params = method.params.map(transParam).mkString(",")
    val outTy = transType(method.outType)
    val body = transStatements(method.body).indent(4)
    s"def ${method.name}($params): outTy = {\n$body}"

  private def transMethodGroup(methods: Seq[MethodDef]): Code =
    val baseCls: ClassDef = ???
    val baseTy = transType(TName(baseCls.name, Seq()))
    val reprMethod: MethodDef = ???
    val reprMethodParam = reprMethod.params.map(transParam).mkString(",")
    val reprMethodOutTy = transType(reprMethod.outType)
    val methodCases = ???
    val concreteMethodsImpl = methods.map(transMethod)
    s"""
     extension (this$$0: $baseTy)
     | $concreteMethodsImpl
     | def ${reprMethod.name}($reprMethodParam}): $reprMethodOutTy = this$$0 match
     | $methodCases
     """.stripMargin

  private def transStatements(stmts: Seq[Statement]): Code = stmts.map(transStatement).mkString("\n")

  private def transStatement(stmt: Statement): Code = stmt match
    case Expr(expression) => transExpression(expression)
    case Return(expression) => s"return ${transExpression(expression)}"
    case Assign(lhs, "=", rhs) => s"$lhs = $rhs"
    case Assign(lhs, "+=", rhs) =>
      ???
    case Assign(lhs, op, rhs) => throw IllegalArgumentException(s"Unsupported assignment operation $op")
    case VarDeclare(name, maybeTy, maybeExpression, immutable) =>
      val kw = if (immutable) "val" else "var"
      (maybeTy, maybeExpression) match
        case (Some(ty), Some(exp)) => s"$kw $name: ${transType(ty)} = ${transExpression(exp)}"
        case (Some(ty), _)         => s"$kw $name: ${transType(ty)} = null"
        case (None, Some(exp))     => s"$kw $name = ${transExpression(exp)}"
        case (None, Some(exp))     => s"$kw $name = null"
    case Super(args) =>
      ???
    case If(cnd, thn, els) =>
      val cndCode = transExpression(cnd)
      val thnCode = transStatements(thn).indent(4)
      val elsCode = transStatements(els).indent(4)
      s"if ($cond) {\n$thnCode} else {\n$elsCode}"
    case VarPhiAssign(name, typ, ifStmt, thnName, elsName) =>
      throw IllegalStateException("Can not translate SSA transformed OODL program to Scala.")

  private def transExpression(expr: Expression): Code = expr match
    case Var(name) =>
      s"$name"
    case Select(recv, targetName) =>
      s"${transExpression(recv)}.$targetName"
    case ConstructorCall(name, tyArgs, args) =>
      ???
    case MethodCall(recv, fun, tyArgs, args, isFix) =>
      val recvCode = transExpression(recv)
      val tyCode = tyArgs.map(transType).mkString(",")
      val argsCode = args.map(transExpression).mkString(",")
      s"$recvCode.$fun[$tyCode]($argsCode)"
    case TypeCast(recv, toTyp) =>
      ???
    case InstanceOf(recv, ofTyp) =>
      ???
    case TupleExp(exps) =>
      exps.map(transExpression).mkString("(", ",", ")")
    case SetExp(exps, maybeType) =>
      val argsCode = exps.map(transExpression).mkString(",")
      maybeType match
        case Some(ty) => s"Set[${transType(ty)}]($argsCode)"
        case _        => s"Set($argsCode)"
    case SetMember(name, recv, predicate) =>
      s"$name <- ${transExpression(recv)} if ${transExpression(predicate)}"
    case SetComprehension(member, body) =>
      val memberCode = member.map(transExpression).mkString(";")
      val bodyCode = transExpression(body)
      s"for ($memberCode) yield $bodyCode"
    case NullLit() =>
      ???
    case BoolLit(b) => s"$b"
    case IntLit(i) => s"$i"
    case DoubleLit(d) => s"$d"
    case StringLit(s) => s""""$s""""
    case BinOp(e1, op, e2) => s"${transExp(e1)} $op ${transExp(e2)}"
    case UnOp(op, e) => s"$op$e"

  private def transType(typ: Type): Code = typ match
    case TAny => "Any"
    case TNull => ???
    case TTuple(ts) => ts.map(transType).mkString("(", ",", ")")
    case t: TName if t.isBuiltIn => t.name.name
    case TName(name, Seq()) => t.name.name
    case TName(name, tyArgs) =>
      val tyCode = tyArgs.map(transType).mkString(",")
      s"$name[$tyCode]"
    case TSet(ty) => s"Set[${transType(ty)}]"