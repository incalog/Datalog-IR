package inca.frontend.oodl.compile

import inca.frontend.oodl.syntax.*
import inca.frontend.oodl.syntax.Type.signatureString
import inca.ir.Name

/**
 * Problem: SID and OID do not include information about the class hierarchy. However, we want to generate a scala
 * representation of an SID or OID object that has dynamic dispatching and an inheritance hierarchy, without needing
 * coalescing and uncoalescing.
 * Solution: We model the same dynamic dispatching we use in Datalog in Scala.
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
 *  def something$(this: ID): Unit =
 *    val cls = this$0 match
 *      case OID(c, _) => c
 *      case SID$Int$Int(c, _, _) => c
 *    cls match
 *      case "A" => something$A()
 *      case "B" => something$A() // does not override the method
 *      case "C" => something$C()
 *
 * That way we can write OODL code such as:
 * val s = new C(0)
 * s.something()
 *
 * and translate it to ~>
 *
 * val s = SID$Int("C", 0)
 * something$(s)
 */

// TODO: Refactor this to share common code to create dispatch table with GenerateIR

class GenerateScala:
  type Code = String

  /** Helper */

  /** Transitively collect all methods for a given qualified name */
  private def collectMethods(classDef: ClassDef)(implClass: ClassDef = classDef): Map[String, (ClassDef, MethodDef)] = {
    val methods = implClass.methods.map { m =>
      val qualifiedMethodName = s"${m.name}$$${signatureString(m.signature)}"
      qualifiedMethodName -> (implClass, m)
    }.toMap

    val parentMethods = implClass.parentCls.flatMap {
      case t: TName if !t.isBuiltIn => t.target match
        case Some(parentClassDef: ClassDef) => collectMethods(classDef)(parentClassDef)
        case _ => throw IllegalStateException(s"Unresolved ClassDef ${t.name}")
      case t => throw IllegalStateException(s"Unexpected type $t")
    }.toMap
    // We rely on the default map collision behaviour to find the concrete implementation class
    parentMethods ++ methods
  }

  def collectParentClasses(classDef: ClassDef, allClasses: Map[Name, ClassDef]): Seq[ClassDef] =
    classDef +: classDef.parentCls.flatMap {
      case TName(Name("Object"), _) => Seq()
      case ty@TName(name, _) if !ty.isBuiltIn =>
        val parentClass = allClasses(name)
        parentClass +: collectParentClasses(parentClass, allClasses)
      case _ => Seq()
    }

  /** Translation */

  // SID name -> Arity
  var builtinSIDCases: Map[String, Int] = Map()

  def transModule(module: Module): Code =
    val caseClasses = module.classes.filter(_.isCaseClass)
    val caseClassFields = caseClasses.map(c => c.name -> c.fields)
    builtinSIDCases = caseClassFields.map {
      case (name, fields) =>
        val signature = fields.map(_.typ)
        val qualifiedName = s"SID$$${signatureString(signature)}"
        qualifiedName -> signature.size
    }.toMap

    // Only translate case classes and their parent classes
    val classMap = module.classes.map(c => c.name -> c).toMap
    val classes = caseClasses.flatMap(c => collectParentClasses(c, classMap))

    val collectedMethods = classes.map(c => c -> collectMethods(c)()).toMap
    // qualifiedMethodName -> (src1, trg1), ...,(srcN, trgN)
    var dispatchTable: Map[String, Seq[(String, String)]] = Map()
    // qualifiedMethodName -> MethodDef1, ..., MethodDefN
    var qualifiedMethods: Map[String, Seq[MethodDef]] = Map()
    classes.foreach { c =>
      collectedMethods(c).foreach { case (qualifiedMethodName, (implClass, implMethod)) =>
        val previousTuples = dispatchTable.getOrElse(qualifiedMethodName, Seq())
        dispatchTable += qualifiedMethodName -> (previousTuples :+ (c.name.name, implClass.name.name))
        val previousMethods = qualifiedMethods.getOrElse(qualifiedMethodName, Seq())
        qualifiedMethods += qualifiedMethodName -> (previousMethods :+ implMethod)
      }
    }

    val methodsCode = qualifiedMethods.map {
      case (name, methods) => transMethodGroup(name, methods, dispatchTable(name).toMap)
    }.mkString("\n")

    val instanceOfCode = transIsInstanceOf(classes)
    val classesCode = module.classes.map(transClassDef).mkString("\n")
    s"$instanceOfCode$methodsCode\n$classesCode"

  private def transIsInstanceOf(classDefs: Seq[ClassDef]): Code = {
    val allClasses = classDefs.map(c => c.name -> c).toMap
    val subtypeRelationPairs = classDefs.flatMap { c =>
      val parentCls = collectParentClasses(c, allClasses)
      val parentSubtypeRelations = parentCls.map(p => (c.name, p.name))
      val selfSubtypeRelation = c.name -> c.name
      val nullSubtypeRelation = Name("Null") -> c.name
      Seq(nullSubtypeRelation, selfSubtypeRelation) ++ parentSubtypeRelations
    }.distinct
    val subtypeRelationCases = (subtypeRelationPairs.map { case (sub, parent) =>
      s"""case ("$sub", "$parent") => true"""
    } :+ "case _ => false").mkString("\n").indent(8)

    val sidExtractRuntimeClassCases = builtinSIDCases.map { case (sidCase, v) =>
      val wildcards = 0.until(v).map(_ => "_").mkString(", ")
      s"case $sidCase(c, $wildcards) => c"
    }.mkString("\n").indent(8)
    s"""
       |def isInstanceOf$$(this$$0: ID, ty: String) =
       |    val cls = this$$0 match
       |        case OID(c, _) => c
       |$sidExtractRuntimeClassCases
       |    (cls, ty) match
       |$subtypeRelationCases
       |""".stripMargin
  }

  private def transMethodGroup(qualifiedMethodName: String, methods: Seq[MethodDef], dispatchTable: Map[String, String]): Code =
    val thisParam = "this$0: ID"
    val reprMethod: MethodDef = methods.head
    val reprMethodParam = (thisParam +: reprMethod.params.map(transParam)).mkString(", ")
    val reprMethodOutTy = transType(reprMethod.outType)
    val methodCases = dispatchTable.map { case (src, trg) =>
      val methodCallArgs = ("this$0" +: reprMethod.params.map(p => p.name)).mkString(", ")
      s"""case "$src" => ${reprMethod.name}$$$trg($methodCallArgs)"""
    }.mkString("\n").indent(8)
    val sidExtractRuntimeClassCases = builtinSIDCases.map { case (sidCase, v) =>
      val wildcards = 0.until(v).map(_ => "_").mkString(", ")
      s"case $sidCase(c, $wildcards) => c"
    }.mkString("\n").indent(8)
    s"""
       |def $qualifiedMethodName($reprMethodParam): $reprMethodOutTy =
       |    val cls = this$$0 match
       |        case OID(c, _) => c
       |$sidExtractRuntimeClassCases
       |    cls match
       |$methodCases""".stripMargin

  def transClassDef(cls: ClassDef): Code =
    // Constructors and fields do not exist for case classes
    cls.methods.map(transMethodDef).mkString("\n")

  private def transParam(param: Param): Code = s"${param.name}: ${transType(param.typ)}"

  private def transMethodDef(method: MethodDef): Code =
    val classDef = method.target match
      case Some(cls) => cls
      case _ => throw IllegalArgumentException(s"Unresolved method definition $method")
    val thisParam = "this$0: ID"
    val params = (thisParam +: method.params.map(transParam)).mkString(", ")
    val outTy = transType(method.outType)
    val body = transStatements(method.body).indent(4)
    val name = s"${method.name}$$${classDef.name}"
    s"def $name($params): $outTy = \n$body"

  private def transFunctionDef(fun: FunctionDef): Code =
    val params = fun.params.map(transParam).mkString(", ")
    val outTy = transType(fun.outType)
    val body = transStatements(fun.body).indent(4)
    s"def ${fun.name}($params): $outTy = \n$body"

  private def transStatements(stmts: Seq[Statement]): Code = stmts.map(transStatement).mkString("\n")

  private def transStatement(stmt: Statement): Code = stmt match
    case Expr(expression) => transExpression(expression)
    case Return(expression) => s"return ${transExpression(expression)}"
    case Assign(lhs, Name("="), rhs) => s"$lhs = $rhs"
    case Assign(lhs, op, rhs) => throw IllegalArgumentException(s"Unsupported assignment operation $op")
    case VarDeclare(name, maybeTy, maybeExpression, immutable) =>
      val kw = if (immutable) "val" else "var"
      (maybeTy, maybeExpression) match
        case (Some(ty), Some(exp)) => s"$kw $name: ${transType(ty)} = ${transExpression(exp)}"
        case (Some(ty), _) => s"$kw $name: ${transType(ty)} = null"
        case (_, Some(exp)) => s"$kw $name = ${transExpression(exp)}"
        case _ => s"$kw $name = null"
    case Super(args) =>
      "" // Case classes do not support super calls
    case If(cnd, thn, els) =>
      val cndCode = transExpression(cnd)
      val thnCode = transStatements(thn).indent(4)
      val elsCode = transStatements(els).indent(4)
      s"if ($cndCode) {\n$thnCode} else {\n$elsCode}"
    case VarPhiAssign(name, typ, ifStmt, thnName, elsName) =>
      throw IllegalStateException("Can not translate SSA transformed OODL program to Scala.")

  private def transExpression(expr: Expression): Code = expr match
    case Var(Name("this")) =>
      "this$0"
    case Var(name) =>
      s"$name"
    case select@Select(recv, targetName) if recv.typ.exists(_.isInstanceOf[TTuple]) =>
      s"${transExpression(recv)}.$targetName"
    case select@Select(recv, targetName) =>
      val (classDef, fieldDef) = select.target match
        case Some((c, f)) => c -> f
        case _ => throw IllegalStateException(s"Unresolved target for select $recv.$targetName")
      if (classDef.isCaseClass)
        val fieldIndex = classDef.fields.indexWhere(_.name == targetName) + 1
        val constrDef = classDef.constructors.head
        val sidClass = s"SID$$${signatureString(constrDef.signature)}"
        s"${transExpression(recv)}.asInstanceOf[$sidClass].param_$fieldIndex"
      else
        throw IllegalStateException(s"Can not read field of none case class ${classDef.name}")
    case constrCall@ConstructorCall(name, tyArgs, args) =>
      val (classDef, constrDef) = constrCall.target match
        case Some(value) => value
        case _ => throw IllegalStateException(s"Unresolved constructor call $name")
      val argsCode = (s""""${classDef.name.name}"""" +: args.map(transExpression)).mkString(", ")
      if (classDef.isCaseClass)
        s"new SID$$${signatureString(constrDef.signature)}($argsCode)"
      else
        throw IllegalArgumentException(s"Can not construct none case class ${classDef.name}")
    case methodCall@MethodCall(recv, fun, tyArgs, args, isFix) =>
      val methodDef = methodCall.target match
        case Some((_, m)) => m
        case _ => throw IllegalStateException(s"Unresolved target for method call '$methodCall'")
      val qualifiedMethodName = s"${methodDef.name}$$${signatureString(methodDef.signature)}"
      val recvCode = transExpression(recv)
      val tyCode = if (tyArgs.nonEmpty) tyArgs.map(transType).mkString("[", ",", "]") else ""
      val argsCode = (recvCode +: args.map(transExpression)).mkString(", ")
      s"$qualifiedMethodName$tyCode($argsCode)"
    case TypeCast(recv, _) =>
      s"${transExpression(recv)}"
    case InstanceOf(recv, tname@TName(name, _)) if !tname.isBuiltIn =>
      s"""isInstanceOf$$(${transExpression(recv)}, "$name")"""
    case InstanceOf(recv, _) =>
      throw IllegalStateException("Unsupported isInstanceOf call!")
    case TupleExp(exps) =>
      exps.map(transExpression).mkString("(", ",", ")")
    case SetExp(exps, maybeType) =>
      val argsCode = exps.map(transExpression).mkString(", ")
      maybeType match
        case Some(ty) => s"Set[${transType(ty)}]($argsCode)"
        case _ => s"Set($argsCode)"
    case SetMember(name, recv, Some(predicate)) =>
      s"$name <- ${transExpression(recv)} if ${transExpression(predicate)}"
    case SetMember(name, recv, None) =>
      s"$name <- ${transExpression(recv)}"
    case SetComprehension(member, body) =>
      val memberCode = member.map(transExpression).mkString(";")
      val bodyCode = transExpression(body)
      s"for ($memberCode) yield $bodyCode"
    case NullLit() => """OID("Null", -1)"""
    case BoolLit(b) => s"$b"
    case IntLit(i) => s"$i"
    case DoubleLit(d) => s"$d"
    case StringLit(s) => s""""$s""""
    case BinOp(e1, op, e2) => s"${transExpression(e1)} $op ${transExpression(e2)}"
    case UnOp(op, e) => s"$op$e"

  private def transType(typ: Type): Code = typ match
    case TAny => "Any"
    case TNull => "ID"
    case TTuple(ts) => ts.map(transType).mkString("(", ",", ")")
    case t: TName if t.isBuiltIn => t.name.name
    case TName(name, tyArgs) => "ID"
    case TSet(ty) => s"Set[${transType(ty)}]"

  def genAggregation(name: String, init: Expression, op: Expression, typ: Type): Code = {
    val scalaTy = transType(typ)
    val funCode = op match
      case v: Var => v.target match
        case Some(f: FunctionDef) => transFunctionDef(f)
        case _ => throw IllegalStateException(s"Unresolved operator target $op")
      case _ => throw IllegalStateException(s"Unexpected operator $op")
    s"""
     |new inca.viatra.runtime.aggregate.JoinAggregation[$scalaTy] {
     |${funCode.indent(2)}
     |  override val name = "$name"
     |  override def init: $scalaTy = ${transExpression(init)}
     |  override def join(v1: $scalaTy, v2: $scalaTy): $scalaTy = ${transExpression(op)}(v1, v2)
     |  override val isAssociative = true
     |  override val isCommutative = true
     }""".stripMargin
  }