package inca.codeql.typecheck

import inca.codeql.syntax.*
import _root_.inca.ir.Name
import _root_.inca.ir.typing.TypeIO
import _root_.inca.ir.util.SourceLocation

import scala.compiletime.uninitialized

class Typechecker extends TypeIO:
  private case class Signature(params: Seq[QlType], result: Option[QlType])

  private var signatures: Map[Name, Signature] = Map.empty
  private var variables: Map[Name, QlType] = Map.empty
  private var classTable: ClassTable = uninitialized
  private var currentClass: Option[ClassDecl] = None

  def checkProgram(program: Program): Unit =
    val duplicates = program.predicates.groupBy(_.name).collect { case (name, declarations) if declarations.size > 1 => name }
    duplicates.foreach(name => error(s"Predicate overloading is not supported yet: $name"))
    signatures = program.predicates.map(p => p.name -> Signature(p.params.map(_.ty), p.resultType)).toMap
    classTable = ClassTable(program)
    if classTable.hasInheritanceCycle then error("The CodeQL class hierarchy contains an inheritance cycle")
    program.classes.foreach(checkClass)
    program.predicates.foreach(checkPredicate)
    program.query.foreach(checkQuery)

  private def checkClass(clazz: ClassDecl): Unit =
    if clazz.isAbstract then error(s"Abstract class $clazz is not supported yet", clazz)
    val duplicateFields = classTable.inheritedFields(clazz.name).groupBy(_.name).collect {
      case (name, fields) if fields.size > 1 => name
    }
    duplicateFields.foreach(name => error(s"Duplicate inherited field $name in class ${clazz.name}", clazz))
    val duplicateMembers = clazz.members.groupBy(_.name).collect { case (name, members) if members.size > 1 => name }
    duplicateMembers.foreach(name => error(s"Member predicate overloading is not supported: ${clazz.name}.$name", clazz))
    currentClass = Some(clazz)
    variables = classVariables(clazz)
    clazz.characteristic.foreach(checkFormula)
    clazz.members.foreach(checkMember(clazz, _))
    currentClass = None

  private def checkMember(clazz: ClassDecl, member: MemberPredicateDecl): Unit =
    val inherited = classTable.directBaseClasses(clazz).flatMap(base => classTable.lookupMember(base.name, member.name)).headOption
    if member.isOverride && inherited.isEmpty then
      error(s"${clazz.name}.${member.name} is marked override but no inherited member exists", member)
    if !member.isOverride && inherited.nonEmpty then
      error(s"${clazz.name}.${member.name} overrides an inherited member and must be marked override", member)
    inherited.foreach { resolved =>
      val expected = Signature(resolved.declaration.params.map(_.ty), resolved.declaration.resultType)
      val actual = Signature(member.params.map(_.ty), member.resultType)
      if expected != actual then error(s"Override ${clazz.name}.${member.name} has incompatible signature", member)
    }
    variables = classVariables(clazz) ++ member.params.map(v => v.name -> v.ty)
    member.resultType.foreach(ty => variables += Name("result") -> ty)
    checkFormula(member.body)

  private def classVariables(clazz: ClassDecl): Map[Name, QlType] =
    classTable.inheritedFields(clazz.name).map(field => field.name -> field.ty).toMap +
      (Name("this") -> QlType.EntityType(clazz.name))

  private def checkPredicate(predicate: PredicateDecl): Unit =
    currentClass = None
    variables = predicate.params.map(v => v.name -> v.ty).toMap
    predicate.resultType.foreach(ty => variables += Name("result") -> ty)
    predicate.body.foreach(checkFormula)

  private def checkQuery(query: SelectQuery): Unit =
    currentClass = None
    variables = query.from.map(v => v.name -> v.ty).toMap
    query.where.foreach(checkFormula)
    query.columns.foreach(column => inferExpr(column.expr))

  private def checkFormula(formula: Formula): Unit = formula match
    case Formula.And(parts) => parts.foreach(checkFormula)
    case Formula.Or(parts) => parts.foreach(checkFormula)
    case Formula.Not(inner) => checkFormula(inner)
    case Formula.Call(name, args) => signatures.get(name) match
      case None => currentClass.flatMap(clazz => classTable.lookupMember(clazz.name, name)) match
        case Some(member) => checkMemberArguments(member, Expr.Var(Name("this")), args, expectResult = false, formula)
        case None =>
          error(s"Unknown predicate $name", formula)
          args.foreach(inferExpr)
      case Some(signature) =>
        if signature.result.nonEmpty then error(s"Result predicate $name must be used as an expression", formula)
        checkArguments(name, args, signature.params, formula)
    case Formula.MemberCall(receiver, name, args) =>
      resolveMember(receiver, name, args, expectResult = false, formula)
    case Formula.Compare(lhs, op, rhs) =>
      val lhsType = inferExpr(lhs)
      val rhsType = inferExpr(rhs)
      assertCompatible(lhsType, rhsType, formula)
      if Set("<", "<=", ">", ">=").contains(op) && !lhsType.exists(isOrdered) then
        error(s"Operator $op requires int or float operands", formula)
    case Formula.InRange(value, lower, upper) =>
      Seq(value, lower, upper).foreach(expr => assertCompatible(inferExpr(expr), Some(QlType.IntType), expr))
    case Formula.Exists(vars, inner) =>
      val previous = variables
      variables ++= vars.map(v => v.name -> v.ty)
      checkFormula(inner)
      variables = previous
    case Formula.Truth(_) => ()

  private def inferExpr(expr: Expr): Option[QlType] =
    val inferred = expr match
      case Expr.Var(name) => variables.get(name) match
        case result@Some(_) => result
        case None => error(s"Unknown variable $name", expr); None
      case Expr.Constant(value) => Some(value match
        case Literal.IntValue(_) => QlType.IntType
        case Literal.FloatValue(_) => QlType.FloatType
        case Literal.StringValue(_) => QlType.StringType
        case Literal.BooleanValue(_) => QlType.BooleanType
      )
      case Expr.Wildcard => None
      case Expr.Call(name, args) => signatures.get(name) match
        case None => currentClass.flatMap(clazz => classTable.lookupMember(clazz.name, name)) match
          case Some(member) => checkMemberArguments(member, Expr.Var(Name("this")), args, expectResult = true, expr)
          case None =>
            error(s"Unknown predicate $name", expr)
            args.foreach(inferExpr)
            None
        case Some(signature) =>
          checkArguments(name, args, signature.params, expr)
          signature.result match
            case result@Some(_) => result
            case None => error(s"Predicate $name has no result", expr); None
      case Expr.MemberCall(receiver, name, args) =>
        resolveBuiltInMember(receiver, name, args, expr).orElse {
          resolveMember(receiver, name, args, expectResult = true, expr)
        }
      case Expr.Binary(lhs, op, rhs) =>
        val lhsType = inferExpr(lhs)
        val rhsType = inferExpr(rhs)
        assertCompatible(lhsType, rhsType, expr)
        (op, lhsType) match
          case ("+", Some(QlType.StringType)) => lhsType
          case (_, Some(ty)) if Set("+", "-", "*", "/", "%").contains(op) && isNumeric(ty) => lhsType
          case _ => error(s"Operator $op is not available for ${lhsType.getOrElse("unknown")}", expr); None
      case Expr.Unary(op, value) =>
        val valueType = inferExpr(value)
        if !valueType.exists(isNumeric) then error(s"Unary operator $op requires int or float", expr)
        valueType
    inferred.foreach { ty =>
      expr.typ match
        case Some(existing) if existing != ty => error(s"Expression $expr has conflicting types $existing and $ty", expr)
        case Some(_) => ()
        case None => expr.typed(ty)
    }
    inferred

  private def checkArguments(name: Name, args: Seq[Expr], params: Seq[QlType], source: SourceLocation): Unit =
    if args.size != params.size then error(s"Predicate $name expects ${params.size} arguments but got ${args.size}", source)
    args.zip(params).foreach { case (arg, expected) =>
      arg match
        case Expr.Wildcard => ()
        case _ => assertCompatible(inferExpr(arg), Some(expected), arg)
    }

  private def resolveMember(
    receiver: Expr,
    name: Name,
    args: Seq[Expr],
    expectResult: Boolean,
    source: SourceLocation
  ): Option[QlType] = inferExpr(receiver) match
    case Some(QlType.EntityType(className)) if classTable.classes.contains(className) =>
      classTable.lookupMember(className, name) match
        case Some(member) => checkMemberArguments(member, receiver, args, expectResult, source)
        case None => error(s"Class $className has no member predicate $name", source); None
    case Some(ty) => error(s"Type $ty has no user-defined member predicate $name", source); None
    case None => None

  private def resolveBuiltInMember(
    receiver: Expr,
    name: Name,
    args: Seq[Expr],
    source: SourceLocation
  ): Option[QlType] =
    val receiverType = inferExpr(receiver)
    if receiverType.exists {
      case QlType.EntityType(className) => classTable.classes.contains(className)
      case _ => false
    } then return None
    def expectArity(size: Int): Boolean =
      if args.size != size then
        error(s"Built-in member $name expects $size arguments but got ${args.size}", source)
        false
      else true
    name.name match
      case "toString" if expectArity(0) =>
        Some(QlType.StringType)
      case "length" if expectArity(0) =>
        assertCompatible(receiverType, Some(QlType.StringType), receiver)
        Some(QlType.IntType)
      case "pow" if expectArity(1) =>
        if !receiverType.exists(isNumeric) then error("Built-in member pow requires an int or float receiver", receiver)
        assertCompatible(inferExpr(args.head), Some(QlType.IntType), args.head)
        Some(QlType.FloatType)
      case op@("minimum" | "maximum") if expectArity(1) =>
        val argumentType = inferExpr(args.head)
        if !receiverType.exists(isNumeric) || !argumentType.exists(isNumeric) then
          error(s"Built-in member $op requires int or float operands", source)
        if receiverType.contains(QlType.FloatType) || argumentType.contains(QlType.FloatType) then Some(QlType.FloatType)
        else Some(QlType.IntType)
      case "booleanNot" if expectArity(0) =>
        assertCompatible(receiverType, Some(QlType.BooleanType), receiver)
        Some(QlType.BooleanType)
      case _ => None

  private def checkMemberArguments(
    member: ResolvedMember,
    receiver: Expr,
    args: Seq[Expr],
    expectResult: Boolean,
    source: SourceLocation
  ): Option[QlType] =
    assertCompatible(inferExpr(receiver), Some(QlType.EntityType(member.owner.name)), receiver)
    checkArguments(member.declaration.name, args, member.declaration.params.map(_.ty), source)
    if expectResult then
      member.declaration.resultType match
        case result@Some(_) => result
        case None => error(s"Member predicate ${member.owner.name}.${member.declaration.name} has no result", source); None
    else
      if member.declaration.resultType.nonEmpty then
        error(s"Result member ${member.owner.name}.${member.declaration.name} must be used as an expression", source)
      None

  private def assertCompatible(lhs: Option[QlType], rhs: Option[QlType], source: SourceLocation): Unit =
    (lhs, rhs) match
      case (Some(a), Some(b)) if !compatible(a, b) => error(s"Incompatible CodeQL types $a and $b", source)
      case _ => ()

  private def compatible(lhs: QlType, rhs: QlType): Boolean =
    val normalizedLeft = underlyingType(lhs)
    val normalizedRight = underlyingType(rhs)
    normalizedLeft == normalizedRight || (isNumeric(lhs) && isNumeric(rhs)) || ((lhs, rhs) match
      case (QlType.EntityType(left), QlType.EntityType(right)) =>
        classTable != null && (classTable.isSubtype(left, right) || classTable.isSubtype(right, left))
      case _ => false
    )

  private def underlyingType(ty: QlType, seen: Set[Name] = Set.empty): QlType = ty match
    case QlType.EntityType(name) if classTable != null && classTable.classes.contains(name) && !seen.contains(name) =>
      val clazz = classTable.classes(name)
      clazz.bases.headOption.map(base => underlyingType(base, seen + name)).getOrElse(ty)
    case _ => ty

  private def isNumeric(ty: QlType): Boolean =
    val underlying = underlyingType(ty)
    underlying == QlType.IntType || underlying == QlType.FloatType
  private def isOrdered(ty: QlType): Boolean = isNumeric(ty)
