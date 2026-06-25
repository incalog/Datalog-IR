package inca.codeql.compile

import inca.codeql.syntax.*
import _root_.inca.ir
import _root_.inca.ir.extension.{arithmetic, bool, string}
import _root_.inca.ir.{Arg, Language, Name}
import _root_.inca.util.Gensym

class GenerateIR(program: Program):
  private case class CompiledExpr(term: ir.Term, atoms: Seq[ir.Atom])
  private case class CompiledArgs(args: Seq[ir.Arg], atoms: Seq[ir.Atom])

  private val classTable = ClassTable(program)
  private val gensym = Gensym(
    program.predicates.map(_.name.name) ++ program.classes.flatMap(clazz => clazz.name.name +: clazz.members.map(_.name.name))
  )
  private val declarations = program.predicateMap
  private val irLanguage = Language(ir.BaseIR, arithmetic.IR, string.IR, bool.IR)

  def compileProgram(): ir.Module =
    val predicates = program.predicates.flatMap(compilePredicate)
    val classes = program.classes.map(compileClass)
    val memberImplementations = program.classes.flatMap(clazz => clazz.members.map(compileMemberImplementation(clazz, _)))
    val memberDispatch = program.classes.flatMap(compileMemberDispatch)
    val query = program.query.toSeq.map(compileQuery)
    ir.Module(Name("CodeQL"), irLanguage, predicates ++ classes ++ memberImplementations ++ memberDispatch ++ query)

  private def compilePredicate(predicate: PredicateDecl): Seq[ir.ModuleEntry] =
    if predicate.external then Seq(compileExternal(predicate))
    else Seq(compileDefined(predicate))

  private def compileExternal(predicate: PredicateDecl): ir.ExtensionalRelation =
    ir.ExtensionalRelation(predicate.name, compileParams(predicate.outputTypes, predicate.params.map(_.name) ++ resultName(predicate)))

  private def compileDefined(predicate: PredicateDecl): ir.Relation = gensym.scoped {
    val outputNames = predicate.params.map(_.name) ++ resultName(predicate)
    val outputParams = compileParams(predicate.outputTypes, outputNames)
    val headAtoms = bindOutputParams(outputParams, outputNames)
    val typeConstraints = compileVariableConstraints(predicate.params)
    val bodies = compileFormulaDnf(predicate.body.get, None).map { atoms =>
      ir.Body(orderAtoms(typeConstraints ++ atoms ++ headAtoms))
    }
    ir.Relation(predicate.name, outputParams, bodies)
  }

  private def compileClass(clazz: ClassDecl): ir.Relation = gensym.scoped {
    val fields = classTable.inheritedFields(clazz.name)
    val outputVariables = VariableDecl(QlType.EntityType(clazz.name), Name("this")) +: fields
    val outputParams = compileParams(outputVariables.map(_.ty), outputVariables.map(_.name))
    val headAtoms = bindOutputParams(outputParams, outputVariables.map(_.name))
    val baseAtoms = classTable.directBaseClasses(clazz).map { base =>
      classCall(base.name, ir.Var(Name("this")), bindInheritedFields = true, negated = false)
    }
    val instanceAtoms = clazz.instanceOf.flatMap {
      case QlType.EntityType(name) if classTable.classes.contains(name) =>
        Some(classCall(name, ir.Var(Name("this")), bindInheritedFields = false, negated = false))
      case _ => None
    }
    val fieldConstraints = compileVariableConstraints(fields)
    val characteristicBodies = clazz.characteristic
      .map(formula => compileFormulaDnf(formula, Some(clazz)))
      .getOrElse(Seq(Seq.empty))
    val bodies = characteristicBodies.map { characteristic =>
      ir.Body(orderAtoms(baseAtoms ++ instanceAtoms ++ fieldConstraints ++ characteristic ++ headAtoms))
    }
    ir.Relation(classRelationName(clazz.name), outputParams, bodies)
  }

  private def compileMemberImplementation(clazz: ClassDecl, member: MemberPredicateDecl): ir.Relation = gensym.scoped {
    val outputVariables =
      VariableDecl(QlType.EntityType(clazz.name), Name("this")) +:
        (member.params ++ member.resultType.toSeq.map(VariableDecl(_, Name("result"))))
    val outputParams = compileParams(outputVariables.map(_.ty), outputVariables.map(_.name))
    val headAtoms = bindOutputParams(outputParams, outputVariables.map(_.name))
    val membership = classCall(clazz.name, ir.Var(Name("this")), bindInheritedFields = true, negated = false)
    val parameterConstraints = compileVariableConstraints(member.params)
    val bodies = compileFormulaDnf(member.body, Some(clazz)).map { atoms =>
      ir.Body(orderAtoms(membership +: (parameterConstraints ++ atoms ++ headAtoms)))
    }
    ir.Relation(memberImplementationName(clazz.name, member.name), outputParams, bodies)
  }

  private def compileMemberDispatch(staticClass: ClassDecl): Seq[ir.Relation] =
    val memberNames = (classTable.ancestors(staticClass.name) ++ classTable.descendants(staticClass.name))
      .flatMap(_.members.map(_.name)).distinct
    memberNames.flatMap { memberName =>
      classTable.lookupMember(staticClass.name, memberName).map { resolved =>
        compileDispatchRelation(staticClass, memberName, resolved.declaration)
      }
    }

  private def compileDispatchRelation(
    staticClass: ClassDecl,
    memberName: Name,
    signature: MemberPredicateDecl
  ): ir.Relation = gensym.scoped {
    val outputVariables =
      VariableDecl(QlType.EntityType(staticClass.name), Name("this")) +:
        (signature.params ++ signature.resultType.toSeq.map(VariableDecl(_, Name("result"))))
    val outputParams = compileParams(outputVariables.map(_.ty), outputVariables.map(_.name))
    val variables = outputVariables.map(variable => ir.Var(variable.name).arg)
    val headAtoms = bindOutputParams(outputParams, outputVariables.map(_.name))
    val candidates = classTable.declaredMemberOwners(staticClass.name, memberName)
    val bodies = candidates.map { candidate =>
      val implementation = ir.Call(memberImplementationName(candidate.owner.name, memberName), variables)
      val moreSpecific = candidates.filter { other =>
        other.owner.name != candidate.owner.name && classTable.isSubtype(other.owner.name, candidate.owner.name)
      }.map { other =>
        classCall(other.owner.name, ir.Var(Name("this")), bindInheritedFields = false, negated = true)
      }
      ir.Body(orderAtoms(implementation +: (moreSpecific ++ headAtoms)))
    }
    ir.Relation(memberDispatchName(staticClass.name, memberName), outputParams, bodies)
  }

  private def compileQuery(query: SelectQuery): ir.Relation = gensym.scoped {
    val columnTypes = query.columns.map(column => column.expr.typ.get)
    val columnNames = query.columns.zipWithIndex.map { case (column, index) =>
      column.label.getOrElse(column.expr match
        case Expr.Var(name) => name
        case _ => Name(s"column_${index + 1}")
      )
    }
    val outputParams = compileParams(columnTypes, columnNames)
    val alternatives = query.where.map(formula => compileFormulaDnf(formula, None)).getOrElse(Seq(Seq.empty))
    val fromConstraints = compileVariableConstraints(query.from)
    val bodies = alternatives.map { atoms =>
      val compiledColumns = query.columns.map(column => compileExpr(column.expr, None))
      val expressionAtoms = compiledColumns.flatMap(_.atoms)
      val headAtoms = outputParams.zip(compiledColumns).map { case (param, column) =>
        ir.Eq(ir.Var(param.name), column.term)
      }
      ir.Body(orderAtoms(fromConstraints ++ atoms ++ expressionAtoms ++ headAtoms))
    }
    ir.Relation(CompiledCodeQlUnit.SelectRelationName, outputParams, bodies)
  }

  private def compileParams(types: Seq[QlType], preferredNames: Seq[Name]): Seq[ir.Param] =
    types.zip(preferredNames).map { case (ty, preferredName) =>
      ir.Param(Name(gensym.fresh(s"${preferredName.name}_param")), compileType(ty))
    }

  private def bindOutputParams(params: Seq[ir.Param], variables: Seq[Name]): Seq[ir.Atom] =
    params.zip(variables).map { case (param, variable) => ir.Eq(ir.Var(param.name), ir.Var(variable)) }

  private def resultName(predicate: PredicateDecl): Seq[Name] = predicate.resultType.toSeq.map(_ => Name("result"))

  private def compileVariableConstraints(variables: Seq[VariableDecl]): Seq[ir.Atom] =
    variables.flatMap { variable =>
      variable.ty match
        case QlType.EntityType(className) if classTable.classes.contains(className) =>
          Some(classCall(className, ir.Var(variable.name), bindInheritedFields = false, negated = false))
        case _ => None
    }

  private def compileFormulaDnf(
    formula: Formula,
    owner: Option[ClassDecl],
    negated: Boolean = false
  ): Seq[Seq[ir.Atom]] = formula match
    case Formula.And(parts) if !negated =>
      parts.foldLeft(Seq(Seq.empty[ir.Atom])) { (current, part) => cross(current, compileFormulaDnf(part, owner)) }
    case Formula.And(parts) => parts.flatMap(compileFormulaDnf(_, owner, negated = true))
    case Formula.Or(parts) if !negated => parts.flatMap(compileFormulaDnf(_, owner))
    case Formula.Or(parts) =>
      parts.foldLeft(Seq(Seq.empty[ir.Atom])) { (current, part) =>
        cross(current, compileFormulaDnf(part, owner, negated = true))
      }
    case Formula.Not(inner) => compileFormulaDnf(inner, owner, !negated)
    case Formula.Exists(vars, inner) =>
      val constraints = compileVariableConstraints(vars)
      compileFormulaDnf(inner, owner, negated).map(constraints ++ _)
    case Formula.Truth(value) => if value != negated then Seq(Seq.empty) else Seq.empty
    case Formula.InRange(value, lower, upper) =>
      if negated then throw IllegalArgumentException("Negated ranges are not supported by the CodeQL frontend")
      val from = integerConstant(lower)
      val to = integerConstant(upper)
      val values = if from <= to then from.to(to) else from.to(to, -1)
      values.map { current =>
        val compiled = compileExpr(value, owner)
        compiled.atoms :+ ir.Eq(compiled.term, arithmetic.IntNum(current))
      }
    case atomic => Seq(compileAtomicFormula(atomic, owner, negated))

  private def cross(lhs: Seq[Seq[ir.Atom]], rhs: Seq[Seq[ir.Atom]]): Seq[Seq[ir.Atom]] =
    for left <- lhs; right <- rhs yield left ++ right

  private def compileAtomicFormula(formula: Formula, owner: Option[ClassDecl], negated: Boolean): Seq[ir.Atom] = formula match
    case Formula.Call(name, args) if declarations.contains(name) =>
      val compiled = compileArgs(args, owner)
      compiled.atoms :+ compileTopLevelCall(name, compiled.args, negated)
    case Formula.Call(name, args) =>
      compileMemberFormula(Expr.Var(Name("this")), name, args, owner, negated)
    case Formula.MemberCall(receiver, name, args) =>
      compileMemberFormula(receiver, name, args, owner, negated)
    case Formula.Compare(lhs, op, rhs) =>
      val left = compileExpr(lhs, owner)
      val right = compileExpr(rhs, owner)
      left.atoms ++ right.atoms :+ compileComparison(left.term, op, right.term, negated)
    case _ => throw IllegalArgumentException(s"Unsupported atomic CodeQL formula: $formula")

  private def compileMemberFormula(
    receiver: Expr,
    name: Name,
    args: Seq[Expr],
    owner: Option[ClassDecl],
    negated: Boolean
  ): Seq[ir.Atom] =
    val compiledReceiver = compileExpr(receiver, owner)
    val compiledArgs = compileArgs(args, owner)
    val className = receiverClass(receiver, owner)
    compiledReceiver.atoms ++ compiledArgs.atoms :+
      ir.Call(memberDispatchName(className, name), compiledReceiver.term.arg +: compiledArgs.args, negated)

  private def compileComparison(lhs: ir.Term, op: String, rhs: ir.Term, negated: Boolean): ir.Atom =
    (op, negated) match
      case ("=", false) => ir.Eq(lhs, rhs)
      case ("=", true) | ("!=", false) => ir.Eq(lhs, rhs, neg = true)
      case ("!=", true) => ir.Eq(lhs, rhs)
      case (operator, false) => arithmetic.BinCompare(lhs, rhs, operator)
      case ("<", true) => arithmetic.BinCompare(lhs, rhs, ">=")
      case ("<=", true) => arithmetic.BinCompare(lhs, rhs, ">")
      case (">", true) => arithmetic.BinCompare(lhs, rhs, "<=")
      case (">=", true) => arithmetic.BinCompare(lhs, rhs, "<")
      case _ => throw IllegalArgumentException(s"Unsupported CodeQL comparison $op")

  private def compileArgs(args: Seq[Expr], owner: Option[ClassDecl]): CompiledArgs =
    args.foldLeft(CompiledArgs(Seq.empty, Seq.empty)) { (compiled, expr) =>
      expr match
        case Expr.Wildcard => compiled.copy(args = compiled.args :+ ir.WildcardArg())
        case _ =>
          val value = compileExpr(expr, owner)
          CompiledArgs(compiled.args :+ value.term.arg, compiled.atoms ++ value.atoms)
    }

  private def compileExpr(expr: Expr, owner: Option[ClassDecl]): CompiledExpr = expr match
    case Expr.Var(name) => CompiledExpr(ir.Var(name), Seq.empty)
    case Expr.Constant(value) => CompiledExpr(compileLiteral(value), Seq.empty)
    case Expr.Wildcard => throw IllegalArgumentException("A wildcard can only be used as a predicate argument")
    case Expr.Call(name, args) if declarations.contains(name) =>
      val declaration = declarations(name)
      if declaration.resultType.isEmpty then throw IllegalArgumentException(s"Predicate $name has no result")
      val compiled = compileArgs(args, owner)
      val result = ir.Var(Name(gensym.fresh(s"${name.name}_result")))
      val call = compileTopLevelCall(name, compiled.args :+ result.arg, negated = false)
      CompiledExpr(result, compiled.atoms :+ call)
    case Expr.Call(name, args) => compileMemberExpression(Expr.Var(Name("this")), name, args, owner)
    case Expr.MemberCall(receiver, name, args) =>
      compileBuiltInMemberExpression(receiver, name, args, owner).getOrElse {
        compileMemberExpression(receiver, name, args, owner)
      }
    case Expr.Binary(lhs, "+", rhs) if expr.typ.contains(QlType.StringType) =>
      combine(lhs, rhs, owner)(string.StringConcat.apply)
    case Expr.Binary(lhs, op, rhs) =>
      combine(lhs, rhs, owner)((left, right) => arithmetic.BinOp(left, right, op))
    case Expr.Unary("+", value) => compileExpr(value, owner)
    case Expr.Unary("-", value) =>
      val compiled = compileExpr(value, owner)
      CompiledExpr(arithmetic.UnOp(compiled.term, "-"), compiled.atoms)
    case Expr.Unary(op, _) => throw IllegalArgumentException(s"Unsupported unary CodeQL operator $op")

  private def compileMemberExpression(
    receiver: Expr,
    name: Name,
    args: Seq[Expr],
    owner: Option[ClassDecl]
  ): CompiledExpr =
    val compiledReceiver = compileExpr(receiver, owner)
    val compiledArgs = compileArgs(args, owner)
    val className = receiverClass(receiver, owner)
    val result = ir.Var(Name(gensym.fresh(s"${className.name}_${name.name}_result")))
    val call = ir.Call(
      memberDispatchName(className, name),
      compiledReceiver.term.arg +: compiledArgs.args :+ result.arg
    )
    CompiledExpr(result, compiledReceiver.atoms ++ compiledArgs.atoms :+ call)

  private def compileBuiltInMemberExpression(
    receiver: Expr,
    name: Name,
    args: Seq[Expr],
    owner: Option[ClassDecl]
  ): Option[CompiledExpr] =
    if receiver.typ.exists {
      case QlType.EntityType(className) => classTable.classes.contains(className)
      case _ => false
    } then None
    else name.name match
      case "toString" if args.isEmpty =>
        val compiled = compileExpr(receiver, owner)
        Some(CompiledExpr(string.ToString(compiled.term), compiled.atoms))
      case "length" if args.isEmpty =>
        val compiled = compileExpr(receiver, owner)
        Some(CompiledExpr(string.StringLength(compiled.term), compiled.atoms))
      case "pow" if args.size == 1 =>
        val compiled = compileExpr(receiver, owner)
        Some(CompiledExpr(pow(compiled.term, integerConstant(args.head)), compiled.atoms))
      case "minimum" if args.size == 1 =>
        Some(combine(receiver, args.head, owner)(arithmetic.Min.apply))
      case "maximum" if args.size == 1 =>
        Some(combine(receiver, args.head, owner)(arithmetic.Max.apply))
      case "booleanNot" if args.isEmpty =>
        val compiled = compileExpr(receiver, owner)
        Some(CompiledExpr(bool.BoolNot(compiled.term), compiled.atoms))
      case _ => None

  private def combine(
    lhs: Expr,
    rhs: Expr,
    owner: Option[ClassDecl]
  )(combineTerms: (ir.Term, ir.Term) => ir.Term): CompiledExpr =
    val left = compileExpr(lhs, owner)
    val right = compileExpr(rhs, owner)
    CompiledExpr(combineTerms(left.term, right.term), left.atoms ++ right.atoms)

  private def compileTopLevelCall(name: Name, args: Seq[Arg], negated: Boolean): ir.Atom =
    declarations.get(name) match
      case Some(declaration) if declaration.external => ir.ExtensionalCall(name, args, negated)
      case Some(_) => ir.Call(name, args, negated)
      case None => throw IllegalArgumentException(s"Unknown CodeQL predicate $name")

  private def receiverClass(receiver: Expr, owner: Option[ClassDecl]): Name =
    receiver.typ match
      case Some(QlType.EntityType(name)) if classTable.classes.contains(name) => name
      case _ if receiver == Expr.Var(Name("this")) => owner.map(_.name).getOrElse {
        throw IllegalArgumentException("this is only available in a class")
      }
      case _ => throw IllegalArgumentException(s"Member receiver has no class type: $receiver")

  private def classCall(className: Name, receiver: ir.Term, bindInheritedFields: Boolean, negated: Boolean): ir.Call =
    val fields = classTable.inheritedFields(className)
    val fieldArgs = fields.map { field =>
      if bindInheritedFields then ir.Var(field.name).arg else ir.WildcardArg()
    }
    ir.Call(classRelationName(className), receiver.arg +: fieldArgs, negated)

  private def compileLiteral(literal: Literal): ir.Term = literal match
    case Literal.IntValue(value) => arithmetic.IntNum(value)
    case Literal.FloatValue(value) => arithmetic.DoubleNum(value)
    case Literal.StringValue(value) => string.StringLit(value)
    case Literal.BooleanValue(true) => bool.BoolTrue
    case Literal.BooleanValue(false) => bool.BoolFalse

  private def compileType(ty: QlType): ir.Type = ty match
    case QlType.IntType => arithmetic.TInt
    case QlType.FloatType => arithmetic.TDouble
    case QlType.StringType | QlType.DateType => string.TString
    case QlType.BooleanType => bool.TBoolean
    case QlType.EntityType(name) if classTable.classes.contains(name) => compileType(classDomainType(name))
    case QlType.EntityType(_) => ir.TAny

  private def classDomainType(name: Name, seen: Set[Name] = Set.empty): QlType =
    if seen.contains(name) then QlType.EntityType(name)
    else
      val clazz = classTable.classes(name)
      clazz.bases.collectFirst {
        case primitive@(_: QlType.IntType.type | _: QlType.FloatType.type | _: QlType.StringType.type |
          _: QlType.BooleanType.type | _: QlType.DateType.type) => primitive
      }.orElse {
        clazz.bases.collectFirst {
          case QlType.EntityType(base) if classTable.classes.contains(base) => classDomainType(base, seen + name)
        }
      }.getOrElse(QlType.EntityType(name))

  private def integerConstant(expr: Expr): Int = expr match
    case Expr.Constant(Literal.IntValue(value)) => value
    case Expr.Unary("-", Expr.Constant(Literal.IntValue(value))) => -value
    case _ => throw IllegalArgumentException(s"Range bounds must be integer constants, got $expr")

  private def pow(base: ir.Term, exponent: Int): ir.Term =
    if exponent < 0 then throw IllegalArgumentException("CodeQL pow currently requires a non-negative integer exponent")
    else if exponent == 0 then arithmetic.DoubleNum(1.0)
    else
      val doubleBase = base match
        case arithmetic.IntNum(value) => arithmetic.DoubleNum(value.toDouble)
        case other => other
      (1 until exponent).foldLeft(doubleBase) { case (acc, _) => arithmetic.Mul(acc, doubleBase) }

  private def classRelationName(className: Name): Name = Name(s"__codeql_class_${className.name}")
  private def memberImplementationName(className: Name, memberName: Name): Name =
    Name(s"__codeql_member_impl_${className.name}_${memberName.name}")
  private def memberDispatchName(className: Name, memberName: Name): Name =
    Name(s"__codeql_member_${className.name}_${memberName.name}")

  private def orderAtoms(atoms: Seq[ir.Atom]): Seq[ir.Atom] = atoms.sortBy {
    case ir.ExtensionalCall(_, _, false) | ir.Call(_, _, false) => 0
    case ir.Eq(_: ir.Var, _: ir.Var, _) => 2
    case _: ir.Eq => 1
    case ir.ExtensionalCall(_, _, true) | ir.Call(_, _, true) => 4
    case _ => 3
  }
