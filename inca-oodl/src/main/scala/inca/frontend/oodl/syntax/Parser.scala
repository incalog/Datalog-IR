package inca.frontend.oodl.syntax

import cats.data.NonEmptyList
import cats.parse.{Numbers, Parser as P, Parser0 as P0}
import inca.ir.Name
import inca.ir.util.SourceLocation

import scala.language.implicitConversions

object Parser:

  implicit class Ploc[T](p: => P[T]) {
    def mapWithLoc[U <: SourceLocation](f: T => U): P[U] =
      val pv = p
      (P.index.with1 ~ pv ~ P.index).map {
        case ((start, t), end) =>
          val u = f(t)
          u.startIndex = start
          u.endIndex = end
          u
      }
  }

  def parseModule(source: String): Module =
    (whitespaces0() *> module <* P.end).parseAll(source) match
      case Right(p) => p
      case Left(err) => throw new IllegalArgumentException(s"Parse error at ${source.slice(err.failedAtOffset, err.failedAtOffset + 10)}: $err")

  /* LEXICAL */

  val lineComment: P[Unit] = P.string("//") *> P.charsWhile0(c => c != '\n' && c != '\r').void
  val blockComment: P[Unit] = P.string("/*") *> P.recursive[Unit](rec =>
    P.product01(P.charsWhile0(c => c != '*').void, P.string("*/") | P.char('*') ~ rec).void
  )
  val comment: P[Unit] = lineComment | blockComment
  val whitespace: P[Unit] = P.charIn(" \t\r\n").void | comment

  def whitespaces0(min: Int = 0): P0[Unit] = whitespace.rep0.void

  def spaced[A](p: P[A], min: Int = 0): P[A] =
    p <* whitespaces0(min)

  val keywords: Set[String] = Set(
    "module",
    "import",
    "private",
    "def",
    "if",
    "else",
    "match",
    "case",
    "true",
    "false",
    "class",
    "extends",
    "var",
    "val",
    "return",
    "new",
    "null",
    "for",
    "yield",
    "fix"
  )

  def keyword(s: String): P[Unit] =
    if (!keywords.contains(s))
      throw new IllegalArgumentException(s"Not a keyword $s")
    else
      spaced(P.string(s) *> P.not(letterDigit), 1).backtrack

  val letter: P[Unit] = P.charIn(('a' to 'z') ++ ('A' to 'Z') ++ Some('_')).void
  val digit: P[Unit] = P.charIn('0' to '9').void
  val letterDigit: P[Unit] = P.charIn(('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9') ++ Some('_')).void
  val opSymbol: P[Unit] = P.charIn("@#$%^&*()+=<>,.:?/\\_|").void

  val id: P[String] =
    (letter ~ letterDigit.rep0)
      .string
      .filter(s => !keywords.contains(s))

  val identifier: P[Name] =
    spaced(id).mapWithLoc(Name.apply)

  val qualifiedIdentifier: P[Name] =
    spaced(id ~ (P.char('.') *> id).rep0).mapWithLoc((a, bs) => Name((a :: bs).mkString(".")))

  def inParens[A](p: P0[A]): P[A] =
    op('(') *> p <* op(')')

  def inBraces[A](p: P0[A]): P[A] =
    op('{') *> p <* op('}')

  def inBrackets[A](p: P0[A]): P[A] =
    op('[') *> p <* op(']')

  def list0[A](p: P[A]): P0[List[A]] =
    p.repSep0(op(','))

  def list[A](p: P[A]): P[List[A]] =
    p.repSep(op(',')).map(_.toList)

  val semi: P[Unit] =
    op(';')

  def op(c: Char): P[Unit] =
    spaced(P.char(c))

  def op(s: String): P[Unit] =
    spaced(P.string(s) *> P.not(letterDigit))

  def operator(s: String): P[Unit] =
    spaced(P.string(s) <* P.not(opSymbol))

  def oneOperator(ss: List[String]): P[String] =
    P.oneOf(ss.map(s => operator(s) *> P.pure(s)))

  /** Auxiliary */

  val visibility: P0[Visibility] = keyword("private").mapWithLoc(_ => Private())

  val mainFuncAnno: P[MainFunctionAnno] = op("@main").mapWithLoc(_ => MainFunctionAnno())
  val overrideFuncAnno: P[OverrideFunctionAnno] = op("override").mapWithLoc(_ => OverrideFunctionAnno())

  val funcAnno: P[Annotation] = mainFuncAnno | overrideFuncAnno

  val caseClassAnno: P[CaseClassAnno] = op("case").mapWithLoc(_ => CaseClassAnno())

  //val annotation: P[Annotation] = mainFuncAnno

  /** Types */

  def simpleType[Ty <: Type](s: String, t: Ty): P[Ty] = op(s).map(_ => t)

  val recType: P[Type] = P.defer(typ)

  val tupleType: P[TTuple] = inParens(recType.repSep0(op(','))).mapWithLoc(TTuple.apply)

  val setType: P[TSet] = op("Set") *> inBrackets(recType).mapWithLoc(TSet.apply)

  def genericName: P[TName] =
    (qualifiedIdentifier ~ inBrackets(recType.repSep0(op(','))).?).mapWithLoc {
      case (name, tys) => TName(name, tys.getOrElse(Seq()))
    }

  val atomicType: P[Type] =
    //simpleType("Nothing",TNothing) |
    simpleType("Any", TAny) |
    simpleType("Null", TNull) |
    simpleType("Unit", TTuple(Seq())) |
    setType |
    tupleType |
    genericName

  lazy val typ: P[Type] = atomicType

  /** Expressions */

  private val recExpression: P[Expression] = P.defer(expression)
  private val recInfixExp: P[Expression] = P.defer(infixExp)

  lazy val setMemberExpr: P[SetMember] =
    (((identifier <* op("<-")) ~ recExpression) ~ (keyword("if") *> recExpression).?).mapWithLoc {
      case ((name, expr), pred) => SetMember(name, expr, pred)
    }

  lazy val setComprehensionExpr: P[SetComprehension] =
    ((keyword("for")
      *> inParens(setMemberExpr.repSep0(1, op(";")))
      <* keyword("yield")) ~ recExpression).mapWithLoc {
      case (memberExpr, expr) => SetComprehension(memberExpr, expr)
    }

  lazy val setExp: P[SetExp] =
    (op("Set") *> inBrackets(typ).? ~ inParens(recExpression.repSep0(op(',')))).mapWithLoc {
      case (maybeType, exprs) => SetExp(exprs, maybeType)
    }

  lazy val tupleExp: P[Expression] = inParens(recExpression.repSep0(op(','))).mapWithLoc {
    case e :: Nil => e
    case es => TupleExp(es)
  }

  val boolLit: P[BoolLit] = spaced(
    keyword("true").mapWithLoc(_ => BoolLit(true)) |
    keyword("false").mapWithLoc(_ => BoolLit(false))
  )

  val nullLit: P[NullLit] = spaced(
    keyword("null").mapWithLoc(_ => NullLit())
  )

  val intLit: P[IntLit] = spaced(
    Numbers.signedIntString.mapWithLoc(s => IntLit(s.toInt))
  )

  val doubleLit: P[DoubleLit] = spaced(
    (Numbers.signedIntString ~ (P.char('.') *> Numbers.nonNegativeIntString)).mapWithLoc {
      case (a, b) => DoubleLit(s"$a.$b".toDouble)
    })

  val stringLit: P[StringLit] = spaced(
    P.char('"') *> P.charsWhile0(_ != '\"') <* P.char('"')
  ).mapWithLoc(StringLit.apply)

  val unaryOperator: P[String] =
    val uOps = List('-', '!')
    P.oneOf(uOps.map(c => P.char(c).string))

  val unaryExp: P[Expression] =
    (unaryOperator ~ recInfixExp).mapWithLoc(UnOp.apply)

  /*val foldExp: P[SetFold] =
    (keyword("fold") *> inBrackets(typ).? ~
      inParens(recExpression.repSep0(op(',')))).flatMap {
      case (ty, init :: op :: set :: Nil) => P.pure(SetFold(ty, init, op, set))
      case (ty, args) => P.failWith(s"Wrong number of fold arguments, expected 3 but got ${args.size}: $args")
    }*/

  lazy val constructorExpr: P[ConstructorCall] =
    val tyArgs = inBrackets(typ.repSep0(op(','))).?
    val args = inParens(recExpression.repSep0(op(',')))
    (keyword("new") *> qualifiedIdentifier ~ tyArgs ~ args).mapWithLoc {
      case ((name, tyArgs), args) => ConstructorCall(name, tyArgs.getOrElse(Seq()), args)
    }

  lazy val varExpr: P[Var] =
    identifier.mapWithLoc(Var.apply)

  def selectExprStep(e: Expression, isFix: Boolean): P[Expression] =
    val tyArgs = inBrackets(typ.repSep0(op(','))).?
    val args = inParens(recExpression.repSep0(op(',')))
    val methodCall = (identifier ~ tyArgs ~ args).backtrack.mapWithLoc { case ((name, tyArgs), args) =>
      MethodCall(e, name, tyArgs.getOrElse(Seq()), args, isFix)
    }
    val asIsInstanceOfOrSelect = (identifier ~ tyArgs).backtrack.mapWithLoc {
      case (Name("asInstanceOf"), Some(Seq(ty: Type))) =>
        TypeCast(e, ty)
      case (Name("isInstanceOf"), Some(Seq(ty: Type))) =>
        InstanceOf(e, ty)
      case (name, None) =>
        Select(e, name)
      case _ =>
        throw IllegalStateException()
    }
    methodCall | asIsInstanceOfOrSelect

  def selectExprRec(e: Expression, isFix: Boolean): P0[Expression] =
    (P.char('.') *> selectExprStep(e, isFix)).flatMap(e => selectExprRec(e, isFix)) | P.pure(e)

  lazy val selectExpr: P[Expression] =
    (keyword("fix").?.with1 ~ atomicExp).flatMap {
      case (fix, e) => selectExprRec(e, fix.isDefined)
    }

  lazy val atomicExp: P[Expression] =
    //foldExp.backtrack |
    setExp |
    setComprehensionExpr |
    constructorExpr |
    tupleExp |
    nullLit |
    boolLit |
    stringLit |
    doubleLit.backtrack |
    intLit.backtrack |
    unaryExp |
    varExpr

  /*val pattern: P[Pattern] =
    (identifier ~ inParens(identifier.repSep0(op(',')))).mapWithLoc { case (name, args) => ConstructorPattern(name, args.map(PatternVariable.apply).toList) }

  lazy val matchCase: P[(Pattern,Expression)] =
    (keyword("case") *> pattern ~ op("=>") ~ recExpression).map { case ((p, _), e) => (p,e) }

  def matchExp(e: Expression): P[Match] =
    (keyword("match") *> inBraces(matchCase.rep)).mapWithLoc(cases => Match(e, cases.toList))*/

  val compareOperator: P[String] =
    oneOperator(List("==", ">=", "<=", "!=", "<", ">"))

  val additiveOperator: P[String] =
    (oneOperator(List("++", "+", "-")) <* P.not(P.string("="))).backtrack

  val multiplicativeOperator: P[String] =
    oneOperator(List("*", "/", "&", "%")).backtrack

  lazy val binMultiplicativeExpression: P[Expression] =
    (selectExpr ~ (multiplicativeOperator ~ P.defer(binMultiplicativeExpression)).?).mapWithLoc {
      case (lhs, Some(op, rhs)) => BinOp(lhs, op, rhs)
      case (lhs, None) => lhs
    }

  lazy val binAdditiveExpression: P[Expression] =
    (binMultiplicativeExpression ~ (additiveOperator ~ P.defer(binAdditiveExpression)).?).mapWithLoc {
      case (lhs, Some(op, rhs)) => BinOp(lhs, op, rhs)
      case (lhs, None) => lhs
    }

  lazy val binCompareExpression: P[Expression] =
    (binAdditiveExpression ~ (compareOperator ~ P.defer(binCompareExpression)).?).mapWithLoc {
      case (lhs, Some(op, rhs)) => BinOp(lhs, op, rhs)
      case (lhs, None) => lhs
    }

  lazy val binBoolAndExpression: P[Expression] =
    (binCompareExpression ~ (operator("&&") ~ P.defer(binBoolAndExpression)).?).mapWithLoc {
      case (lhs, Some(op, rhs)) => BinOp(lhs, "&&", rhs)
      case (lhs, None) => lhs
    }

  lazy val binBoolOrExpression: P[Expression] =
    (binBoolAndExpression ~ (operator("||") ~ P.defer(binBoolOrExpression)).?).mapWithLoc {
      case (lhs, Some(op, rhs)) => BinOp(lhs, "||", rhs)
      case (lhs, None) => lhs
    }

  val binOpExp: P[Expression] = binBoolOrExpression

  def infixExpRec(e: Expression): P0[Expression] = P.pure(e)
  //(infixExpStep(e) flatMap infixExpRec) | P.pure(e)

  //def infixExpStep(e: Expression): P[Expression] =
  //  matchExp(e)

  lazy val infixExp: P[Expression] =
    binOpExp flatMap infixExpRec

  lazy val expression: P[Expression] = infixExp


  /** Definitions */

  val typeParam: P[ParametricType] = identifier.mapWithLoc(ParametricType.apply)

  val typeParams: P0[Seq[ParametricType]] =
    inBrackets(typeParam.repSep0(op(","))) | P.pure(Seq())

  val param: P[Param] = (identifier ~ op(":") ~ typ).mapWithLoc {
    case ((name, _), typeAnno) => Param(name, typeAnno)
  }

  val params: P0[Seq[Param]] =
    inParens(param.repSep0(op(","))) | P.pure(Seq())

  private val function = (funcAnno.rep0 ~ visibility.?).with1 ~
                         keyword("def") ~ (identifier | P.string("+=").string.map(Name.apply)) ~ typeParams ~ params ~
                         op(":") ~ typ ~ op("=") ~ statements

  /** Statements */

  // TODO: We can remove this if we make the typechecker smarter
  private def insertMissingReturn(stmts: Seq[Statement]): Seq[Statement] =
    // Automatically insert return statements
    val unitStmt = Return(TupleExp(Seq()))
    val lastStmt = stmts.lastOption.getOrElse(unitStmt)
    val newTail = lastStmt match
      case Return(_) =>
        Seq(lastStmt)
      case Expr(expression) =>
        Seq(Return(expression))
      case If(_, thn, els) =>
        val allReturn = lastStmt.last.forall(_.isInstanceOf[Return])
        if (allReturn)
          Seq(lastStmt)
        else
          Seq(lastStmt, unitStmt)
      case _ =>
        Seq(lastStmt, unitStmt)
    stmts.dropRight(1) ++ newTail

  lazy val statements: P0[Seq[Statement]] =
    (
      expression.repSep0(1, 1, P.char(',')).map(_.map(Expr.apply)) |
      spaced(inBraces(statement.rep0(0)))
      ).map(insertMissingReturn)

  lazy val statement: P[Statement] =
    valDeclStmt |
    varDeclStmt |
    returnStmt |
    ifElseStmt |
    monoAddStmt.backtrack |
    assignStmt.backtrack |
    exprStmt

  lazy val returnStmt: P[Return] = (keyword("return") *> expression.?).map {
    case Some(expr) => Return(expr)
    case None => Return(TupleExp(Seq()))
  }

  lazy val exprStmt: P[Statement] = expression.mapWithLoc(Expr.apply)

  lazy val monoAddStmt: P[Assign] = ((expression <* op("+=")) ~ expression).mapWithLoc((mono, value) => Assign(mono, Name("+="), value))

  lazy val assignStmt: P[Assign] = ((expression <* op("=")) ~ expression).mapWithLoc((lhs, rhs) => Assign(lhs, Name("="), rhs))

  lazy val ifElseStmt: P[If] = {
    val ifBlock = keyword("if") *>
                  inParens(P.defer(expression)) ~
                  (inBraces(P.defer(statement).rep0) | P.defer(statement).map(Seq(_)))
    val elseBlock = keyword("else") *>
                    (inBraces(P.defer(statement).rep0) | P.defer(statement).map(Seq(_)))
    (ifBlock ~ elseBlock.?).mapWithLoc {
      case ((compareExpr, thnStmt), elseStmts) => If(compareExpr, thnStmt, elseStmts.getOrElse(Seq()))
    }
  }

  def baseDecl(immutable: Boolean): P[VarDeclare] =
    val kw = if (immutable) "val" else "var"
    (keyword(kw) *> identifier ~ (op(":") *> typ).? ~ (op("=") *> expression).?).mapWithLoc {
      case ((name, maybeTy), maybeExpr) => VarDeclare(name, maybeTy, maybeExpr, immutable)
    }

  lazy val varDeclStmt: P[VarDeclare] = baseDecl(false)

  lazy val valDeclStmt: P[VarDeclare] = baseDecl(true)

  /** ClassContent */

  val primaryConstructor: P0[Seq[FieldDef]] =
    val varDeclArg = (keyword("var") *> param).mapWithLoc {
      case Param(name, typ) => FieldDef(Seq(GeneratedConstructorFieldAnno()), None, name, typ, None, false)
    }
    val valDeclArg = (keyword("val") *> param).mapWithLoc {
      case Param(name, typ) => FieldDef(Seq(GeneratedConstructorFieldAnno()), None, name, typ, None, true)
    }
    val privateVarDeclArg = param.mapWithLoc {
      case Param(name, typ) => FieldDef(Seq(GeneratedConstructorFieldAnno()), Some(Private()), name, typ, None, true)
    }
    val decls = varDeclArg | valDeclArg | privateVarDeclArg
    inParens(decls.repSep0(op(","))) | P.pure(Seq())

  private def baseFieldDef(immutable: Boolean): P[FieldDef] = {
    val kw = if (immutable) "val" else "var"
    (((visibility.? <* keyword(kw)).with1 ~ param) ~ (op('=') *> expression).?).mapWithLoc {
      case ((visibility, Param(name, ty)), valueExpr) =>
        FieldDef(Seq(), visibility, name, ty, valueExpr, immutable)
    }
  }

  val fieldDef: P[FieldDef] =
    baseFieldDef(false).backtrack |
    baseFieldDef(true).backtrack

  val methodDef: P[MethodDef] =
    function.mapWithLoc {
      case (((((((((annos, vis), _), name), tyParams), params), _), ty), _), body) =>
        MethodDef(annos, vis, name, tyParams, params, ty, body)
    }

  val classContent: P0[Seq[ClassContent]] =
    spaced(inBraces((methodDef | fieldDef).rep0)) | P.pure(Seq[ClassContent]())

  /** Module content */

  val functionDef: P[FunctionDef] =
    function.mapWithLoc {
      case (((((((((annos, vis), _), name), tyParams), params), _), ty), _), body) =>
        FunctionDef(annos, vis, name, tyParams, params, ty, body)
    }

  val classDef: P[ClassDef] =
    ((visibility.? ~ caseClassAnno.?).with1 ~
     (keyword("class") *> identifier) ~ typeParams.? ~ primaryConstructor ~
     (keyword("extends") *> typ ~ inParens(expression.repSep0(op(','))).?).? ~ classContent).mapWithLoc {
      case ((((((vis, annos), name), tys), primaryConstrFields), maybeParentCls), clsContent) =>
        // Inherit from Object if no superclass is specified
        var isMono = false
        val (parentCls, superArgs) = maybeParentCls match
          case Some((cls@TName(Name("mono.Type"), _), _)) =>
            isMono = true
            (cls, Seq())
          case Some((cls, Some(args))) =>
            (cls, args)
          case Some((cls, None)) =>
            (cls, List())
          case _ =>
            (TName(Name("Object"), Seq()), List())

        // Generate a constructor + fields based on the header
        val constrParams = primaryConstrFields.map(f => Param(f.name, f.typ))
        val superCall = Super(superArgs)
        val fieldAssigns = primaryConstrFields.map(f => Assign(Select(Var("this"), f.name), Name("="), Var(f.name)))
        val constrDef = ConstructorDef(Seq(), None, constrParams, superCall +: fieldAssigns)

        val allContent = (primaryConstrFields :+ constrDef) ++ clsContent
        val annotations =
          if (isMono)
            Seq(MonoClassAnno())
          else
            annos match
              case Some(value) => Seq(value)
              case None => Seq()
        ClassDef(annotations, vis, name, tys.getOrElse(Seq()), Seq(parentCls), allContent)
    }

  val content: P[ModuleContent] =
    functionDef | classDef

  val impor: P[Import] =
    keyword("import") *> qualifiedIdentifier.mapWithLoc(Import.apply)

  val module: P[Module] =
    whitespaces0().with1 *>
    keyword("module") *> (qualifiedIdentifier ~ impor.rep0 ~ content.rep0)
      .mapWithLoc { case ((name, imports), contents) => Module(name, imports, contents) }
