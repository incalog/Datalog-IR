package inca.ddlog.syntax

import inca.ddlog.syntax.Expr.Concat


enum Identifier:
  case UCIdentifier(qname: Seq[String]) // [A..Z][a..zA..Z0..9_]*
  case LCIdentifier(qname: Seq[String]) // [a..z_][a..zA..Z0..9_]*

  override def toString: String = this match
    case Identifier.UCIdentifier(qname) =>
      val prefix = qname.dropRight(1)
      var relName = qname.last.capitalize
      if (relName.startsWith("_"))
        relName = "U" + relName
      (prefix :+ relName).mkString("::")
    case Identifier.LCIdentifier(qname) =>
      val prefix = qname.dropRight(1)
      val relName = qname.last.toLowerCase
      (prefix :+ relName).mkString("::")

type ArgName = Identifier.LCIdentifier
type RelName = Identifier.UCIdentifier
type VarName = Identifier.LCIdentifier

object Identifier:
  def varn(s: String): VarName = Identifier.LCIdentifier(Seq(s))
  def rel(s: String): RelName = Identifier.UCIdentifier(Seq(s))
  def arg(s: String): ArgName = Identifier.LCIdentifier(Seq(s))

enum SimpleType:
  case Bigint
  case Bool
  case String
  case Double
  case Float
  case TypeAlias(name: String)
  case Ref(param : SimpleType)

  override def toString: String = this match
    case SimpleType.Bigint => "bigint"
    case SimpleType.Bool => "bool"
    case SimpleType.String => "string"
    case SimpleType.Double => "double"
    case SimpleType.Float => "float"
    case SimpleType.TypeAlias(name) => s"$name"
    case SimpleType.Ref(param) => s"Ref<$param>"


enum IOType:
  case Input
  case Output

  override def toString: String = this match
    case Input => "input"
    case Output => "output"

case class Arg(name: ArgName, ty: SimpleType):
  override def toString: String = s"$name: $ty"

enum Atom:
  case Call(rel: RelName, args: Seq[Expr])

  override def toString: String = this match
    case Atom.Call(rel, args) => s"$rel${args.mkString("(", ", ", ")")}"

enum Term:
  case Wildcard()
  case IntLit(i: Int)
  case DoubleLit(f: Double)
  case VarTerm(name: VarName)
  case VarDeclTerm(name: VarName)
  case StringLit(s: String)
  case NewRef(t: Term)
  case Constructor(name: String, fields: Seq[String], args: Seq[Expr])

  override def toString: String = this match
    case Wildcard() => "_"
    case IntLit(i) => i.toString
    case DoubleLit(f) => f.toString
    case VarTerm(name) => name.toString
    case VarDeclTerm(name: VarName) => s"var $name"
    case StringLit(s) => s""""$s""""
    case NewRef(t) => s"ref_new($t)"
    case Constructor(name, fields, args) =>
      s"$name${args.zip(fields).map { case (n, f) => s".$f=$n"}.mkString("{", ",", "}")}"

enum Expr:
  case T(term: Term)
  case Assign(lhs: Expr, rhs: Expr)
  case Eq(lhs: Expr, rhs: Expr)
  case Deref(t: Expr)

  //BinOp
  case Add(lhs:Expr, rhs:Expr)
  case Sub(lhs:Expr, rhs:Expr)
  case Mul(lhs: Expr, rhs: Expr)
  case Div(lhs:Expr, rhs:Expr)
  case Remainder(lhs:Expr, rhs:Expr)
  case Min(lhs:Expr, rhs:Expr)
  case Max(lhs:Expr, rhs:Expr)

  //UnOp
  case Neg(t:Expr)
  case Abs(t:Expr)

  //BinCompare
  case LT(lhs:Expr, rhs:Expr)
  case LE(lhs:Expr, rhs:Expr)
  case GT(lhs:Expr, rhs:Expr)
  case GE(lhs:Expr, rhs:Expr)

  //String
  case Concat(lhs:Expr, rhs:Expr)
  case ToString(value:Expr)

  //Base
  case Cast(t:Expr, ty:SimpleType)

  override def toString: String = this match
    case T(term) => term.toString
    case Assign(lhs, rhs) => s"$lhs = $rhs"
    case Eq(lhs, rhs) => s"$lhs == $rhs"
    case Deref(t) => s"deref($t)"

    //BinOp
    case Add(lhs, rhs) => s"$lhs + $rhs"
    case Sub(lhs, rhs) => s"$lhs - $rhs"
    case Mul(lhs, rhs) => s"$lhs * $rhs"
    case Div(lhs, rhs) => s"$lhs / $rhs"
    case Remainder(lhs, rhs) => s"$lhs % $rhs"
    case Min(lhs, rhs) => s"min($lhs,$rhs)"
    case Max(lhs, rhs) => s"max($lhs, $rhs)"

    //Unop
    case Neg(t) => s"(-($t))"
    case Abs(t) => s"(if ($t > 0) ($t) else (-($t)))"

    //BinCompare
    case LT(lhs, rhs) => s"$lhs < $rhs"
    case LE(lhs, rhs) => s"$lhs <= $rhs"
    case GT(lhs, rhs) => s"$lhs > $rhs"
    case GE(lhs, rhs) => s"$lhs >= $rhs"
    
    //String
    case Concat(lhs, rhs) => s"$lhs ++ $rhs"
    case ToString(value) => s"to_string($value)"

    //Base
    case Cast(t, ty) => s"($t) as $ty"

enum RhsClause:
  case At(at: Atom)
  case Not(at: Atom)
  case Ex(expr: Expr)
  case Eq(lhs: Expr, rhs: Expr) extends RhsClause
  case NotEq(lhs:Expr, rhs:Expr)
  case Deconstruct(t: Expr, caseName : String, fields: Seq[String], args : Seq[Expr], derefs: Seq[RhsClause], neg:Boolean)
  case GroupBy(aggArg: Expr, aggColArg: Expr, groupByArgs: Seq[Expr], opname: String)

  override def toString: String = this match
    case RhsClause.At(at) => at.toString
    case RhsClause.Not(at) => s"not $at"
    case RhsClause.Ex(expr) => expr.toString
    case RhsClause.Eq(lhs, rhs) => s"$lhs == $rhs"
    case RhsClause.NotEq(lhs, rhs) => s"$lhs != $rhs"
    case Deconstruct(t, casename, fields, args, derefs, neg) =>
      val op = if (neg) "!=" else "="
      val deconstr = s"$casename${args.zip(fields).map {case (n, f) => s".$f=$n"}.mkString("{", ", ", "}") } $op $t"
      val derefS = derefs.map(_.toString).mkString(", ")
      if derefS.isEmpty then
        s"$deconstr"
        else
        s"$deconstr, $derefS"

    case RhsClause.GroupBy(aggArg, aggColArg, groupByArgs, opname) =>
      val groupByTuple = groupByArgs.size match
        case 0 => "()"
        case 1 => groupByArgs.head.toString
        case _ => groupByArgs.mkString("(", ", ", ")")
      opname match
        case "sum_of" => s"var $aggArg = $aggColArg.group_by($groupByTuple).sum_of(|x| x)" // TODO: not working...
        case "count_distinct" => s"var $aggArg = $aggColArg.group_by($groupByTuple).count_distinct() as bigint"
        case _ => s"var $aggArg = $aggColArg.group_by($groupByTuple).$opname()"

enum Declaration:
  case Relation(iotype: Option[IOType], name: RelName, args: Seq[Arg])
  case Rule(head: Seq[Atom], rhs: Seq[RhsClause])
  case Datatype(typename: String, cases: Seq[(String, Seq[(String, SimpleType)])])
  
  override def toString: String = this match
    case Declaration.Relation(iotype, name, args) =>
      val ioPrefix = iotype match
        case Some(value) => s"$value "
        case _ => ""
      val argS = args.mkString("(", ", ", ")")
      s"${ioPrefix}relation $name$argS"
    case Declaration.Rule(head, body) =>
      val headS = head.mkString(", ")
      val bodyS = body.mkString(", ")
      s"$headS :- $bodyS."

    case Declaration.Datatype(typename, cases) =>
      //für jeden Case des Typs den string: CaseName{type1, type2, ...} erstellen
      val caseS = cases.map {
        case (caseName, params) =>
          val fields = params.map {
            case (fieldName, fieldType) => s"$fieldName: $fieldType"
          }
          s"$caseName${fields.mkString("{", ",", "}")}"
      }.mkString(" | ")
      s"typedef $typename = $caseS"

case class Program(decls: Seq[Declaration], additionalContent: String = ""):
  override def toString: String = "import group\nimport set\n\n" + decls.mkString("\n") + "\n" + additionalContent