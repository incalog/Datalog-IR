package inca.souffle.syntax

case class Program(content: Seq[ProgramContent]):
  override def toString: String = content.mkString("\n")

enum ProgramContent:
  case TypeDecl(name: String, rhs: TypeDeclConstraint)
  case RelationDecl(names: Seq[String], attrs: Seq[Attribute], qualifiers: Seq[Qualifier], choiceDomain: Option[ChoiceDomain])
  case Rule(heads: Seq[Atom], body: Seq[Atom], queryPlan: Option[QueryPlan])
  case Fact(name: String, args: Seq[Term])
  case Directive(dirQualifier: DirectiveQualifier, name: QualifiedName, attrs: Map[String, DirectiveValue])
  case ComponentDecl(ty: ComponentType, superTys: Seq[ComponentType], content: Seq[ProgramContent])
  case ComponentInit(n: String, compType: ComponentType)
  // can only be within component decl
  case Override(n: String)
  // cannot be within component decl
  case FunctorDecl(name: String, params: Seq[Attribute], retType: Type, stateful: Boolean)
  case Pragma(option: String, arg: Option[String])

  override def toString: String = this match
    case Rule(heads, body, queryPlan) =>
      val queryPlanStr = queryPlan match
        case Some(queryPlan) => queryPlan.toString
        case None => ""
      s"${heads.mkString(", ")} :- ${body.mkString(", ")}.$queryPlanStr"
    case Fact(name, args) => s"$name(${args.mkString(", ")})."
    case RelationDecl(names, attrs, qualifiers, choiceDomain) =>
      val qualifiersStr =
        if (qualifiers.isEmpty) ""
        else s" ${qualifiers.mkString(" ")}"
      val choiceDomainStr = choiceDomain match
        case Some(choiceDomain) => s" $choiceDomain"
        case None => ""
      s".decl ${names.mkString(", ")}(${attrs.mkString(", ")})$qualifiersStr$choiceDomainStr"
    case TypeDecl(name, rhs) => s".type $name $rhs"
    case Directive(dirQual, name, attrs) =>
      val attrsStr =
        if(attrs.isEmpty) ""
        else "(" + attrs.map{ case (k, v) => s"$k = $v" }.mkString(", ") + ")"
      s"$dirQual $name$attrsStr"
    case ComponentDecl(ty, superTys, contents) =>
      val superTysStr =
        if (superTys.isEmpty) ""
        else s": ${superTys.mkString(", ")}"
      s""".comp $ty$superTys {
         |${contents.mkString("\n")}
         |}
         |""".stripMargin
    case ComponentInit(n, componentType) =>
      s".init $n = $componentType"
    case Override(n) => ".override $n"
    case FunctorDecl(name, attrs, retty, stateful) =>
      val statefulStr = if (stateful) " stateful" else ""
      s".functor $name(${attrs.mkString(", ")}): $retty$statefulStr"
    case Pragma(option, arg) =>
      val argStr = arg match
        case Some(arg) => s" $arg"
        case None => ""
      s".pragma $option$argStr"


enum TypeDeclConstraint:
  case SubType(ty: Type)
  case UnionType(alts: Seq[Type])
  case RecordType(alts: Record)
  case ADTType(alts: Seq[ADTConstructor])

  override def toString: String = this match
    case SubType(ty) => s"<: $ty"
    case UnionType(alts) => s"= ${alts.mkString(" | ")}"
    case RecordType(rec) => s"= $rec"
    case ADTType(alts) => s"= ${alts.mkString(" | ")}"

enum Type:
  case Number
  case Symbol
  case Unsigned
  case Float
  case Name(qualName: QualifiedName)

  override def toString: String = this match
    case Number => "number"
    case Symbol => "symbol"
    case Unsigned => "unsigned"
    case Float => "float"
    case Name(qualName) => qualName.toString

case class QualifiedName(ns: Seq[String]):
  override def toString: String = ns.mkString(".")
case class Record(attrs: Seq[Attribute]):
  override def toString: String = s"[${attrs.mkString(", ")}]"
case class ADTConstructor(name: String, attrs: Seq[Attribute]):
  override def toString: String = s"$name { ${attrs.mkString(", ")} }"

case class Attribute(name: String, ty: Type):
  override def toString: String = s"$name: $ty"

case class Conjunction(atoms: Seq[Atom])

enum Atom:
  case Not(atom: Atom)
  case Call(qualifiedName: QualifiedName, args: Seq[Term])
  case Disjunction(bodys: Seq[Conjunction])
  case LessThan(t1: Term, t2: Term)
  case LessThanEqual(t1: Term, t2: Term)
  case GreaterThan(t1: Term, t2: Term)
  case GreaterThanEqual(t1: Term, t2: Term)
  case Equal(t1: Term, t2: Term)
  case Unequal(t1: Term, t2: Term)
  case Match(t1: Term, t2: Term)
  case Contains(t1: Term, t2: Term)
  case True
  case False

  override def toString: String = this match
    case Not(atom: Atom) => s"!$atom"
    case Call(qualName, args) => s"$qualName(${args.mkString(", ")})"
    case Disjunction(bodys) => ""
    case LessThan(t1, t2) => s"$t1 < $t2"
    case LessThanEqual(t1, t2) => s"$t1 <= $t2"
    case GreaterThan(t1, t2) => s"$t1 > $t2"
    case GreaterThanEqual(t1, t2) => s"$t1 >= $t2"
    case Equal(t1, t2) => s"$t1 = $t2"
    case Unequal(t1, t2) => s"$t1 != $t2"
    case Match(t1, t2) => s"match($t1, $t2)"
    case Contains(t1, t2) => s"contains($t1, $t2)"
    case True => "true"
    case False => "false"


enum BinOp:
  case Add
  case Sub
  case Mul
  case Div
  case Rem
  case Pow
  case Land
  case Lor
  case Lxor
  case Band
  case Bor
  case Bxor
  case Bshl
  case Bshr
  case Bshru

  override def toString: String = this match
    case Add => "+"
    case Sub => "-"
    case Mul => "*"
    case Div => "/"
    case Rem => "%"
    case Pow => "^"
    case Land => "land"
    case Lor => "lor"
    case Lxor => "lxor"
    case Band => "band"
    case Bor => "bor"
    case Bxor => "bxor"
    case Bshl => "bshl"
    case Bshr => "bshr"
    case Bshru => "bshru"

enum UnOp:
  case Neg
  case Bnot
  case Lnot

  override def toString: String = this match
    case Neg => "-"
    case Bnot => "bnot"
    case Lnot => "lnot"


enum Term:
  case Var(name: String)
  case StringLit(s: String)
  case NumberLit(n: Int)
  case UnsignedLit(n: Long)
  case FloatLit(f: Float)
  case Nil
  case List(s: Seq[Term])
  case Constr(name: String, args: Seq[Term])
  case Parens(t: Term)
  case TypeCast(t: Term, ty: Type)
  case AggregatorTerm(agg: Aggregator)
  case IntrinsicFunctorApp(f: IntrinsicFunctor, args: Seq[Term])
  case UserDefFunctorApp(f: UserDefFunctor, args: Seq[Term])
  case Unary(op: UnOp, t: Term)
  case Binary(t1: Term, op: BinOp, t2: Term)

  override def toString: String = this match
    case Var(name) => name
    case StringLit(s) => "\"" + s + "\""
    case NumberLit(n) => n.toString
    case UnsignedLit(n) => n.toString
    case FloatLit(f) => f.toString
    case Nil => "nil"
    case List(s) => s"[${s.mkString(", ")}]"
    case Constr(name, args) =>
      val argList = s"(${args.mkString(", ")})"
      s"$$$name$argList"
    case Parens(t) => s"(t)"
    case TypeCast(t, ty) => s"as($t, $ty)"
    case AggregatorTerm(agg) => agg.toString
    case IntrinsicFunctorApp(f, args) => s"$f(${args.mkString(", ")})"
    case UserDefFunctorApp(f, args) => s"$f(${args.mkString(", ")})"
    case Unary(op, t) => s"$op$t"
    case Binary(t1, op, t2) => s"($t1 $op $t2)"

case class UserDefFunctor(name: String)
enum IntrinsicFunctor:
  case Ord
  case ToFloat
  case ToNumber
  case ToString
  case ToUnsigned
  case Cat
  case StrLen
  case Substr
  case Max
  case Min

  override def toString: String = this match
    case Ord => "ord"
    case ToFloat => "to_float"
    case ToNumber => "to_number"
    case ToString => "to_string"
    case ToUnsigned => "to_unsigned"
    case Cat => "cat"
    case StrLen => "strlen"
    case Substr => "substr"
    case Max => "max"
    case Min => "min"

enum Aggregator:
  case Max(t: Term, atom: Atom)
  case Mean(t: Term, atom: Atom)
  case Min(t: Term, atom: Atom)
  case Sum(t: Term, atom: Atom)
  case Count(atom: Atom)
  case Range(begin: Term, end: Term, step: Option[Term])

  override def toString: String = this match
    case Max(t, a) => s"max $t : ${atomToString(a)}"
    case Mean(t, a) => s"mean $t : ${atomToString(a)}"
    case Min(t, a) => s"min $t : ${atomToString(a)}"
    case Sum(t, a) => s"sum $t : ${atomToString(a)}"
    case Count(a) => s"count : ${atomToString(a)}"
    case Range(begin, end, step) => s"range(${(Seq(begin, end) :+ step).mkString(", ")}"
  private def atomToString(a: Atom): String = a match
    case Atom.Disjunction(bodys) => s"{ ${bodys.mkString("; ")}"
    case Atom.Call(name, args) => a.toString


enum Qualifier:
  case EqRel
  case BTree
  case Brie
  case NoMagic
  case Magic
  case NoInline
  case Inline
  case Override

  override def toString: String = this match
    case EqRel => "eqrel"
    case BTree => "btree"
    case Brie => "brie"
    case NoMagic => "no_magic"
    case Magic => "magic"
    case NoInline => "no_inline"
    case Inline => "inline"
    case Override => "override"

// TODO functional dependencies
case class ChoiceDomain():
  override def toString: String = ""

// TODO query plan
case class QueryPlan():
  override def toString: String = s".plan "

enum DirectiveQualifier:
  case Input
  case Output
  case Printsize
  case Limitsize

  override def toString: String = this match
    case Input => ".input"
    case Output => ".output"
    case Printsize => ".printsize"
    case Limitsize => ".limitsize"

enum DirectiveValue:
  case StringLit(s: String)
  case Id(n: String)
  case Number(i: Int)
  case True
  case False

  override def toString: String = this match
    case StringLit(s) => "\"" + s + "\""
    case Id(n) => n
    case Number(i) => i.toString
    case True => "true"
    case False => "false"


case class ComponentType(n: String, argTypes: Seq[String]):
  override def toString: String =
    val argTypesStr =
      if (argTypes.isEmpty) ""
      else s"<${argTypes.mkString(", ")}>"
    s"$n$argTypesStr"