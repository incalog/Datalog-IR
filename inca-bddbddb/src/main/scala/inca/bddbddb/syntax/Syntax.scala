package inca.bddbddb.syntax

import inca.ir.{Name, Ref}
import inca.ir.typing.{Resolvable, Typeable}
import inca.ir.util.SourceLocation

// See: https://sourceforge.net/p/bddbddb/code/HEAD/tree/trunk/bddbddb/net/sf/bddbddb/DatalogParser.java

case class Program(content: Seq[ProgramContent]) extends SourceLocation:
  override def toString: String = content.mkString("\n")

case class QualifiedName(ns: Seq[String])  extends SourceLocation:
  override def toString: String = ns.mkString(".")

enum DirectiveQualifier extends SourceLocation:
  case Include(filename: String)
  case BaseDir(path: String)

  case BddVarOrder(order: String)
  case BddCache(cacheSize: Int)
  case BddNodes(amount: Int)
  case BddmInFree(num: Int)

  case SplitAllRules(flag: Boolean)
  case ReportStats(flag: Boolean)
  case Noisy(flag: Boolean)
  case Strict(flag: Boolean)
  case SingleIgnore(flag: Boolean)
  case Trace(flag: Boolean)

  case FindBestOrder(order: String)
  case Incremental(value: String)
  case Dot(graph: String) // Used for dat graph annotations

  override def toString: String = this match
    case DirectiveQualifier.Include(filename) => s"include $filename"
    case DirectiveQualifier.BaseDir(path) => s"basedir $path"
    case DirectiveQualifier.BddVarOrder(order) => s"bddvarorder $order"
    case DirectiveQualifier.BddCache(cacheSize) => s"bddcache $cacheSize"
    case DirectiveQualifier.BddNodes(amount) => s"bddnodes $amount"
    case DirectiveQualifier.BddmInFree(num) => s"bddminfree $num"
    case DirectiveQualifier.SplitAllRules(flag) => s"splitallrules $flag"
    case DirectiveQualifier.ReportStats(flag) => s"reportstats $flag"
    case DirectiveQualifier.Noisy(flag) => s"noisy $flag"
    case DirectiveQualifier.Strict(flag) => s"strict $flag"
    case DirectiveQualifier.SingleIgnore(flag) => s"singleignore $flag"
    case DirectiveQualifier.Trace(flag) => s"trace $flag"
    case DirectiveQualifier.FindBestOrder(order) => s"findbestorder $order"
    case DirectiveQualifier.Incremental(value) => s"incremental $value"
    case DirectiveQualifier.Dot(graph) => s"dot $graph"

enum RelationOption extends SourceLocation:
  case Output
  case OutputTuples
  case Input
  case InputTuples
  case PrintTuples
  case PrintSize
  case Constraint(lhs: Name, op: String, rhs: Name)
  case Custom(body: String)

  override def toString: String = this match
    case RelationOption.Output => "output"
    case RelationOption.OutputTuples => "outputtuples"
    case RelationOption.Input => "input"
    case RelationOption.InputTuples => "inputtuples"
    case RelationOption.PrintTuples => "printtuples"
    case RelationOption.PrintSize => "printsize"
    case RelationOption.Constraint(lhs, op, rhs) => s"$lhs$op$rhs"
    case RelationOption.Custom(body) => s"{ $body }"

enum RuleOption extends SourceLocation:
  case Split
  case Number
  case Single
  case CacheAfterRename
  case FindBestOrder
  case Trace
  case TraceFull
  case Pri(value: Int) // pri=Num
  case Pre(body: String)
  case Post(body: String)
  case Modifies(ns: Seq[Name])

  override def toString: String = this match
    case RuleOption.Split => "split"
    case RuleOption.Number => "number"
    case RuleOption.Single => "single"
    case RuleOption.CacheAfterRename => "cacheafterrename"
    case RuleOption.FindBestOrder => "findbestorder"
    case RuleOption.Trace => "trace"
    case RuleOption.TraceFull => "tracefull"
    case RuleOption.Pri(value) => s"pri=$value"
    case RuleOption.Pre(body) => s"pre $body"
    case RuleOption.Post(body) => s"post $body"
    case RuleOption.Modifies(ns) => s"modifies ${ns.mkString(", ")}"

enum ProgramContent extends SourceLocation:
  case Directive(dirQualifier: DirectiveQualifier)
  case DomainDecl(name: Name, size: Long, qualifiedFileName: Option[QualifiedName])
  case RelationDecl(name: Name, attrs: Seq[Attribute], options: Seq[RelationOption])
  case Rule(name: Name, params: Seq[Term], body: Seq[Atom], options: Seq[RuleOption])

  override def toString: String = this match
    case ProgramContent.Directive(dirQualifier) => s".$dirQualifier"
    case ProgramContent.DomainDecl(name, size, Some(qualifiedFileName)) => s"$name $size $qualifiedFileName"
    case ProgramContent.DomainDecl(name, size, None) => s"$name $size"
    case ProgramContent.RelationDecl(name, params, options) => s"""$name ${params.mkString("(", ", ", ")")} ${options.mkString(" ")}"""
    case ProgramContent.Rule(name, params, body, options) => s"""$name(${params.mkString(",")}) :- ${body.mkString(", ")}. ${options.mkString(" ")}"""

case class Domain(name: Name) extends SourceLocation:
  override def toString: String = name.toString
  lazy val canonicalName: Name = Name(name.toString.replaceAll("\\d+$", ""))

case class Attribute(name: Name, domain: Domain) extends SourceLocation  with Resolvable[Domain]:
  this.target = Some(this.domain)

  override def toString: String = s"$name: $domain"

enum Atom extends SourceLocation:
  case Call(rel: Name, args: Seq[Term], neg: Boolean)
  case Compare(lhs: Term, op: String, rhs: Term)

  override def toString: String = this match
    case Call(name, args, not) =>
      val prefix = if not then "!" else ""
      s"$prefix$name(${args.mkString(",")})"
    case Compare(lhs, op, rhs) => s"$lhs $op $rhs"

enum Term extends SourceLocation with Resolvable[Domain]:
  case Var(name: Name)
  case NumberLit(value: Int)
  case StringLit(value: String)

  private def domainSuffix: String = target.map(d => s": $d").getOrElse("")

  override def toString: String = this match
    case Var(name) => name.toString + domainSuffix
    case NumberLit(value) => value.toString + domainSuffix
    case StringLit(value) => s""""$value"""" + domainSuffix
