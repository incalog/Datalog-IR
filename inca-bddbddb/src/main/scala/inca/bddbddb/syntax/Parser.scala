package inca.bddbddb.syntax

import cats.parse.{Numbers, Parser as P, Parser0 as P0}
import inca.bddbddb.syntax.DirectiveQualifier.*
import inca.ir.util.SourceLocation
import inca.ir.{Name, RefByName}

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

  implicit class P0loc[T](p: => P0[T]) {
    def mapWithLoc[U <: SourceLocation](f: T => U): P0[U] =
      val pv = p
      (P.index ~ pv ~ P.index).map {
        case ((start, t), end) =>
          val u = f(t)
          u.startIndex = start
          u.endIndex = end
          u
      }
  }

  def parseModule(source: String): Module =
    (whitespaces0 *> module <* P.end).parseAll(source) match
      case Right(p) => p
      case Left(err) => throw new IllegalArgumentException(s"Parse error at ${source.slice(err.failedAtOffset, err.failedAtOffset+10)}: $err")

  /* LEXICAL */

  val lineComment: P[Unit] = P.char('#') *> P.charsWhile0(c => c != '\n' && c != '\r').void
  val comment: P[Unit] = lineComment
  // ignore escaped new lines
  val whitespace: P[Unit] = (P.stringIn(Seq(" ", "\t", "\\\n", "\\\r")).void | comment)
  val whitespaces0: P0[Unit] = whitespace.rep0.void
  val newLine: P[Unit] = P.charIn("\n\r").void

  def spaced[A](p: P[A]): P[A] =
    p <* whitespaces0

  val keywords: Set[String] = Set()

  def keyword(s: String): P[Unit] =
    if (!keywords.contains(s))
      throw new IllegalArgumentException(s"Not a keyword $s")
    else
      spaced(P.string(s) *> P.not(letterDigit))

  val letter: P[Unit] = (P.ignoreCaseCharIn('a' to 'z') | P.char('$')).void
  val digit: P[Unit] = P.charIn('0' to '9').void
  val letterDigit: P[Unit] = P.charIn(('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9') ++ Some('_')).void
  val opSymbol: P[Unit] = P.charIn("!@#%^&*()+=<>,.:?/\\_|").void

  val id: P[String] =
    (letter ~ letterDigit.rep0)
      .string
      .filter(s => !keywords.contains(s))

  val identifier: P[Name] =
    spaced(id).mapWithLoc(Name.apply)

  val qualifiedIdentifier: P[QualifiedName] =
    spaced(id ~ (P.char('.') *> id).rep0).mapWithLoc((a, bs) => QualifiedName(a :: bs))


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
    spaced(P.string(s))//*> P.not(letterDigit))

  def operator(s: String): P[Unit] =
    spaced(P.string(s) <* P.not(opSymbol))

  def oneOperator(ss: List[String]): P[String] =
    P.oneOf(ss.map(s => operator(s) *> P.pure(s)))

  val wildcard: P[Term] = op("_").mapWithLoc(_ => Term.Var(Name("_")))

  private val intNum: P[Int] =
    spaced(Numbers.signedIntString).map(_.toInt)

  private val intLit: P[Term] =
    intNum.mapWithLoc(s => Term.NumberLit(s))

  private val stringPrimitive: P[String] = spaced(
    P.char('"') *> P.charsWhile0(_ != '\"') <* P.char('"')
  )

  private val stringLit: P[Term] =
    stringPrimitive.mapWithLoc(s => Term.StringLit(s))

  val atomicTerm: P[Term] =
    intLit |
    stringLit |
    identifier.mapWithLoc(Term.Var.apply) |
    wildcard

  lazy val term: P[Term] = atomicTerm

  val call: P[Atom.Call] =
    (op("!").?.with1 ~ identifier ~ inParens(term.repSep0(op(',')))).mapWithLoc {
      case ((not, name), args) => Atom.Call(RefByName(name), args, not.isDefined)
    }

  val comparator: P[String] = oneOperator(List("=>", "!=", "=", ">", "<"))

  val compare: P[Atom.Compare] =
    (term ~ comparator ~ term).mapWithLoc {
      case ((lhs, op), rhs) => Atom.Compare(lhs, op, rhs)
    }

  val atom: P[Atom] = call.backtrack | compare

  val litDirective: P[ProgramContent.Directive] =
    (op('.') *> identifier ~ identifier).mapWithLoc {
      case (Name("bddvarorder"), Name(value)) => ProgramContent.Directive(BddVarOrder(value))
      case (Name("findbestorder"), Name(value)) => ProgramContent.Directive(FindBestOrder(value))
      case (Name("incremental"), Name(value)) => ProgramContent.Directive(Incremental(value))
    }

  val stringDirective: P[ProgramContent.Directive] =
    (op('.') *> identifier ~ stringPrimitive).mapWithLoc {
      case (Name("include"), value) => ProgramContent.Directive(Include(value))
      case (Name("basedir"), value) => ProgramContent.Directive(BaseDir(value))
    }

  private val boolValue: P[Boolean] =
    P.string("true").as(true) |
    P.string("yes").as(true) |
    P.string("false").as(false) |
    P.string("no").as(false)

  val boolDirective: P[ProgramContent.Directive] =
    (op('.') *> identifier ~ spaced(boolValue)).mapWithLoc {
      case (Name("splitallrules"), value) => ProgramContent.Directive(SplitAllRules(value))
      case (Name("reportstats"), value) => ProgramContent.Directive(ReportStats(value))
      case (Name("noisy"), value) => ProgramContent.Directive(Noisy(value))
      case (Name("strict"), value) => ProgramContent.Directive(Strict(value))
      case (Name("singleignore"), value) => ProgramContent.Directive(SingleIgnore(value))
      case (Name("trace"), value) => ProgramContent.Directive(Trace(value))
    }

  val intDirective: P[ProgramContent.Directive] =
    (op('.') *> identifier ~ spaced(Numbers.nonNegativeIntString)).mapWithLoc {
      case (Name("bddcache"), value) => ProgramContent.Directive(BddCache(value.toInt))
      case (Name("bddnodes"), value) => ProgramContent.Directive(BddNodes(value.toInt))
      case (Name("bddminfree"), value) => ProgramContent.Directive(BddmInFree(value.toInt))
    }

  val dotDirective: P[ProgramContent.Directive] =
    (op('.') *> identifier ~ P.charsWhile(c => c != '}')).mapWithLoc {
      case (Name("dot"), value) => ProgramContent.Directive(Dot(value))
    }

  val directive =
    stringDirective.backtrack |
    intDirective.backtrack |
    boolDirective.backtrack |
    litDirective.backtrack |
    dotDirective.backtrack

  val domain: P[Domain] = identifier.mapWithLoc(Domain.apply)

  val param: P[Param] = ((identifier <* op(':')) ~ domain).mapWithLoc {
    case (name, domain) => Param(name, domain)
  }

  val constraintRelationOption: P[RelationOption] =
    (identifier ~ oneOperator(List("=", "<", ">")) ~ identifier).backtrack.mapWithLoc {
      case ((lhs, op), rhs) => RelationOption.Constraint(lhs, op, rhs)
    }

  val customRelationOption: P[RelationOption] = (op('{') *> P.charsWhile(c => c != '}') <* op('}')).mapWithLoc {
    case body => RelationOption.Custom(body)
  }

  val simpleRelationOption: P[RelationOption] = P.oneOf(List(
    P.string("inputtuples").string,
    P.string("input").string,
    P.string("outputtuples").string,
    P.string("output").string,
    P.string("printtuples").backtrack.string,
    P.string("printsize").string,
  )).mapWithLoc {
    case "input" => RelationOption.Input
    case "inputtuples" => RelationOption.InputTuples
    case "output" => RelationOption.Output
    case "outputtuples" => RelationOption.OutputTuples
    case "printtuples" => RelationOption.PrintTuples
    case "printsize" => RelationOption.PrintSize
  }

  val relationOption = constraintRelationOption.backtrack | simpleRelationOption.backtrack | customRelationOption

  val relationDecl: P[ProgramContent.RelationDecl] =
    (identifier ~ inParens(param.repSep0(op(','))) ~ relationOption.repSep0(whitespaces0)).mapWithLoc {
      case ((name, params), options) => ProgramContent.RelationDecl(name, params, options)
    }

  val domainDecl: P[ProgramContent.DomainDecl] =
    (identifier ~ spaced(Numbers.nonNegativeIntString) ~ qualifiedIdentifier.?).mapWithLoc {
      case ((name, size), fileName) => ProgramContent.DomainDecl(name, size.toInt, fileName)
    }

  val simpleRuleOption: P[RuleOption] = P.oneOf(List(
    P.string("split").string,
    P.string("number").string,
    P.string("single").string,
    P.string("cacheafterrename").string,
    P.string("findbestorder").string,
    P.string("trace").string,
  )).mapWithLoc {
    case "split" => RuleOption.Split
    case "number" => RuleOption.Number
    case "single" => RuleOption.Single
    case "cacheafterrename" => RuleOption.CacheAfterRename
    case "findbestorder" => RuleOption.FindBestOrder
    case "trace" => RuleOption.Trace
  }

  val modifiesRuleOption: P[RuleOption] =
    spaced(P.string("modifies")) *> (
      spaced(inParens(identifier.repSep0(op(',')))).mapWithLoc(RuleOption.Modifies.apply) |
      spaced(identifier).mapWithLoc(n => RuleOption.Modifies(Seq(n)))
    )

  val priRuleOption: P[RuleOption] = (P.string("pri") ~ op('=') *> intNum).mapWithLoc {
    case value => RuleOption.Pri(value)
  }

  // TODO: This does not support all required features
  val complexRuleOption: P[RuleOption] = (identifier ~ identifier).mapWithLoc {
    case (Name("pre"), body) => RuleOption.Pre(body.name)
    case (Name("post"), body) => RuleOption.Post(body.name)
  }

  val ruleOption: P[RuleOption] = priRuleOption | modifiesRuleOption | simpleRuleOption | complexRuleOption

  val rule: P[ProgramContent.Rule] =
    (identifier
      ~ inParens(atomicTerm.repSep0(op(',')))
     ~ ((op(":-") *> atom.repSep0(op(','))).? <* op('.'))
      ~ ruleOption.repSep0(whitespaces0)
      ).mapWithLoc {
      case (((name, params), atoms), options) => ProgramContent.Rule(name, params, atoms.getOrElse(Seq()), options)
    }

  val moduleEntry: P[Option[ProgramContent]] =
    (directive.backtrack | relationDecl.backtrack | domainDecl.backtrack | rule).map(c => Some(c))

  val module: P0[Module] =
    (
      (whitespaces0.with1 <* newLine).map(_ => None) |
      (whitespaces0.with1 *> moduleEntry <* newLine).backtrack |
      (whitespaces0.with1 *> moduleEntry) | // File ends with a module entry
      whitespace.rep(1).map(_ => None) // File ends with a comment
    ).rep0.map { contentOptions =>
      Module(contentOptions.flatten)
    }
