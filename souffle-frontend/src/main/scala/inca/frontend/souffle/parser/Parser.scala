package inca.frontend.souffle.parser

import inca.compiler.source.{Source, SourceLocation}
import inca.frontend.souffle.Syntax

// TODO currently only supports a subset of souffle which is needed to load a specific file
class Parser(source: Source) {
  import fastparse.{parse => _, _}
  import JavaWhitespace._

  def Analysis[_: P]: P[Seq[Syntax.SouffleContent]] =
    P(Start ~ AnalysisContent.rep ~ End)

  def AnalysisContent[_: P]: P[Syntax.SouffleContent] =
    P(ComponentInitialization | ComponentDefinition | TypeDeclaration |
      RuleSignature | Input | RuleDefinition | Output | PrintSize
    )

  def ComponentInitialization[_: P]: P[Syntax.ComponentInitialization] =
    P(".init" ~ identifier ~ "=" ~ identifier).mapWithLoc(Syntax.ComponentInitialization.tupled)

  def ComponentDefinition[_: P]: P[Syntax.ComponentDefinition] =
    P(".comp" ~ identifier ~ "{" ~ AnalysisContent.rep ~ "}").mapWithLoc(Syntax.ComponentDefinition.tupled)

  def TypeDeclaration[_: P]: P[Syntax.TypeDeclaration] =
    P(".type" ~ identifier ~ ("=" ~ DeclaredType).?).mapWithLoc(Syntax.TypeDeclaration.tupled)

  def RuleSignature[_: P]: P[Syntax.RuleSignature] =
    P(".decl" ~ identifier ~ "(" ~ RuleParameter.rep(1, sep = ",") ~ ")" ~ "output".!.?).mapWithLoc {
      case (rule, params, output) => Syntax.RuleSignature(rule, params, output.isDefined)
    }
  def RuleParameter[_: P]: P[Syntax.RuleParameter] =
    P(identifier ~ ":" ~ Type).map(Syntax.RuleParameter.tupled)

  def Output[_: P]: P[Syntax.Output] =
    P(".output" ~ identifier).mapWithLoc(Syntax.Output)

  def PrintSize[_: P]: P[Syntax.PrintSize] =
    P(".printsize" ~ identifier).mapWithLoc(Syntax.PrintSize)

  def Input[_: P]: P[Syntax.Input] =
    P(".input" ~ identifier ~ "(" ~
      "IO" ~ "=" ~ "\"file\"" ~
      "," ~
      "filename" ~ "=" ~ string ~
      "," ~
      "delimiter" ~ "=" ~ string ~
    ")").mapWithLoc(Syntax.Input.tupled)

  def Plan[_: P]: P[Unit] =
    P(".plan" ~ decimalinteger ~ "(" ~ decimalinteger.rep(sep = ",") ~ ")")

  def RuleDefinition[_: P]: P[Syntax.RuleDefinition] =
    P(RuleHead.rep(min = 1, sep = ",") ~ ":-" ~ Statement.rep(min = 1, sep = ",") ~ ".").mapWithLoc(Syntax.RuleDefinition.tupled)

  def RuleHead[_: P]: P[Syntax.RuleHead] =
    P(identifier ~ "(" ~ Expression.rep(min = 1, sep = ",") ~ ")").mapWithLoc(Syntax.RuleHead.tupled)

  def Statement[_: P]: P[Syntax.Statement] =
    P(RuleApplication | Equality | Parens )
  def RuleApplication[_: P]: P[Syntax.RelationApplication] =
    P("!".!.? ~ (identifier ~ ".").? ~ identifier ~ "(" ~ Expression.rep(min = 1, sep = ",") ~ ")").mapWithLoc {
      case (neg, comp, ruleName, args) => Syntax.RelationApplication(neg.isDefined, comp, ruleName, args)
    }
  def Equality[_: P]: P[Syntax.Equality] =
    P(Expression ~ ("!=" | "=").! ~ Expression).mapWithLoc {
      case (left, compare, right) => Syntax.Equality(left, compare == "!=", right)
    }
  def Parens[_: P]: P[Syntax.Statement] =
    P("(" ~ Statement ~ ")")


  def Expression[_: P]: P[Syntax.Expression] =
    P(Any | BuiltInFunctionCall | Variable | StringValue | NumberValue)
  def Variable[_: P]: P[Syntax.Variable] =
    P(identifier).mapWithLoc(Syntax.Variable)
  def StringValue[_: P]: P[Syntax.StringValue] =
    P(string).mapWithLoc(Syntax.StringValue)
  def NumberValue[_: P]: P[Syntax.NumberValue] =
    P(decimalinteger).mapWithLoc(Syntax.NumberValue)
  def Any[_: P]: P[Syntax.Wildcard.type] =
    P("_").mapWithLoc(_ => Syntax.Wildcard)
  def BuiltInFunctionCall[_: P]: P[Syntax.BuiltInFunctionCall] =
    P(BuiltInFunction ~ "(" ~ Expression.rep(min = 1, sep = ",") ~ ")").mapWithLoc(Syntax.BuiltInFunctionCall.tupled)


  def BuiltInFunction[_: P]: P[Syntax.BuiltInFunction] = CatBuiltInFunction
  def CatBuiltInFunction[_: P]: P[Syntax.CatBuiltInFunction.type] = P("cat").map(_ => Syntax.CatBuiltInFunction)


  def Type[_: P]: P[Syntax.Type] =
    P(SymbolType | NumberType | UnsignedType | FloatType | DeclaredType)

  def DeclaredType[_: P]: P[Syntax.DeclaredType] = P(identifier).map(Syntax.DeclaredType)

  def SymbolType[_: P]: P[Syntax.SymbolType.type] = P("symbol").map(_ => Syntax.SymbolType)
  def NumberType[_: P]: P[Syntax.NumberType.type] = P("number").map(_ => Syntax.NumberType)
  def UnsignedType[_: P]: P[Syntax.UnsignedType.type] = P("unsigned").map(_ => Syntax.UnsignedType)
  def FloatType[_: P]: P[Syntax.FloatType.type] = P("float").map(_ => Syntax.FloatType)


  def identifier[_: P]: P[Syntax.Name] =
    P(((letter | "_" | "?")  ~~ (letter | digit | "_").repX).!.filter(_ != "_")).mapWithLoc(Syntax.Name.apply)

  def letter[_: P]: P[Unit] = P( lowercase | uppercase )
  def lowercase[_: P]: P[Unit] = P( CharIn("a-z") )
  def uppercase[_: P]: P[Unit] = P( CharIn("A-Z") )
  def digit[_: P]: P[Unit] = P( CharIn("0-9") )

  def decimalinteger[_: P]: P[Int] = P( nonzerodigit ~~ digit.rep | "0").!.map(_.toInt)
  def nonzerodigit[_: P]: P[Unit] = P( CharIn("1-9") )

  def stringChars(c: Char) = c != '\"' && c != '\\'
  def strChars[_: P]       = P( CharsWhile(stringChars) )
  def hexDigit[_: P]       = P( CharIn("0-9a-fA-F") )
  def unicodeEscape[_: P]  = P( "u" ~~ hexDigit ~~ hexDigit ~~ hexDigit ~~ hexDigit )
  def escape[_: P]         = P( "\\" ~~ (CharIn("\"/\\\\bfnrt") | unicodeEscape) )
  def string[_: P]         = P( "\"" ~~ (strChars | escape).repX.! ~~ "\"")


  implicit class Ploc[T](p: => P[T])(implicit ctx: P[_]) {
    def mapWithLoc[U <: SourceLocation](f: T => U): P[U] =
      (Index ~ p ~ Index).map {
        case (start, t, end) =>
          val u = f(t)
          u.source = source
          u.startIndex = start
          u.endIndex = end
          u
      }

    def mapWithLocFun[U <: SourceLocation, V <: SourceLocation](f: T => (U => V)): P[U => V] =
      (Index ~ p ~ Index).map {
        case (start, t, end) =>
          val uv = f(t)
          u => {
            val v = uv(u)
            u.source = source
            v.startIndex = u.startIndex
            v.endIndex = end
            v
          }
      }

    def flatMapWithLoc[U <: SourceLocation](f: T => P[U]): P[U] =
      (Index ~ p ~ Index).flatMap {
        case (start, t, end) =>
          val up = f(t)
          up.map { u =>
            u.source = source
            u.startIndex = start
            u.endIndex = end
            u
          }
      }
  }
}


object Parser {
  def parse(source: Source): Syntax.SouffleModule = {
    import fastparse.Parsed
    val parser = new Parser(source)

    fastparse.parse(source.code, parser.Analysis(_), verboseFailures = true) match {
      case Parsed.Success(value, _) => Syntax.SouffleModule(value)
      case fail: Parsed.Failure =>
        throw new IllegalArgumentException(s"Parsing Error: ${fail.trace(true).longTerminalsMsg}")
    }
  }
}