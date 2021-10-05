package inca.frontend.souffle.parser

import inca.frontend.souffle.Souffle

// TODO currently only supports a subset of souffle which is needed to load a specific file
object Parser {
  import fastparse.{parse => _, _}
  import JavaWhitespace._

  def parse(code: ParserInput): Souffle.Module = {
    import fastparse.Parsed

    fastparse.parse(code, Analysis(_), verboseFailures = true) match {
      case Parsed.Success(value, _) => Souffle.Module(value)
      case fail: Parsed.Failure =>
        throw new IllegalArgumentException(s"Parsing Error: ${fail.trace(true).longTerminalsMsg}")
    }
  }

  def Analysis[_: P]: P[Seq[Souffle.SouffleContent]] =
    P(Start ~ AnalysisContent.rep ~ End)

  def AnalysisContent[_: P]: P[Souffle.SouffleContent] =
    P(ComponentInitialization | ComponentDefinition | TypeDeclaration |
      RuleSignature | Input | RuleDefinition | Output | PrintSize
    )

  def ComponentInitialization[_: P]: P[Souffle.ComponentInitialization] =
    P(".init" ~ identifier ~ "=" ~ identifier).map(Souffle.ComponentInitialization.tupled)

  def ComponentDefinition[_: P]: P[Souffle.ComponentDefinition] =
    P(".comp" ~ identifier ~ "{" ~ AnalysisContent.rep ~ "}").map(Souffle.ComponentDefinition.tupled)

  def TypeDeclaration[_: P]: P[Souffle.TypeDeclaration] =
    P(".type" ~ identifier ~ ("=" ~ DeclaredType).?).map(Souffle.TypeDeclaration.tupled)

  def RuleSignature[_: P]: P[Souffle.RuleSignature] =
    P(".decl" ~ identifier ~ "(" ~ RuleParameter.rep(1, sep = ",") ~ ")" ~ "output".!.?).map {
      case (rule, params, output) => Souffle.RuleSignature(rule, params, output.isDefined)
    }
  def RuleParameter[_: P]: P[Souffle.RuleParameter] =
    P(identifier ~ ":" ~ Type).map(Souffle.RuleParameter.tupled)

  def Output[_: P]: P[Souffle.Output] =
    P(".output" ~ identifier).map(Souffle.Output)

  def PrintSize[_: P]: P[Souffle.PrintSize] =
    P(".printsize" ~ identifier).map(Souffle.PrintSize)

  def Input[_: P]: P[Souffle.Input] =
    P(".input" ~ identifier ~ "(" ~
      "IO" ~ "=" ~ "\"file\"" ~
      "," ~
      "filename" ~ "=" ~ string ~
      "," ~
      "delimiter" ~ "=" ~ string ~
    ")").map(Souffle.Input.tupled)

  def Plan[_: P]: P[Unit] =
    P(".plan" ~ decimalinteger ~ "(" ~ decimalinteger.rep(sep = ",") ~ ")")

  def RuleDefinition[_: P]: P[Souffle.RuleDefinition] =
    P(RuleHead.rep(min = 1, sep = ",") ~ ":-" ~ Statement.rep(min = 1, sep = ",") ~ ".").map(Souffle.RuleDefinition.tupled)

  def RuleHead[_: P]: P[Souffle.RuleHead] =
    P(identifier ~ "(" ~ Expression.rep(min = 1, sep = ",") ~ ")").map(Souffle.RuleHead.tupled)

  def Statement[_: P]: P[Souffle.Statement] =
    P(RuleApplication | Equality | Parens )
  def RuleApplication[_: P]: P[Souffle.RuleApplication] =
    P("!".!.? ~ (identifier ~ ".").? ~ identifier ~ "(" ~ Expression.rep(min = 1, sep = ",") ~ ")").map {
      case (neg, comp, ruleName, args) => Souffle.RuleApplication(neg.isDefined, comp, ruleName, args)
    }
  def Equality[_: P]: P[Souffle.Equality] =
    P(Expression ~ ("!=" | "=").! ~ Expression).map {
      case (left, compare, right) => Souffle.Equality(left, compare == "!=", right)
    }
  def Parens[_: P]: P[Souffle.Statement] =
    P("(" ~ Statement ~ ")").map(Souffle.Parens)


  def Expression[_: P]: P[Souffle.Expression] =
    P(Any | BuiltInFunctionCall | Variable | StringValue | NumberValue)
  def Variable[_: P]: P[Souffle.Variable] =
    P(identifier).map(Souffle.Variable)
  def StringValue[_: P]: P[Souffle.StringValue] =
    P(string).map(Souffle.StringValue)
  def NumberValue[_: P]: P[Souffle.NumberValue] =
    P(decimalinteger).map(Souffle.NumberValue)
  def Any[_: P]: P[Souffle.Wildcard.type] =
    P("_").map(_ => Souffle.Wildcard)
  def BuiltInFunctionCall[_: P]: P[Souffle.BuiltInFunctionCall] =
    P(BuiltInFunction ~ "(" ~ Expression.rep(min = 1, sep = ",") ~ ")").map(Souffle.BuiltInFunctionCall.tupled)


  def BuiltInFunction[_: P]: P[Souffle.BuiltInFunction] = CatBuiltInFunction
  def CatBuiltInFunction[_: P]: P[Souffle.CatBuiltInFunction.type] = P("cat").map(_ => Souffle.CatBuiltInFunction)


  def Type[_: P]: P[Souffle.Type] =
    P(SymbolType | NumberType | UnsignedType | FloatType | DeclaredType)

  def DeclaredType[_: P]: P[Souffle.DeclaredType] = P(identifier).map(Souffle.DeclaredType)

  def SymbolType[_: P]: P[Souffle.SymbolType.type] = P("symbol").map(_ => Souffle.SymbolType)
  def NumberType[_: P]: P[Souffle.NumberType.type] = P("number").map(_ => Souffle.NumberType)
  def UnsignedType[_: P]: P[Souffle.UnsignedType.type] = P("unsigned").map(_ => Souffle.UnsignedType)
  def FloatType[_: P]: P[Souffle.FloatType.type] = P("float").map(_ => Souffle.FloatType)


  def identifier[_: P]: P[String] = P( (letter | "_" | "?")  ~~ (letter | digit | "_").repX).!.filter(_ != "_")
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
}
