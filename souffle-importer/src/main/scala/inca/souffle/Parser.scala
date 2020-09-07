package inca.souffle

object Parser {
  import fastparse._
  import JavaWhitespace._

  def Analysis[_: P]: P[Seq[Syntax.AnalysisContent]] =
    P(AnalysisContent.rep)

  def AnalysisContent[_: P]: P[Syntax.AnalysisContent] =
    P(ComponentInitialization | ComponentDefinition | TypeDeclaration |
      RuleSignature | Input | RuleDefinition
    )

  def ComponentInitialization[_: P]: P[Syntax.ComponentInitialization] =
    P(".init" ~ identifier ~ "=" ~ identifier).map(Syntax.ComponentInitialization.tupled)

  def ComponentDefinition[_: P]: P[Syntax.ComponentDefinition] =
    P(".comp" ~ identifier ~ "{" ~ AnalysisContent.rep ~ "}").map(Syntax.ComponentDefinition.tupled)

  def TypeDeclaration[_: P]: P[Syntax.TypeDeclaration] =
    P(".type" ~ identifier ~ ("=" ~ DeclaredType).?).map(Syntax.TypeDeclaration.tupled)

  def RuleSignature[_: P]: P[Syntax.RuleSignature] =
    P(".decl" ~ identifier ~ "(" ~ RuleParameter.rep(1) ~ ")" ~ "output".!.?).map {
      case (rule, params, output) => Syntax.RuleSignature(rule, params, output.isDefined)
    }
  def RuleParameter[_: P]: P[Syntax.RuleParameter] =
    P("?" ~ identifier ~ ":" ~ Type).map(Syntax.RuleParameter.tupled)

  def Input[_: P]: P[Syntax.Input] =
    P(".input" ~ identifier ~ "(" ~
      "IO" ~ "=" ~ "\"file\"" ~
      "filename" ~ "=" ~ string ~
      "delimiter" ~ "=" ~ string ~
    ")").map(Syntax.Input.tupled)

  def RuleDefinition[_: P]: P[Syntax.RuleDefinition] =
    P(RuleHead.rep(1) ~ ":-" ~ Statement.rep(1) ~ ".").map(Syntax.RuleDefinition.tupled)
  def RuleHead[_: P]: P[Syntax.RuleHead] =
    P(identifier ~ "(" ~ Expression.rep(1) ~ ")").map(Syntax.RuleHead.tupled)

  def Statement[_: P]: P[Syntax.Statement] =
    P(RuleApplication | Equality)
  def RuleApplication[_: P]: P[Syntax.RuleApplication] =
    P("!".!.? ~ (identifier ~ ".").? ~ identifier ~ Expression.rep(1)).map {
      case (neg, comp, ruleName, args) => Syntax.RuleApplication(neg.isDefined, comp, ruleName, args)
    }
  def Equality[_: P]: P[Syntax.Equality] =
    P(Expression ~ ("!=" | "=").! ~ Expression).map {
      case (left, compare, right) => Syntax.Equality(left, compare == "!=", right)
    }


  def Expression[_: P]: P[Syntax.Expression] =
    P(Variable | StringValue | NumberValue | Any | BuiltInFunctionCall)
  def Variable[_: P]: P[Syntax.Variable] =
    P("?" ~ identifier).map(Syntax.Variable)
  def StringValue[_: P]: P[Syntax.StringValue] =
    P(string).map(Syntax.StringValue)
  def NumberValue[_: P]: P[Syntax.NumberValue] =
    P(decimalinteger).map(Syntax.NumberValue)
  def Any[_: P]: P[Syntax.Any.type] =
    P("_").map(_ => Syntax.Any)
  def BuiltInFunctionCall[_: P]: P[Syntax.BuiltInFunctionCall] =
    P(BuiltInFunction ~ "(" ~ Expression.rep(1) ~ ")").map(Syntax.BuiltInFunctionCall.tupled)


  def BuiltInFunction[_: P]: P[Syntax.BuiltInFunction] = CatBuiltInFunction
  def CatBuiltInFunction[_: P]: P[Syntax.CatBuiltInFunction.type] = P("cat").map(_ => Syntax.CatBuiltInFunction)


  def Type[_: P]: P[Syntax.Type] =
    P(DeclaredType | SymbolType | NumberType | UnsignedType | FloatType)

  def DeclaredType[_: P]: P[Syntax.DeclaredType] = P(identifier).map(Syntax.DeclaredType)

  def SymbolType[_: P]: P[Syntax.SymbolType.type] = P("symbol").map(_ => Syntax.SymbolType)
  def NumberType[_: P]: P[Syntax.NumberType.type] = P("number").map(_ => Syntax.NumberType)
  def UnsignedType[_: P]: P[Syntax.UnsignedType.type] = P("unsigned").map(_ => Syntax.UnsignedType)
  def FloatType[_: P]: P[Syntax.FloatType.type] = P("float").map(_ => Syntax.FloatType)


  def identifier[_: P]: P[String] = P( (letter|"_") ~ (letter | digit | "_").rep ).!
  def letter[_: P]: P[Unit] = P( lowercase | uppercase )
  def lowercase[_: P]: P[Unit] = P( CharIn("a-z") )
  def uppercase[_: P]: P[Unit] = P( CharIn("A-Z") )
  def digit[_: P]: P[Unit] = P( CharIn("0-9") )

  def decimalinteger[_: P]: P[Int] = P( nonzerodigit ~ digit.rep | "0" ).!.map(_.toInt)
  def nonzerodigit[_: P]: P[Unit] = P( CharIn("1-9") )

  def stringChars(c: Char) = c != '\"' && c != '\\'
  def strChars[_: P]       = P( CharsWhile(stringChars) )
  def hexDigit[_: P]       = P( CharIn("0-9a-fA-F") )
  def unicodeEscape[_: P]  = P( "u" ~ hexDigit ~ hexDigit ~ hexDigit ~ hexDigit )
  def escape[_: P]         = P( "\\" ~ (CharIn("\"/\\\\bfnrt") | unicodeEscape) )
  def string[_: P]         = P( "\"" ~/ (strChars | escape).rep.! ~ "\"")
}
