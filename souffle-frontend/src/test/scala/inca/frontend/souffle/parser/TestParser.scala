package inca.frontend.souffle.parser

import fastparse._
import inca.compiler.source.NoSource
import inca.frontend.souffle.Syntax
import inca.frontend.souffle.Syntax.Name
import org.scalatest.flatspec.AnyFlatSpec

class TestParser extends AnyFlatSpec {

  val parser: Parser = new Parser(NoSource)
  
  "parsing" should " parse identifier " in {
    assertResult(Name("?abc"))(parse("?abc", parser.identifier(_)).get.value)
    assertResult(Name("abc"))(parse("abc", parser.identifier(_)).get.value)
    assertResult(Name("_abc"))(parse("_abc", parser.identifier(_)).get.value)
    assertResult(Name("Ab_C"))(parse("Ab_C", parser.identifier(_)).get.value)
    assertResult(Name("Ab1C"))(parse("Ab1C", parser.identifier(_)).get.value)

    assertResult(false)(parse("0AbC", parser.identifier(_)).isSuccess)
    assertResult(false)(parse("0A?C", parser.identifier(_)).isSuccess)
    assertResult(false)(parse("-abc", parser.identifier(_)).isSuccess)
    assertResult(false)(parse("∂", parser.identifier(_)).isSuccess)
  }

  "parsing" should " parse decimalinteger " in {
    assertResult(0)(parse("0", parser.decimalinteger(_)).get.value)
    assertResult(123)(parse("123", parser.decimalinteger(_)).get.value)
    assertResult(103)(parse("103", parser.decimalinteger(_)).get.value)

    assertResult(false)(parse("-abc", parser.decimalinteger(_)).isSuccess)
    assertResult(false)(parse("∂", parser.decimalinteger(_)).isSuccess)
  }

  "parsing" should " parse strings " in {
    assertResult(Syntax.StringValue("abc"))(parse("\"abc\"", parser.StringValue(_)).get.value)
    assertResult(Syntax.StringValue("abc\n"))(parse("\"abc\n\"", parser.StringValue(_)).get.value)
//    assertResult(Syntax.StringValue("\tabc"))(parse("\"\tabc\"", parser.StringValue(_)).get.value)
//    assertResult(Syntax.StringValue("a\bb\f\rc"))(parse("\"a\bb\f\rc\"", parser.StringValue(_)).get.value)

    assertResult(false)(parse("_12", parser.StringValue(_)).isSuccess)
    assertResult(false)(parse("0AbC", parser.StringValue(_)).isSuccess)
    assertResult(false)(parse("0A?C", parser.StringValue(_)).isSuccess)
    assertResult(false)(parse("-abc", parser.StringValue(_)).isSuccess)
    assertResult(false)(parse("∂", parser.StringValue(_)).isSuccess)
  }

  "parsing" should " parse _ " in {
    assertResult(Syntax.Wildcard)(parse("_", parser.Any(_)).get.value)

    assertResult(false)(parse("0AbC", parser.Any(_)).isSuccess)
    assertResult(false)(parse("0A?C", parser.Any(_)).isSuccess)
    assertResult(false)(parse("-abc", parser.Any(_)).isSuccess)
    assertResult(false)(parse("∂", parser.Any(_)).isSuccess)
  }

  "parsing" should " parse built-in function call" in {
    assertResult(
      Syntax.BuiltInFunctionCall(
        Syntax.CatBuiltInFunction,
        Seq(
          Syntax.Variable(Name("s1")),
          Syntax.Variable(Name("s2"))))
    )(parse("cat(s1, s2)", parser.BuiltInFunctionCall(_)).get.value)

    assertResult(
      Syntax.BuiltInFunctionCall(
        Syntax.CatBuiltInFunction,
        Seq(
          Syntax.Wildcard,
          Syntax.Variable(Name("s2"))))
    )(parse("cat(_, s2)", parser.BuiltInFunctionCall(_)).get.value)
    assertResult(
      Syntax.BuiltInFunctionCall(
        Syntax.CatBuiltInFunction,
        Seq(Syntax.NumberValue(123)))
    )(parse("cat(123)", parser.BuiltInFunctionCall(_)).get.value)

    assertResult(false)(parse("cat(c, ", parser.BuiltInFunctionCall(_)).isSuccess)
    assertResult(false)(parse("cat(c b)", parser.BuiltInFunctionCall(_)).isSuccess)
    assertResult(false)(parse("cat()c", parser.BuiltInFunctionCall(_)).isSuccess)
    assertResult(false)(parse("cot()", parser.BuiltInFunctionCall(_)).isSuccess)
  }

  "parsing" should " parse statements" in {
    assertResult(
      Syntax.Equality(
        Syntax.Variable(Name("x")),
        not = false,
        Syntax.Variable(Name("y")))
    )(parse("x = y", parser.Statement(_)).get.value)

    assertResult(
      Syntax.Equality(
        Syntax.Variable(Name("x")),
        not = true,
        Syntax.StringValue("abc"))
    )(parse("x != \"abc\"", parser.Statement(_)).get.value)

    assertResult(
      Syntax.Equality(
        Syntax.Variable(Name("x")),
        not = true,
        Syntax.StringValue("abc")
      )
    )(parse("(x != \"abc\")", parser.Statement(_)).get.value)

    assertResult(
      Syntax.RelationApplication(
        false,
        None,
        Name("Rule"),
        Seq(
          Syntax.Variable(Name("x")),
          Syntax.StringValue("abc"),
          Syntax.NumberValue(123)
        )
      )
    )(parse("Rule(x, \"abc\", 123)", parser.Statement(_)).get.value)

    assertResult(
      Syntax.RelationApplication(
        true,
        Some(Name("comp")),
        Name("Rule"),
        Seq(
          Syntax.Variable(Name("x")),
          Syntax.StringValue("abc"),
          Syntax.NumberValue(123)
        )
      )
    )(parse("!comp.Rule(x, \"abc\", 123)", parser.Statement(_)).get.value)

    assertResult(false)(parse("x,", parser.Statement(_)).isSuccess)
    assertResult(false)(parse("?!Rule(c b)", parser.Statement(_)).isSuccess)
    assertResult(false)(parse("Rule(c b)", parser.Statement(_)).isSuccess)
    assertResult(false)(parse("Rule()c", parser.Statement(_)).isSuccess)
  }

  "parsing" should " parse rule head" in {
    assertResult(
      Syntax.RuleHead(
        Name("_Rule"),
        Seq(
          Syntax.Variable(Name("x")),
          Syntax.Variable(Name("y")),
          Syntax.Variable(Name("z")))
      )
    )(parse("_Rule(x, y, z)", parser.RuleHead(_)).get.value)

    assertResult(false)(parse("x,", parser.Statement(_)).isSuccess)
    assertResult(false)(parse("?!Rule(c b)", parser.Statement(_)).isSuccess)
    assertResult(false)(parse("Rule(c b)", parser.Statement(_)).isSuccess)
    assertResult(false)(parse("Rule()c", parser.Statement(_)).isSuccess)
  }

  "parsing" should " parse rule" in {
    assertResult(
      Syntax.RuleDefinition(
        Seq(
          Syntax.RuleHead(
          Name("Rule"),
          Seq(
            Syntax.Variable(Name("x")),
            Syntax.Variable(Name("y")))
          )
        ),
        Seq(
          Syntax.RelationApplication(false, None, Name("Rule1"), Seq(Syntax.Variable(Name("x")))),
          Syntax.Equality(Syntax.Variable(Name("y")), false, Syntax.BuiltInFunctionCall(Syntax.CatBuiltInFunction, Seq(Syntax.Variable(Name("x")), Syntax.Variable(Name("y"))))),
          Syntax.RelationApplication(true, None, Name("Rule3"), Seq(Syntax.Variable(Name("x")), Syntax.Variable(Name("y"))))
        )
      )
    )(parse("Rule(x, y) :- Rule1(x), y = cat(x, y), !Rule3(x, y).", parser.RuleDefinition(_)).get.value)

    assertResult(
      Syntax.RuleDefinition(
        Seq(
          Syntax.RuleHead(
            Name("Rule"),
            Seq(
              Syntax.Variable(Name("x")),
              Syntax.Variable(Name("y")))
          ),
          Syntax.RuleHead(
            Name("Rule4"),
            Seq(Syntax.Variable(Name("y")))
          )
        ),
        Seq(
          Syntax.RelationApplication(false, None, Name("Rule1"), Seq(Syntax.Variable(Name("x")))),
          Syntax.Equality(Syntax.Variable(Name("y")), false, Syntax.BuiltInFunctionCall(Syntax.CatBuiltInFunction, Seq(Syntax.Variable(Name("x")), Syntax.Variable(Name("y"))))),
          Syntax.RelationApplication(true, None, Name("Rule3"), Seq(Syntax.Variable(Name("x")), Syntax.Variable(Name("y"))))
        )
      )
    )(parse("Rule(x, y), Rule4(y) :- Rule1(x), y = cat(x, y), !Rule3(x, y).", parser.RuleDefinition(_)).get.value)

    assertResult(false)(parse("Rule(x, y)", parser.RuleDefinition(_)).isSuccess)
    assertResult(false)(parse(":- R(x).", parser.RuleDefinition(_)).isSuccess)
    assertResult(false)(parse("R(x) :- R(x), .", parser.RuleDefinition(_)).isSuccess)
    assertResult(false)(parse("Rule().c", parser.RuleDefinition(_)).isSuccess)
    assertResult(false)(parse("Rule(1), :- R(x).", parser.RuleDefinition(_)).isSuccess)
  }

  "parsing" should " parse rule 2" in {
    assertResult(
      Syntax.RuleDefinition(
        Seq(
          Syntax.RuleHead(
            Name("Rule"),
            Seq(
              Syntax.Variable(Name("x")),
              Syntax.Variable(Name("y")))
          )
        ),
        Seq(
          Syntax.RelationApplication(false, None, Name("Rule1"), Seq(Syntax.Variable(Name("x")))),
          Syntax.Equality(Syntax.Variable(Name("y")), false, Syntax.BuiltInFunctionCall(Syntax.CatBuiltInFunction, Seq(Syntax.Variable(Name("x")), Syntax.Variable(Name("y"))))),
          Syntax.RelationApplication(true, None, Name("Rule3"), Seq(Syntax.Variable(Name("x")), Syntax.Variable(Name("y"))))
        )
      )
    )(parse("Rule(x, y) :- Rule1(x), y = cat(x, y), !Rule3(x, y).", parser.RuleDefinition(_)).get.value)

    assertResult(
      Syntax.RuleDefinition(
        Seq(
          Syntax.RuleHead(
            Name("Rule"),
            Seq(
              Syntax.Variable(Name("x")),
              Syntax.Variable(Name("y")))
          ),
          Syntax.RuleHead(
            Name("Rule4"),
            Seq(Syntax.Variable(Name("y")))
          )
        ),
        Seq(
          Syntax.RelationApplication(false, None, Name("Rule1"), Seq(Syntax.Variable(Name("x")))),
          Syntax.Equality(Syntax.Variable(Name("y")), false, Syntax.BuiltInFunctionCall(Syntax.CatBuiltInFunction, Seq(Syntax.Variable(Name("x")), Syntax.Variable(Name("y"))))),
          Syntax.RelationApplication(true, None, Name("Rule3"), Seq(Syntax.Variable(Name("x")), Syntax.Variable(Name("y"))))
        )
      )
    )(parse("Rule(x, y), Rule4(y) :- Rule1(x), y = cat(x, y), !Rule3(x, y).", parser.RuleDefinition(_)).get.value)

    assertResult(false)(parse("Rule(x, y)", parser.RuleDefinition(_)).isSuccess)
    assertResult(false)(parse(":- R(x).", parser.RuleDefinition(_)).isSuccess)
    assertResult(false)(parse("R(x) :- R(x), .", parser.RuleDefinition(_)).isSuccess)
    assertResult(false)(parse("Rule().c", parser.RuleDefinition(_)).isSuccess)
    assertResult(false)(parse("Rule(1), :- R(x).", parser.RuleDefinition(_)).isSuccess)
  }

  "parsing" should " input" in {
    assertResult(
      Syntax.Input(Name("Rule"), "path", "\n")
    )(parse(".input Rule(IO=\"file\", filename=\"path\", delimiter=\"\n\")", parser.Input(_)).get.value)

    assertResult(false)(parse(".input Rule(OI=\"file\", filename=\"path\", delimiter=\"\n\")", parser.Input(_)).isSuccess)
    assertResult(false)(parse(".input Rule(IO=\"x\", filename=\"path\", delimiter=\"\n\")", parser.Input(_)).isSuccess)
    assertResult(false)(parse(".input Rule(IO=\"file\", filname=\"path\", delimiter=\"\n\")", parser.Input(_)).isSuccess)
    assertResult(false)(parse(".input Rule(IO=\"file\", filename=\"path\", delimitr=\"\n\")", parser.Input(_)).isSuccess)
    assertResult(false)(parse(".input Rule(IO=\"file, filename=\"path\", delimiter=\"\n\")", parser.Input(_)).isSuccess)
    assertResult(false)(parse(".input Rule(IO=\"file, filename=path\", delimiter=\"\n\")", parser.Input(_)).isSuccess)
    assertResult(false)(parse(".input Rule(IO=\"file, filename=path\", delimiter=\n\")", parser.Input(_)).isSuccess)
  }

  "parsing" should " output" in {
    assertResult(
      Syntax.Output(Name("Rule"))
    )(parse(".output Rule", parser.Output(_)).get.value)

    assertResult(false)(parse(".otput Rule", parser.Output(_)).isSuccess)
  }

  "parsing" should " printsize" in {
    assertResult(
      Syntax.PrintSize(Name("Rule"))
    )(parse(".printsize Rule", parser.PrintSize(_)).get.value)

    assertResult(false)(parse(".printsze Rule", parser.PrintSize(_)).isSuccess)
  }

  "parsing" should " rule signature" in {
    assertResult(
      Syntax.RuleSignature(
        Name("Rule"),
        Seq(
          Syntax.RuleParameter(Name("x"), Syntax.SymbolType),
          Syntax.RuleParameter(Name("y"), Syntax.NumberType),
          Syntax.RuleParameter(Name("z"), Syntax.DeclaredType(Name("Var")))),
        false
      )
    )(parse(".decl Rule(x: symbol, y: number, z: Var)", parser.RuleSignature(_)).get.value)

    assertResult(false)(parse(".decl Rule()", parser.RuleSignature(_)).isSuccess)
    assertResult(false)(parse(".decl Rule(x symbol)", parser.RuleSignature(_)).isSuccess)
    assertResult(false)(parse(".decl Rule(x)", parser.RuleSignature(_)).isSuccess)
//    assertResult(false)(parse(".decl Rule(x: symbol).", parser.RuleSignature(_)).isSuccess)
    assertResult(false)(parse(".decl 0Rule(x: symbol)", parser.RuleSignature(_)).isSuccess)
  }

  "parsing" should " type declaration" in {
    assertResult(
      Syntax.TypeDeclaration(Name("Ty"), None)
    )(parse(".type Ty", parser.TypeDeclaration(_)).get.value)

    assertResult(
      Syntax.TypeDeclaration(Name("Ty1"), Some(Syntax.DeclaredType(Name("Ty2"))))
    )(parse(".type Ty1 = Ty2", parser.TypeDeclaration(_)).get.value)

    assertResult(false)(parse(".ty Ty", parser.TypeDeclaration(_)).isSuccess)
    assertResult(false)(parse(".ty Ty +", parser.TypeDeclaration(_)).isSuccess)
    assertResult(false)(parse(".ty Ty.", parser.TypeDeclaration(_)).isSuccess)
  }

  "parsing" should " component definition" in {
    assertResult(
      Syntax.ComponentDefinition(Name("Comp"), Seq())
    )(parse(".comp Comp { }", parser.ComponentDefinition(_)).get.value)

    assertResult(
      Syntax.ComponentDefinition(Name("Comp"),
        Seq(
          Syntax.TypeDeclaration(Name("T"), None),
          Syntax.RuleSignature(Name("R"), Seq(
           Syntax.RuleParameter(Name("x"), Syntax.DeclaredType(Name("T"))),
           Syntax.RuleParameter(Name("y"), Syntax.DeclaredType(Name("T")))
          ), false)
        )
      )
    )(parse(".comp Comp { .type T \n.decl R(x: T, y: T) }", parser.ComponentDefinition(_)).get.value)

    assertResult(false)(parse(".comp Comp { .type T \n.decl R(x: T, y: T) ", parser.ComponentDefinition(_)).isSuccess)
  }

  "parsing" should " component initialization" in {
    assertResult(
      Syntax.ComponentInitialization(Name("Comp"), Name("Other"))
    )(parse(".init Comp = Other", parser.ComponentInitialization(_)).get.value)

    assertResult(false)(parse(".init Comp", parser.ComponentInitialization(_)).isSuccess)
    assertResult(false)(parse(".init Comp != Other", parser.ComponentInitialization(_)).isSuccess)
    assertResult(false)(parse(".init Comp ! Other", parser.ComponentInitialization(_)).isSuccess)
  }

  "parsing" should " analysis" in {
    assertResult(
      Seq(
        Syntax.TypeDeclaration(Name("T"), None),
        Syntax.RuleSignature(Name("R"), Seq(Syntax.RuleParameter(Name("x"), Syntax.SymbolType)), false),
        Syntax.ComponentInitialization(Name("Comp"), Name("Other"))
      )
    )(parse(".type T\n.decl R(x: symbol)\n .init Comp = Other", parser.Analysis(_)).get.value)

    assertResult(
      Seq(
        Syntax.TypeDeclaration(Name("T"), None),
        Syntax.ComponentDefinition(Name("R"), Seq(
          Syntax.TypeDeclaration(Name("X"), None),
          Syntax.TypeDeclaration(Name("Y"), None)
        )),
      )
    )(parse(".type T\n.comp R {\n.type X\n.type Y \n}", parser.Analysis(_)).get.value)

    assertResult(false)(parse(".type Comp\n.decl R(x: symbol)\nj", parser.Analysis(_)).isSuccess)
    assertResult(false)(parse(".type Comp\n.decl R(x: symbol", parser.Analysis(_)).isSuccess)
  }
}
