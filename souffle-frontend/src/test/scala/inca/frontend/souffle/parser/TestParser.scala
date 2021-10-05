package inca.frontend.souffle.parser

import fastparse._
import inca.frontend.souffle.Souffle
import org.scalatest.flatspec.AnyFlatSpec

class TestParser extends AnyFlatSpec {

  "parsing" should " parse identifier " in {
    assertResult("?abc")(parse("?abc", Parser.identifier(_)).get.value)
    assertResult("abc")(parse("abc", Parser.identifier(_)).get.value)
    assertResult("_abc")(parse("_abc", Parser.identifier(_)).get.value)
    assertResult("Ab_C")(parse("Ab_C", Parser.identifier(_)).get.value)
    assertResult("Ab1C")(parse("Ab1C", Parser.identifier(_)).get.value)

    assertResult(false)(parse("0AbC", Parser.identifier(_)).isSuccess)
    assertResult(false)(parse("0A?C", Parser.identifier(_)).isSuccess)
    assertResult(false)(parse("-abc", Parser.identifier(_)).isSuccess)
    assertResult(false)(parse("∂", Parser.identifier(_)).isSuccess)
  }

  "parsing" should " parse decimalinteger " in {
    assertResult(0)(parse("0", Parser.decimalinteger(_)).get.value)
    assertResult(123)(parse("123", Parser.decimalinteger(_)).get.value)
    assertResult(103)(parse("103", Parser.decimalinteger(_)).get.value)

    assertResult(false)(parse("-abc", Parser.decimalinteger(_)).isSuccess)
    assertResult(false)(parse("∂", Parser.decimalinteger(_)).isSuccess)
  }

  "parsing" should " parse strings " in {
    assertResult(Souffle.StringValue("abc"))(parse("\"abc\"", Parser.StringValue(_)).get.value)
    assertResult(Souffle.StringValue("abc\n"))(parse("\"abc\n\"", Parser.StringValue(_)).get.value)
//    assertResult(Syntax.StringValue("\tabc"))(parse("\"\tabc\"", Parser.StringValue(_)).get.value)
//    assertResult(Syntax.StringValue("a\bb\f\rc"))(parse("\"a\bb\f\rc\"", Parser.StringValue(_)).get.value)

    assertResult(false)(parse("_12", Parser.StringValue(_)).isSuccess)
    assertResult(false)(parse("0AbC", Parser.StringValue(_)).isSuccess)
    assertResult(false)(parse("0A?C", Parser.StringValue(_)).isSuccess)
    assertResult(false)(parse("-abc", Parser.StringValue(_)).isSuccess)
    assertResult(false)(parse("∂", Parser.StringValue(_)).isSuccess)
  }

  "parsing" should " parse _ " in {
    assertResult(Souffle.Wildcard)(parse("_", Parser.Any(_)).get.value)

    assertResult(false)(parse("0AbC", Parser.Any(_)).isSuccess)
    assertResult(false)(parse("0A?C", Parser.Any(_)).isSuccess)
    assertResult(false)(parse("-abc", Parser.Any(_)).isSuccess)
    assertResult(false)(parse("∂", Parser.Any(_)).isSuccess)
  }

  "parsing" should " parse built-in function call" in {
    assertResult(
      Souffle.BuiltInFunctionCall(
        Souffle.CatBuiltInFunction,
        Seq(
          Souffle.Variable("s1"),
          Souffle.Variable("s2")))
    )(parse("cat(s1, s2)", Parser.BuiltInFunctionCall(_)).get.value)

    assertResult(
      Souffle.BuiltInFunctionCall(
        Souffle.CatBuiltInFunction,
        Seq(
          Souffle.Wildcard,
          Souffle.Variable("s2")))
    )(parse("cat(_, s2)", Parser.BuiltInFunctionCall(_)).get.value)
    assertResult(
      Souffle.BuiltInFunctionCall(
        Souffle.CatBuiltInFunction,
        Seq(Souffle.NumberValue(123)))
    )(parse("cat(123)", Parser.BuiltInFunctionCall(_)).get.value)

    assertResult(false)(parse("cat(c, ", Parser.BuiltInFunctionCall(_)).isSuccess)
    assertResult(false)(parse("cat(c b)", Parser.BuiltInFunctionCall(_)).isSuccess)
    assertResult(false)(parse("cat()c", Parser.BuiltInFunctionCall(_)).isSuccess)
    assertResult(false)(parse("cot()", Parser.BuiltInFunctionCall(_)).isSuccess)
  }

  "parsing" should " parse statements" in {
    assertResult(
      Souffle.Equality(
        Souffle.Variable("x"),
        not = false,
        Souffle.Variable("y"))
    )(parse("x = y", Parser.Statement(_)).get.value)

    assertResult(
      Souffle.Equality(
        Souffle.Variable("x"),
        not = true,
        Souffle.StringValue("abc"))
    )(parse("x != \"abc\"", Parser.Statement(_)).get.value)

    assertResult(
      Souffle.Parens(
        Souffle.Equality(
          Souffle.Variable("x"),
          not = true,
          Souffle.StringValue("abc"))
      )
    )(parse("(x != \"abc\")", Parser.Statement(_)).get.value)

    assertResult(
      Souffle.RuleApplication(
        false,
        None,
        "Rule",
        Seq(
          Souffle.Variable("x"),
          Souffle.StringValue("abc"),
          Souffle.NumberValue(123)
        )
      )
    )(parse("Rule(x, \"abc\", 123)", Parser.Statement(_)).get.value)

    assertResult(
      Souffle.RuleApplication(
        true,
        Some("comp"),
        "Rule",
        Seq(
          Souffle.Variable("x"),
          Souffle.StringValue("abc"),
          Souffle.NumberValue(123)
        )
      )
    )(parse("!comp.Rule(x, \"abc\", 123)", Parser.Statement(_)).get.value)

    assertResult(false)(parse("x,", Parser.Statement(_)).isSuccess)
    assertResult(false)(parse("?!Rule(c b)", Parser.Statement(_)).isSuccess)
    assertResult(false)(parse("Rule(c b)", Parser.Statement(_)).isSuccess)
    assertResult(false)(parse("Rule()c", Parser.Statement(_)).isSuccess)
  }

  "parsing" should " parse rule head" in {
    assertResult(
      Souffle.RuleHead(
        "_Rule",
        Seq(
          Souffle.Variable("x"),
          Souffle.Variable("y"),
          Souffle.Variable("z"))
      )
    )(parse("_Rule(x, y, z)", Parser.RuleHead(_)).get.value)

    assertResult(false)(parse("x,", Parser.Statement(_)).isSuccess)
    assertResult(false)(parse("?!Rule(c b)", Parser.Statement(_)).isSuccess)
    assertResult(false)(parse("Rule(c b)", Parser.Statement(_)).isSuccess)
    assertResult(false)(parse("Rule()c", Parser.Statement(_)).isSuccess)
  }

  "parsing" should " parse rule" in {
    assertResult(
      Souffle.RuleDefinition(
        Seq(
          Souffle.RuleHead(
          "Rule",
          Seq(
            Souffle.Variable("x"),
            Souffle.Variable("y"))
          )
        ),
        Seq(
          Souffle.RuleApplication(false, None, "Rule1", Seq(Souffle.Variable("x"))),
          Souffle.Equality(Souffle.Variable("y"), false, Souffle.BuiltInFunctionCall(Souffle.CatBuiltInFunction, Seq(Souffle.Variable("x"), Souffle.Variable("y")))),
          Souffle.RuleApplication(true, None, "Rule3", Seq(Souffle.Variable("x"), Souffle.Variable("y")))
        )
      )
    )(parse("Rule(x, y) :- Rule1(x), y = cat(x, y), !Rule3(x, y).", Parser.RuleDefinition(_)).get.value)

    assertResult(
      Souffle.RuleDefinition(
        Seq(
          Souffle.RuleHead(
            "Rule",
            Seq(
              Souffle.Variable("x"),
              Souffle.Variable("y"))
          ),
          Souffle.RuleHead(
            "Rule4",
            Seq(Souffle.Variable("y"))
          )
        ),
        Seq(
          Souffle.RuleApplication(false, None, "Rule1", Seq(Souffle.Variable("x"))),
          Souffle.Equality(Souffle.Variable("y"), false, Souffle.BuiltInFunctionCall(Souffle.CatBuiltInFunction, Seq(Souffle.Variable("x"), Souffle.Variable("y")))),
          Souffle.RuleApplication(true, None, "Rule3", Seq(Souffle.Variable("x"), Souffle.Variable("y")))
        )
      )
    )(parse("Rule(x, y), Rule4(y) :- Rule1(x), y = cat(x, y), !Rule3(x, y).", Parser.RuleDefinition(_)).get.value)

    assertResult(false)(parse("Rule(x, y)", Parser.RuleDefinition(_)).isSuccess)
    assertResult(false)(parse(":- R(x).", Parser.RuleDefinition(_)).isSuccess)
    assertResult(false)(parse("R(x) :- R(x), .", Parser.RuleDefinition(_)).isSuccess)
    assertResult(false)(parse("Rule().c", Parser.RuleDefinition(_)).isSuccess)
    assertResult(false)(parse("Rule(1), :- R(x).", Parser.RuleDefinition(_)).isSuccess)
  }

  "parsing" should " parse rule 2" in {
    assertResult(
      Souffle.RuleDefinition(
        Seq(
          Souffle.RuleHead(
            "Rule",
            Seq(
              Souffle.Variable("x"),
              Souffle.Variable("y"))
          )
        ),
        Seq(
          Souffle.RuleApplication(false, None, "Rule1", Seq(Souffle.Variable("x"))),
          Souffle.Equality(Souffle.Variable("y"), false, Souffle.BuiltInFunctionCall(Souffle.CatBuiltInFunction, Seq(Souffle.Variable("x"), Souffle.Variable("y")))),
          Souffle.RuleApplication(true, None, "Rule3", Seq(Souffle.Variable("x"), Souffle.Variable("y")))
        )
      )
    )(parse("Rule(x, y) :- Rule1(x), y = cat(x, y), !Rule3(x, y).", Parser.RuleDefinition(_)).get.value)

    assertResult(
      Souffle.RuleDefinition(
        Seq(
          Souffle.RuleHead(
            "Rule",
            Seq(
              Souffle.Variable("x"),
              Souffle.Variable("y"))
          ),
          Souffle.RuleHead(
            "Rule4",
            Seq(Souffle.Variable("y"))
          )
        ),
        Seq(
          Souffle.RuleApplication(false, None, "Rule1", Seq(Souffle.Variable("x"))),
          Souffle.Equality(Souffle.Variable("y"), false, Souffle.BuiltInFunctionCall(Souffle.CatBuiltInFunction, Seq(Souffle.Variable("x"), Souffle.Variable("y")))),
          Souffle.RuleApplication(true, None, "Rule3", Seq(Souffle.Variable("x"), Souffle.Variable("y")))
        )
      )
    )(parse("Rule(x, y), Rule4(y) :- Rule1(x), y = cat(x, y), !Rule3(x, y).", Parser.RuleDefinition(_)).get.value)

    assertResult(false)(parse("Rule(x, y)", Parser.RuleDefinition(_)).isSuccess)
    assertResult(false)(parse(":- R(x).", Parser.RuleDefinition(_)).isSuccess)
    assertResult(false)(parse("R(x) :- R(x), .", Parser.RuleDefinition(_)).isSuccess)
    assertResult(false)(parse("Rule().c", Parser.RuleDefinition(_)).isSuccess)
    assertResult(false)(parse("Rule(1), :- R(x).", Parser.RuleDefinition(_)).isSuccess)
  }

  "parsing" should " input" in {
    assertResult(
      Souffle.Input("Rule", "path", "\n")
    )(parse(".input Rule(IO=\"file\", filename=\"path\", delimiter=\"\n\")", Parser.Input(_)).get.value)

    assertResult(false)(parse(".input Rule(OI=\"file\", filename=\"path\", delimiter=\"\n\")", Parser.Input(_)).isSuccess)
    assertResult(false)(parse(".input Rule(IO=\"x\", filename=\"path\", delimiter=\"\n\")", Parser.Input(_)).isSuccess)
    assertResult(false)(parse(".input Rule(IO=\"file\", filname=\"path\", delimiter=\"\n\")", Parser.Input(_)).isSuccess)
    assertResult(false)(parse(".input Rule(IO=\"file\", filename=\"path\", delimitr=\"\n\")", Parser.Input(_)).isSuccess)
    assertResult(false)(parse(".input Rule(IO=\"file, filename=\"path\", delimiter=\"\n\")", Parser.Input(_)).isSuccess)
    assertResult(false)(parse(".input Rule(IO=\"file, filename=path\", delimiter=\"\n\")", Parser.Input(_)).isSuccess)
    assertResult(false)(parse(".input Rule(IO=\"file, filename=path\", delimiter=\n\")", Parser.Input(_)).isSuccess)
  }

  "parsing" should " output" in {
    assertResult(
      Souffle.Output("Rule")
    )(parse(".output Rule", Parser.Output(_)).get.value)

    assertResult(false)(parse(".otput Rule", Parser.Output(_)).isSuccess)
  }

  "parsing" should " printsize" in {
    assertResult(
      Souffle.PrintSize("Rule")
    )(parse(".printsize Rule", Parser.PrintSize(_)).get.value)

    assertResult(false)(parse(".printsze Rule", Parser.PrintSize(_)).isSuccess)
  }

  "parsing" should " rule signature" in {
    assertResult(
      Souffle.RuleSignature(
        "Rule",
        Seq(
          Souffle.RuleParameter("x", Souffle.SymbolType),
          Souffle.RuleParameter("y", Souffle.NumberType),
          Souffle.RuleParameter("z", Souffle.DeclaredType("Var"))),
        false
      )
    )(parse(".decl Rule(x: symbol, y: number, z: Var)", Parser.RuleSignature(_)).get.value)

    assertResult(false)(parse(".decl Rule()", Parser.RuleSignature(_)).isSuccess)
    assertResult(false)(parse(".decl Rule(x symbol)", Parser.RuleSignature(_)).isSuccess)
    assertResult(false)(parse(".decl Rule(x)", Parser.RuleSignature(_)).isSuccess)
//    assertResult(false)(parse(".decl Rule(x: symbol).", Parser.RuleSignature(_)).isSuccess)
    assertResult(false)(parse(".decl 0Rule(x: symbol)", Parser.RuleSignature(_)).isSuccess)
  }

  "parsing" should " type declaration" in {
    assertResult(
      Souffle.TypeDeclaration("Ty", None)
    )(parse(".type Ty", Parser.TypeDeclaration(_)).get.value)

    assertResult(
      Souffle.TypeDeclaration("Ty1", Some(Souffle.DeclaredType("Ty2")))
    )(parse(".type Ty1 = Ty2", Parser.TypeDeclaration(_)).get.value)

    assertResult(false)(parse(".ty Ty", Parser.TypeDeclaration(_)).isSuccess)
    assertResult(false)(parse(".ty Ty +", Parser.TypeDeclaration(_)).isSuccess)
    assertResult(false)(parse(".ty Ty.", Parser.TypeDeclaration(_)).isSuccess)
  }

  "parsing" should " component definition" in {
    assertResult(
      Souffle.ComponentDefinition("Comp", Seq())
    )(parse(".comp Comp { }", Parser.ComponentDefinition(_)).get.value)

    assertResult(
      Souffle.ComponentDefinition("Comp",
        Seq(
          Souffle.TypeDeclaration("T", None),
          Souffle.RuleSignature("R", Seq(
           Souffle.RuleParameter("x", Souffle.DeclaredType("T")),
           Souffle.RuleParameter("y", Souffle.DeclaredType("T"))
          ), false)
        )
      )
    )(parse(".comp Comp { .type T \n.decl R(x: T, y: T) }", Parser.ComponentDefinition(_)).get.value)

    assertResult(false)(parse(".comp Comp { .type T \n.decl R(x: T, y: T) ", Parser.ComponentDefinition(_)).isSuccess)
  }

  "parsing" should " component initialization" in {
    assertResult(
      Souffle.ComponentInitialization("Comp", "Other")
    )(parse(".init Comp = Other", Parser.ComponentInitialization(_)).get.value)

    assertResult(false)(parse(".init Comp", Parser.ComponentInitialization(_)).isSuccess)
    assertResult(false)(parse(".init Comp != Other", Parser.ComponentInitialization(_)).isSuccess)
    assertResult(false)(parse(".init Comp ! Other", Parser.ComponentInitialization(_)).isSuccess)
  }

  "parsing" should " analysis" in {
    assertResult(
      Seq(
        Souffle.TypeDeclaration("T", None),
        Souffle.RuleSignature("R", Seq(Souffle.RuleParameter("x", Souffle.SymbolType)), false),
        Souffle.ComponentInitialization("Comp", "Other")
      )
    )(parse(".type T\n.decl R(x: symbol)\n .init Comp = Other", Parser.Analysis(_)).get.value)

    assertResult(
      Seq(
        Souffle.TypeDeclaration("T", None),
        Souffle.ComponentDefinition("R", Seq(
          Souffle.TypeDeclaration("X", None),
          Souffle.TypeDeclaration("Y", None)
        )),
      )
    )(parse(".type T\n.comp R {\n.type X\n.type Y \n}", Parser.Analysis(_)).get.value)

    assertResult(false)(parse(".type Comp\n.decl R(x: symbol)\nj", Parser.Analysis(_)).isSuccess)
    assertResult(false)(parse(".type Comp\n.decl R(x: symbol", Parser.Analysis(_)).isSuccess)
  }
}
