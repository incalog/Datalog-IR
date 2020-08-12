package inca.frontend.funext

import inca.analyzedLangs.Exp
import inca.frontend.fun.Fun._
import inca.runtime.context.QueryScope
import inca.{CompilerOptions, IncaMatchers}
import org.scalatest.flatspec.AnyFlatSpec

class TestMatch extends AnyFlatSpec with IncaMatchers {

  val one = Constant(IntLiteral(1))
  val two = Constant(IntLiteral(2))

  val scope: QueryScope = new QueryScope(Exp.languageMetaInfo)
  val options: CompilerOptions = CompilerOptions(scope.langMetaInfo, desugarables = Seq(Match))

  "desugaring" should "eliminate wildcard pattern" in {
    val sugared = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), Seq(), Seq(Body(Seq(
        Match(one, Seq(
          Case(WildcardPattern, Body(
            Assign(Seq("case"), Constant(IntLiteral(7)))
          ))
        ))
      ))))
    ))

    val core = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), Seq(), Seq(Body(Seq(
        Assign(Seq("case"), Constant(IntLiteral(7)))
      ))))
    ))

    assertDesugar(core, sugared)
  }

  "desugaring" should "eliminate var pattern" in {
    val sugared = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), Seq(), Seq(Body(Seq(
        Match(one, Seq(
          Case(VarPattern("n"), Body(
            Assign(Seq("case"), Constant(IntLiteral(1)))
          ))
        ))
      ))))
    ))

    val core = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), Seq(), Seq(Body(Seq(
        Assign(Seq("n"), one),
        Assign(Seq("case"), Constant(IntLiteral(1)))
      ))))
    ))

    assertDesugar(core, sugared)
  }

  "desugaring" should "eliminate node pattern" in {
    val sugared = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), Seq(), Seq(Body(Seq(
        Match(one, Seq(
          Case(NodePattern(TNode("Decl"), Seq(PatternBinding("name", VarPattern("n")).typed(TString))), Body(
            Assign(Seq("case"), Constant(IntLiteral(2)))
          ))
        ))
      ))))
    ))

    val core = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), Seq(), Seq(Body(Seq(
        Assign(Seq("matchee"), one),
        Assert(InstanceOf(Var("matchee"), TNode("Decl"))),
        Assign(Seq("n"), PathAccess(Var("matchee"), NamedLink(TNode("Decl"), "name")).typed(TString)),
        Assign(Seq("case"), Constant(IntLiteral(2)))
      ))))
    ))

    assertDesugar(core, sugared)
  }

  "desugaring" should "eliminate node pattern wildcard" in {
    val sugared = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), Seq(), Seq(Body(Seq(
        Match(one, Seq(
          Case(NodePattern(TNode("Decl"), Seq(PatternBinding("name", WildcardPattern).typed(TString))), Body(
            Assign(Seq("case"), Constant(IntLiteral(3)))
          ))
        ))
      ))))
    ))

    val core = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), Seq(), Seq(Body(Seq(
        Assign(Seq("matchee"), one),
        Assert(InstanceOf(Var("matchee"), TNode("Decl"))),
        Assign(Seq("case"), Constant(IntLiteral(3)))
      ))))
    ))

    assertDesugar(core, sugared)
  }

  "desugaring" should "eliminate named pattern" in {
    val sugared = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), Seq(), Seq(Body(Seq(
        Match(one, Seq(
          Case(NamedPattern("node", NodePattern(TNode("Decl"), Seq(PatternBinding("name", VarPattern("n")).typed(TString)))), Body(
            Assign(Seq("case"), Constant(IntLiteral(3)))
          ))
        ))
      ))))
    ))

    val core = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), Seq(), Seq(Body(Seq(
        Assign(Seq("node"), one),
        Assign(Seq("matchee"), one),
        Assert(InstanceOf(Var("matchee"), TNode("Decl"))),
        Assign(Seq("n"), PathAccess(Var("matchee"), NamedLink(TNode("Decl"), "name")).typed(TString)),
        Assign(Seq("case"), Constant(IntLiteral(3)))
      ))))
    ))

    assertDesugar(core, sugared)
  }

  "desugaring" should "eliminate tuple pattern" in {
    val sugared = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), Seq(), Seq(Body(Seq(
        Match(one, Seq(
          Case(TuplePattern(Seq(VarPattern("x1"), VarPattern("x2"), VarPattern("x3"))), Body(
            Assign(Seq("case"), Constant(IntLiteral(4)))
          ))
        ))
      ))))
    ))

    val core = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), Seq(), Seq(Body(Seq(
        Assign(Seq("matchee_tuple0", "matchee_tuple1", "matchee_tuple2"), one),
        Assign(Seq("x1"), Var("matchee_tuple0")),
        Assign(Seq("x2"), Var("matchee_tuple1")),
        Assign(Seq("x3"), Var("matchee_tuple2")),
        Assign(Seq("case"), Constant(IntLiteral(4)))
      ))))
    ))

    assertDesugar(core, sugared)
  }

  "desugaring" should "eliminate literal pattern" in {
    val sugared = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), Seq(), Seq(Body(Seq(
        Match(one, Seq(
          Case(LiteralPattern(StringLiteral("abc")), Body(
            Assign(Seq("case"), Constant(IntLiteral(5)))
          ))
        ))
      ))))
    ))

    val core = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), Seq(), Seq(Body(Seq(
        Assert(Eq(one, Constant(StringLiteral("abc")))),
        Assign(Seq("case"), Constant(IntLiteral(5)))
      ))))
    ))

    assertDesugar(core, sugared)
  }

  "desugaring" should "eliminate two literal cases" in {
    val sugared = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), Seq(), Seq(Body(Seq(
        Match(one, Seq(
          Case(LiteralPattern(StringLiteral("abc")), Body(
            Assign(Seq("case"), Constant(IntLiteral(1)))
          )),
          Case(LiteralPattern(StringLiteral("def")), Body(
            Assign(Seq("case"), Constant(IntLiteral(2)))
          ))
        ))
      ))))
    ))

    val core = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), Seq(), Seq(
        Body(Seq(
          Assert(Eq(one, Constant(StringLiteral("abc")))),
          Assign(Seq("case"), Constant(IntLiteral(1)))
        )),
        Body(Seq(
          Assert(Neq(one, Constant(StringLiteral("abc")))),
          Assert(Eq(one, Constant(StringLiteral("def")))),
          Assign(Seq("case"), Constant(IntLiteral(2)))
        ))))
    ))

    assertDesugar(core, sugared)
  }

  "desugaring" should "eliminate three literal cases" in {
    val sugared = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), Seq(), Seq(Body(Seq(
        Match(one, Seq(
          Case(LiteralPattern(StringLiteral("abc")), Body(
            Assign(Seq("case"), Constant(IntLiteral(1)))
          )),
          Case(LiteralPattern(StringLiteral("def")), Body(
            Assign(Seq("case"), Constant(IntLiteral(2)))
          )),
          Case(LiteralPattern(StringLiteral("ghi")), Body(
            Assign(Seq("case"), Constant(IntLiteral(3)))
          ))
        ))
      ))))
    ))

    val core = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), Seq(), Seq(
        Body(Seq(
          Assert(Eq(one, Constant(StringLiteral("abc")))),
          Assign(Seq("case"), Constant(IntLiteral(1)))
        )),
        Body(Seq(
          Assert(Neq(one, Constant(StringLiteral("abc")))),
          Assert(Eq(one, Constant(StringLiteral("def")))),
          Assign(Seq("case"), Constant(IntLiteral(2)))
        )),
        Body(Seq(
          Assert(Neq(one, Constant(StringLiteral("abc")))),
          Assert(Neq(one, Constant(StringLiteral("def")))),
          Assert(Eq(one, Constant(StringLiteral("ghi")))),
          Assign(Seq("case"), Constant(IntLiteral(3)))
        ))
      ))
    ))

    assertDesugar(core, sugared)
  }

  "desugaring" should "eliminate two tuple cases" in {
    val sugared = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), Seq(), Seq(Body(Seq(
        Match(one, Seq(
          Case(TuplePattern(Seq(LiteralPattern(StringLiteral("abc")), LiteralPattern(StringLiteral("abc")))), Body(
            Assign(Seq("case"), Constant(IntLiteral(1)))
          )),
          Case(TuplePattern(Seq(LiteralPattern(StringLiteral("def")), LiteralPattern(StringLiteral("def")))), Body(
            Assign(Seq("case"), Constant(IntLiteral(2)))
          ))
        ))
      ))))
    ))

    val core = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), Seq(), Seq(
        Body(Seq(
          Assign(Seq("matchee_tuple0", "matchee_tuple1"), one),
          Assert(Eq(Var("matchee_tuple0"), Constant(StringLiteral("abc")))),
          Assert(Eq(Var("matchee_tuple1"), Constant(StringLiteral("abc")))),
          Assign(Seq("case"), Constant(IntLiteral(1)))
        )),
        Body(Seq(
          Assign(Seq("matchee_tuple0_0", "matchee_tuple1_0"), one),
          Assert(Neq(Var("matchee_tuple0_0"), Constant(StringLiteral("abc")))),
          Assign(Seq("matchee_tuple0_1", "matchee_tuple1_1"), one),
          Assert(Eq(Var("matchee_tuple0_1"), Constant(StringLiteral("def")))),
          Assert(Eq(Var("matchee_tuple1_1"), Constant(StringLiteral("def")))),
          Assign(Seq("case"), Constant(IntLiteral(2)))
        )),
        Body(Seq(
          Assign(Seq("matchee_tuple0_0", "matchee_tuple1_0"), one),
          Assert(Neq(Var("matchee_tuple1_0"), Constant(StringLiteral("abc")))),
          Assign(Seq("matchee_tuple0_1", "matchee_tuple1_1"), one),
          Assert(Eq(Var("matchee_tuple0_1"), Constant(StringLiteral("def")))),
          Assert(Eq(Var("matchee_tuple1_1"), Constant(StringLiteral("def")))),
          Assign(Seq("case"), Constant(IntLiteral(2)))
        ))))
    ))

    assertDesugar(core, sugared)
  }

  "desugaring" should "eliminate three tuple cases" in {
    val sugared = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), Seq(), Seq(Body(Seq(
        Match(one, Seq(
          Case(TuplePattern(Seq(LiteralPattern(StringLiteral("abc")), LiteralPattern(StringLiteral("abc")))), Body(
            Assign(Seq("case"), Constant(IntLiteral(1)))
          )),
          Case(TuplePattern(Seq(LiteralPattern(StringLiteral("def")), LiteralPattern(StringLiteral("def")))), Body(
            Assign(Seq("case"), Constant(IntLiteral(2)))
          )),
          Case(TuplePattern(Seq(LiteralPattern(StringLiteral("ghi")), LiteralPattern(StringLiteral("ghi")))), Body(
            Assign(Seq("case"), Constant(IntLiteral(3)))
          ))
        ))
      ))))
    ))

    val core = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), Seq(), Seq(
        Body(Seq(
          Assign(Seq("matchee_tuple0", "matchee_tuple1"), one),
          Assert(Eq(Var("matchee_tuple0"), Constant(StringLiteral("abc")))),
          Assert(Eq(Var("matchee_tuple1"), Constant(StringLiteral("abc")))),
          Assign(Seq("case"), Constant(IntLiteral(1)))
        )),
        Body(Seq(
          Assign(Seq("matchee_tuple0_0", "matchee_tuple1_0"), one),
          Assert(Neq(Var("matchee_tuple0_0"), Constant(StringLiteral("abc")))),
          Assign(Seq("matchee_tuple0_1", "matchee_tuple1_1"), one),
          Assert(Eq(Var("matchee_tuple0_1"), Constant(StringLiteral("def")))),
          Assert(Eq(Var("matchee_tuple1_1"), Constant(StringLiteral("def")))),
          Assign(Seq("case"), Constant(IntLiteral(2)))
        )),
        Body(Seq(
          Assign(Seq("matchee_tuple0_0", "matchee_tuple1_0"), one),
          Assert(Neq(Var("matchee_tuple1_0"), Constant(StringLiteral("abc")))),
          Assign(Seq("matchee_tuple0_1", "matchee_tuple1_1"), one),
          Assert(Eq(Var("matchee_tuple0_1"), Constant(StringLiteral("def")))),
          Assert(Eq(Var("matchee_tuple1_1"), Constant(StringLiteral("def")))),
          Assign(Seq("case"), Constant(IntLiteral(2)))
        )),
        Body(Seq(
          Assign(Seq("matchee_tuple0_0", "matchee_tuple1_0"), one),
          Assert(Neq(Var("matchee_tuple0_0"), Constant(StringLiteral("abc")))),
          Assign(Seq("matchee_tuple0_2", "matchee_tuple1_2"), one),
          Assert(Neq(Var("matchee_tuple0_2"), Constant(StringLiteral("def")))),
          Assign(Seq("matchee_tuple0_3", "matchee_tuple1_3"), one),
          Assert(Eq(Var("matchee_tuple0_3"), Constant(StringLiteral("ghi")))),
          Assert(Eq(Var("matchee_tuple1_3"), Constant(StringLiteral("ghi")))),
          Assign(Seq("case"), Constant(IntLiteral(3)))
        )),
        Body(Seq(
          Assign(Seq("matchee_tuple0_0", "matchee_tuple1_0"), one),
          Assert(Neq(Var("matchee_tuple0_0"), Constant(StringLiteral("abc")))),
          Assign(Seq("matchee_tuple0_2", "matchee_tuple1_2"), one),
          Assert(Neq(Var("matchee_tuple1_2"), Constant(StringLiteral("def")))),
          Assign(Seq("matchee_tuple0_3", "matchee_tuple1_3"), one),
          Assert(Eq(Var("matchee_tuple0_3"), Constant(StringLiteral("ghi")))),
          Assert(Eq(Var("matchee_tuple1_3"), Constant(StringLiteral("ghi")))),
          Assign(Seq("case"), Constant(IntLiteral(3)))
        )),
        Body(Seq(
          Assign(Seq("matchee_tuple0_0", "matchee_tuple1_0"), one),
          Assert(Neq(Var("matchee_tuple1_0"), Constant(StringLiteral("abc")))),
          Assign(Seq("matchee_tuple0_2", "matchee_tuple1_2"), one),
          Assert(Neq(Var("matchee_tuple0_2"), Constant(StringLiteral("def")))),
          Assign(Seq("matchee_tuple0_3", "matchee_tuple1_3"), one),
          Assert(Eq(Var("matchee_tuple0_3"), Constant(StringLiteral("ghi")))),
          Assert(Eq(Var("matchee_tuple1_3"), Constant(StringLiteral("ghi")))),
          Assign(Seq("case"), Constant(IntLiteral(3)))
        )),
        Body(Seq(
          Assign(Seq("matchee_tuple0_0", "matchee_tuple1_0"), one),
          Assert(Neq(Var("matchee_tuple1_0"), Constant(StringLiteral("abc")))),
          Assign(Seq("matchee_tuple0_2", "matchee_tuple1_2"), one),
          Assert(Neq(Var("matchee_tuple1_2"), Constant(StringLiteral("def")))),
          Assign(Seq("matchee_tuple0_3", "matchee_tuple1_3"), one),
          Assert(Eq(Var("matchee_tuple0_3"), Constant(StringLiteral("ghi")))),
          Assert(Eq(Var("matchee_tuple1_3"), Constant(StringLiteral("ghi")))),
          Assign(Seq("case"), Constant(IntLiteral(3)))
        ))
      ))
    ))

    assertDesugar(core, sugared)
  }

  "desugaring" should "eliminate two node cases" in {
    val sugared = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), Seq(), Seq(Body(Seq(
        Match(one, Seq(
          Case(NodePattern(TNode("A"), Seq(PatternBinding("a", LiteralPattern(StringLiteral("abc"))).typed(TString))), Body(
            Assign(Seq("case"), Constant(IntLiteral(1)))
          )),
          Case(NodePattern(TNode("B"), Seq(PatternBinding("b", LiteralPattern(StringLiteral("def"))).typed(TString))), Body(
            Assign(Seq("case"), Constant(IntLiteral(2)))
          ))
        ))
      ))))
    ))

    val core = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), Seq(), Seq(
        Body(Seq(
          Assign(Seq("matchee"), one),
          Assert(InstanceOf(Var("matchee"), TNode("A"))),
          Assert(Eq(PathAccess(Var("matchee"), NamedLink(TNode("A"), "a")).typed(TString), Constant(StringLiteral("abc")))),
          Assign(Seq("case"), Constant(IntLiteral(1)))
        )),
        Body(Seq(
          Assign(Seq("matchee_0"), one),
          Assert(NotInstanceOf(Var("matchee_0"), TNode("A"))),
          Assign(Seq("matchee_1"), one),
          Assert(InstanceOf(Var("matchee_1"), TNode("B"))),
          Assert(Eq(PathAccess(Var("matchee_1"), NamedLink(TNode("B"), "b")).typed(TString), Constant(StringLiteral("def")))),
          Assign(Seq("case"), Constant(IntLiteral(2)))
        )),
        Body(Seq(
          Assign(Seq("matchee_0"), one),
          Assert(InstanceOf(Var("matchee_0"), TNode("A"))),
          Assert(Neq(PathAccess(Var("matchee_0"), NamedLink(TNode("A"), "a")).typed(TString), Constant(StringLiteral("abc")))),
          Assign(Seq("matchee_1"), one),
          Assert(InstanceOf(Var("matchee_1"), TNode("B"))),
          Assert(Eq(PathAccess(Var("matchee_1"), NamedLink(TNode("B"), "b")).typed(TString), Constant(StringLiteral("def")))),
          Assign(Seq("case"), Constant(IntLiteral(2)))
        ))))
    ))

    assertDesugar(core, sugared)
  }

  "desugaring" should "eliminate three node cases" in {
    val sugared = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), Seq(), Seq(Body(Seq(
        Match(one, Seq(
          Case(NodePattern(TNode("A"), Seq(PatternBinding("a", LiteralPattern(StringLiteral("abc"))).typed(TString))), Body(
            Assign(Seq("case"), Constant(IntLiteral(1)))
          )),
          Case(NodePattern(TNode("B"), Seq(PatternBinding("b", LiteralPattern(StringLiteral("def"))).typed(TString))), Body(
            Assign(Seq("case"), Constant(IntLiteral(2)))
          )),
          Case(NodePattern(TNode("C"), Seq(PatternBinding("c", LiteralPattern(StringLiteral("ghi"))).typed(TString))), Body(
            Assign(Seq("case"), Constant(IntLiteral(3)))
          ))
        ))
      ))))
    ))

    val core = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), Seq(), Seq(
        Body(Seq(
          Assign(Seq("matchee"), one),
          Assert(InstanceOf(Var("matchee"), TNode("A"))),
          Assert(Eq(PathAccess(Var("matchee"), NamedLink(TNode("A"), "a")).typed(TString), Constant(StringLiteral("abc")))),
          Assign(Seq("case"), Constant(IntLiteral(1)))
        )),
        Body(Seq(
          Assign(Seq("matchee_0"), one),
          Assert(NotInstanceOf(Var("matchee_0"), TNode("A"))),
          Assign(Seq("matchee_1"), one),
          Assert(InstanceOf(Var("matchee_1"), TNode("B"))),
          Assert(Eq(PathAccess(Var("matchee_1"), NamedLink(TNode("B"), "b")).typed(TString), Constant(StringLiteral("def")))),
          Assign(Seq("case"), Constant(IntLiteral(2)))
        )),
        Body(Seq(
          Assign(Seq("matchee_0"), one),
          Assert(InstanceOf(Var("matchee_0"), TNode("A"))),
          Assert(Neq(PathAccess(Var("matchee_0"), NamedLink(TNode("A"), "a")).typed(TString), Constant(StringLiteral("abc")))),
          Assign(Seq("matchee_1"), one),
          Assert(InstanceOf(Var("matchee_1"), TNode("B"))),
          Assert(Eq(PathAccess(Var("matchee_1"), NamedLink(TNode("B"), "b")).typed(TString), Constant(StringLiteral("def")))),
          Assign(Seq("case"), Constant(IntLiteral(2)))
        )),
        Body(Seq(
          Assign(Seq("matchee_0"), one),
          Assert(NotInstanceOf(Var("matchee_0"), TNode("A"))),
          Assign(Seq("matchee_2"), one),
          Assert(NotInstanceOf(Var("matchee_2"), TNode("B"))),
          Assign(Seq("matchee_3"), one),
          Assert(InstanceOf(Var("matchee_3"), TNode("C"))),
          Assert(Eq(PathAccess(Var("matchee_3"), NamedLink(TNode("C"), "c")).typed(TString), Constant(StringLiteral("ghi")))),
          Assign(Seq("case"), Constant(IntLiteral(3)))
        )),
        Body(Seq(
          Assign(Seq("matchee_0"), one),
          Assert(NotInstanceOf(Var("matchee_0"), TNode("A"))),
          Assign(Seq("matchee_2"), one),
          Assert(InstanceOf(Var("matchee_2"), TNode("B"))),
          Assert(Neq(PathAccess(Var("matchee_2"), NamedLink(TNode("B"), "b")).typed(TString), Constant(StringLiteral("def")))),
          Assign(Seq("matchee_3"), one),
          Assert(InstanceOf(Var("matchee_3"), TNode("C"))),
          Assert(Eq(PathAccess(Var("matchee_3"), NamedLink(TNode("C"), "c")).typed(TString), Constant(StringLiteral("ghi")))),
          Assign(Seq("case"), Constant(IntLiteral(3)))
        )),
        Body(Seq(
          Assign(Seq("matchee_0"), one),
          Assert(InstanceOf(Var("matchee_0"), TNode("A"))),
          Assert(Neq(PathAccess(Var("matchee_0"), NamedLink(TNode("A"), "a")).typed(TString), Constant(StringLiteral("abc")))),
          Assign(Seq("matchee_2"), one),
          Assert(NotInstanceOf(Var("matchee_2"), TNode("B"))),
          Assign(Seq("matchee_3"), one),
          Assert(InstanceOf(Var("matchee_3"), TNode("C"))),
          Assert(Eq(PathAccess(Var("matchee_3"), NamedLink(TNode("C"), "c")).typed(TString), Constant(StringLiteral("ghi")))),
          Assign(Seq("case"), Constant(IntLiteral(3)))
        )),
        Body(Seq(
          Assign(Seq("matchee_0"), one),
          Assert(InstanceOf(Var("matchee_0"), TNode("A"))),
          Assert(Neq(PathAccess(Var("matchee_0"), NamedLink(TNode("A"), "a")).typed(TString), Constant(StringLiteral("abc")))),
          Assign(Seq("matchee_2"), one),
          Assert(InstanceOf(Var("matchee_2"), TNode("B"))),
          Assert(Neq(PathAccess(Var("matchee_2"), NamedLink(TNode("B"), "b")).typed(TString), Constant(StringLiteral("def")))),
          Assign(Seq("matchee_3"), one),
          Assert(InstanceOf(Var("matchee_3"), TNode("C"))),
          Assert(Eq(PathAccess(Var("matchee_3"), NamedLink(TNode("C"), "c")).typed(TString), Constant(StringLiteral("ghi")))),
          Assign(Seq("case"), Constant(IntLiteral(3)))
        ))
      ))
    ))

    assertDesugar(core, sugared)
  }

  "desugaring" should "eliminate var pattern eliminates remaining patterns" in {
    val sugared = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), Seq(), Seq(Body(Seq(
        Match(one, Seq(
          Case(VarPattern("n"), Body(
            Assign(Seq("case"), Constant(IntLiteral(1)))
          )),
          Case(NodePattern(TNode("Decl"), Seq(PatternBinding("name", VarPattern("n")).typed(TString))), Body(
            Assign(Seq("case"), Constant(IntLiteral(2)))
          )),
          Case(NamedPattern("node", NodePattern(TNode("Decl"), Seq(PatternBinding("name", VarPattern("n")).typed(TString)))), Body(
            Assign(Seq("case"), Constant(IntLiteral(3)))
          )),
          Case(TuplePattern(Seq(VarPattern("x1"), VarPattern("x2"), VarPattern("x3"))), Body(
            Assign(Seq("case"), Constant(IntLiteral(4)))
          )),
          Case(LiteralPattern(StringLiteral("abc")), Body(
            Assign(Seq("case"), Constant(IntLiteral(5)))
          )),
          Case(NodePattern(TNode("Decl"), Seq(PatternBinding("name", WildcardPattern).typed(TString))), Body(
            Assign(Seq("case"), Constant(IntLiteral(6)))
          )),
          Case(WildcardPattern, Body(
            Assign(Seq("case"), Constant(IntLiteral(7)))
          ))
        ))
      ))))
    ))

    val core = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), Seq(), Seq(Body(Seq(
        Assign(Seq("n"), one),
        Assign(Seq("case"), Constant(IntLiteral(1)))
      ))))
    ))

    assertDesugar(core, sugared)
  }


  "desugaring" should "implement match semantics" in {
    val module = Module("Test_Match", Seq(), Seq(
      PatternFunction(None, "integerlits", Seq(), Seq(AnnoParam(None, TInt)), Seq(Body(Seq(
        Values("root", TNode(Exp.expTag)),
        Assert(Undef(PathAccess(Var("root").typed(TNode(Exp.expTag)), ParentLink).typed(TAnyLinked))),
        Yield(
          Call("integerlits_rec",
            Seq(Var("root")),
            transitive = false, count = false
          )
        )
      )))),

      PatternFunction(None, "integerlits_rec", Seq(Param("e", TNode(Exp.expTag))), Seq(AnnoParam(None, TInt)), Seq(Body(Seq(
        Match(Var("e"), Seq(
          Case(
            NodePattern(TNode(Exp.intTag), Seq(PatternBinding("value", VarPattern("v")).typed(TInt))),
            Body(Yield(Var("v")))),
          Case(
            NodePattern(TNode(Exp.addTag), Seq(PatternBinding("lhs", VarPattern("e1")).typed(TNode(Exp.expTag)))),
            Body(Yield(Call("integerlits_rec",
              Seq(Var("e1")),
              transitive = false, count = false)))),
          Case(
            NodePattern(TNode(Exp.multTag), Seq(PatternBinding("rhs", VarPattern("e1")).typed(TNode(Exp.expTag)))),
            Body(Yield(Call("integerlits_rec",
              Seq(Var("e1")),
              transitive = false, count = false))))
        ))
      ))))
    ))

    val input = {
      import Exp._
      Add(
        Mul(
          IntegerLit(1),
          IntegerLit(2)
        ),
        Many(
          List(
            IntegerLit(3),
            IntegerLit(4),
            IntegerLit(5)
          )
        )
      )
    }

    assertMatch(module, "generated_helper_undefpath_ParentLink", input) { matcher =>
      assert(matcher.getAllMatches.size() == 7)
    }

    assertMatch(module, "integerlits_rec", input) { matcher =>
      assert(matcher.getAllMatches.size() == 7)
    }

    assertMatch(module, "integerlits", input) { matcher =>
      assert(matcher.getAllMatches.size() == 1)
      matcher.getAllMatchArrays should contain theSameElementsAs Seq(Array(2))
    }
  }
}
