package inca.lang.funext

import inca.IncaMatchers
import inca.lang.fun.Fun._
import org.scalatest.flatspec.AnyFlatSpec

class TestMatchDesugar extends AnyFlatSpec with IncaMatchers {

  val one = Constant(IntLiteral(1))
  val two = Constant(IntLiteral(2))

  "desugaring" should "eliminate wildcard pattern" in {
    val sugared = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), Seq(), Seq(Body(Seq(
        Match(one, Seq(
          Case(WildcardPattern, Seq(
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

    assertDesugar(core, sugared, Match)
  }

  "desugaring" should "eliminate var pattern" in {
    val sugared = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), Seq(), Seq(Body(Seq(
        Match(one, Seq(
          Case(VarPattern("n"), Seq(
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

    assertDesugar(core, sugared, Match)
  }

  "desugaring" should "eliminate node pattern" in {
    val sugared = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), Seq(), Seq(Body(Seq(
        Match(one, Seq(
          Case(NodePattern(TNode("Decl"), Seq(PatternBinding("name", VarPattern("n")).typed(TString))), Seq(
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

    assertDesugar(core, sugared, Match)
  }

  "desugaring" should "eliminate node pattern wildcard" in {
    val sugared = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), Seq(), Seq(Body(Seq(
        Match(one, Seq(
          Case(NodePattern(TNode("Decl"), Seq(PatternBinding("name", WildcardPattern).typed(TString))), Seq(
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

    assertDesugar(core, sugared, Match)
  }

  "desugaring" should "eliminate named pattern" in {
    val sugared = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), Seq(), Seq(Body(Seq(
        Match(one, Seq(
          Case(NamedPattern("node", NodePattern(TNode("Decl"), Seq(PatternBinding("name", VarPattern("n")).typed(TString)))), Seq(
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

    assertDesugar(core, sugared, Match)
  }

  "desugaring" should "eliminate tuple pattern" in {
    val sugared = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), Seq(), Seq(Body(Seq(
        Match(one, Seq(
          Case(TuplePattern(Seq(VarPattern("x1"), VarPattern("x2"), VarPattern("x3"))), Seq(
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

    assertDesugar(core, sugared, Match)
  }

  "desugaring" should "eliminate literal pattern" in {
    val sugared = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), Seq(), Seq(Body(Seq(
        Match(one, Seq(
          Case(LiteralPattern(StringLiteral("abc")), Seq(
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

    assertDesugar(core, sugared, Match)
  }

  "desugaring" should "eliminate two literal cases" in {
    val sugared = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), Seq(), Seq(Body(Seq(
        Match(one, Seq(
          Case(LiteralPattern(StringLiteral("abc")), Seq(
            Assign(Seq("case"), Constant(IntLiteral(1)))
          )),
          Case(LiteralPattern(StringLiteral("def")), Seq(
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

    assertDesugar(core, sugared, Match)
  }

  "desugaring" should "eliminate three literal cases" in {
    val sugared = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), Seq(), Seq(Body(Seq(
        Match(one, Seq(
          Case(LiteralPattern(StringLiteral("abc")), Seq(
            Assign(Seq("case"), Constant(IntLiteral(1)))
          )),
          Case(LiteralPattern(StringLiteral("def")), Seq(
            Assign(Seq("case"), Constant(IntLiteral(2)))
          )),
          Case(LiteralPattern(StringLiteral("ghi")), Seq(
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

    assertDesugar(core, sugared, Match)
  }

  "desugaring" should "eliminate two tuple cases" in {
    val sugared = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), Seq(), Seq(Body(Seq(
        Match(one, Seq(
          Case(TuplePattern(Seq(LiteralPattern(StringLiteral("abc")), LiteralPattern(StringLiteral("abc")))), Seq(
            Assign(Seq("case"), Constant(IntLiteral(1)))
          )),
          Case(TuplePattern(Seq(LiteralPattern(StringLiteral("def")), LiteralPattern(StringLiteral("def")))), Seq(
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

    assertDesugar(core, sugared, Match)
  }

  "desugaring" should "eliminate three tuple cases" in {
    val sugared = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), Seq(), Seq(Body(Seq(
        Match(one, Seq(
          Case(TuplePattern(Seq(LiteralPattern(StringLiteral("abc")), LiteralPattern(StringLiteral("abc")))), Seq(
            Assign(Seq("case"), Constant(IntLiteral(1)))
          )),
          Case(TuplePattern(Seq(LiteralPattern(StringLiteral("def")), LiteralPattern(StringLiteral("def")))), Seq(
            Assign(Seq("case"), Constant(IntLiteral(2)))
          )),
          Case(TuplePattern(Seq(LiteralPattern(StringLiteral("ghi")), LiteralPattern(StringLiteral("ghi")))), Seq(
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

    assertDesugar(core, sugared, Match)
  }

  "desugaring" should "eliminate two node cases" in {
    val sugared = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), Seq(), Seq(Body(Seq(
        Match(one, Seq(
          Case(NodePattern(TNode("A"), Seq(PatternBinding("a", LiteralPattern(StringLiteral("abc"))).typed(TString))), Seq(
            Assign(Seq("case"), Constant(IntLiteral(1)))
          )),
          Case(NodePattern(TNode("B"), Seq(PatternBinding("b", LiteralPattern(StringLiteral("def"))).typed(TString))), Seq(
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

    assertDesugar(core, sugared, Match)
  }

  "desugaring" should "eliminate three node cases" in {
    val sugared = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), Seq(), Seq(Body(Seq(
        Match(one, Seq(
          Case(NodePattern(TNode("A"), Seq(PatternBinding("a", LiteralPattern(StringLiteral("abc"))).typed(TString))), Seq(
            Assign(Seq("case"), Constant(IntLiteral(1)))
          )),
          Case(NodePattern(TNode("B"), Seq(PatternBinding("b", LiteralPattern(StringLiteral("def"))).typed(TString))), Seq(
            Assign(Seq("case"), Constant(IntLiteral(2)))
          )),
          Case(NodePattern(TNode("C"), Seq(PatternBinding("c", LiteralPattern(StringLiteral("ghi"))).typed(TString))), Seq(
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

    assertDesugar(core, sugared, Match)
  }

  "desugaring" should "eliminate var pattern eliminates remaining patterns" in {
    val sugared = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), Seq(), Seq(Body(Seq(
        Match(one, Seq(
          Case(VarPattern("n"), Seq(
            Assign(Seq("case"), Constant(IntLiteral(1)))
          )),
          Case(NodePattern(TNode("Decl"), Seq(PatternBinding("name", VarPattern("n")).typed(TString))), Seq(
            Assign(Seq("case"), Constant(IntLiteral(2)))
          )),
          Case(NamedPattern("node", NodePattern(TNode("Decl"), Seq(PatternBinding("name", VarPattern("n")).typed(TString)))), Seq(
            Assign(Seq("case"), Constant(IntLiteral(3)))
          )),
          Case(TuplePattern(Seq(VarPattern("x1"), VarPattern("x2"), VarPattern("x3"))), Seq(
            Assign(Seq("case"), Constant(IntLiteral(4)))
          )),
          Case(LiteralPattern(StringLiteral("abc")), Seq(
            Assign(Seq("case"), Constant(IntLiteral(5)))
          )),
          Case(NodePattern(TNode("Decl"), Seq(PatternBinding("name", WildcardPattern).typed(TString))), Seq(
            Assign(Seq("case"), Constant(IntLiteral(6)))
          )),
          Case(WildcardPattern, Seq(
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

    assertDesugar(core, sugared, Match)
  }

}
