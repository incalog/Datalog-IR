package inca.frontend.extensions

import inca.IncaMatchers
import inca.analyzedLangs.Exp
import inca.compiler.{CompilerFrontend, Options}
import inca.frontend.core.Trees
import inca.frontend.core.tree._
import inca.frontend.extensions.match_.Trees._
import inca.runtime.context.{LanguageMetaInfo, QueryScope}
import org.scalatest.flatspec.AnyFlatSpec

import scala.language.implicitConversions

class TestMatch extends AnyFlatSpec with IncaMatchers {

  implicit def name(s: String): Name = Name(s)

  val one = Constant(IntLiteral(1))
  val two = Constant(IntLiteral(2))

  val scope: QueryScope = new QueryScope(Exp.languageMetaInfo)
  val options: Options = Options(scope.langMetaInfo, info => new CompilerFrontend with match_.Frontend {
    override val lang: LanguageMetaInfo = info
    override val syntax = new Trees with match_.Trees
  })

  "desugaring" should "eliminate wildcard pattern" in {
    val sugared = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), TUnit, Seq(Body(Seq(
        Match(one, Seq(
          Case(WildcardPattern, Body(
            Assign(Seq("case"), Constant(IntLiteral(7)))
          ))
        ))
      ))))
    ))

    val core = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), TUnit, Seq(Body(Seq(
        Assign(Seq("case"), Constant(IntLiteral(7)))
      ))))
    ))

    assertDesugar(core, sugared)
  }

  "desugaring" should "eliminate var pattern" in {
    val sugared = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), TUnit, Seq(Body(Seq(
        Match(one, Seq(
          Case(VarPattern("n"), Body(
            Assign(Seq("case"), Constant(IntLiteral(1)))
          ))
        ))
      ))))
    ))

    val core = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), TUnit, Seq(Body(Seq(
        Assign(Seq("n"), one),
        Assign(Seq("case"), Constant(IntLiteral(1)))
      ))))
    ))

    assertDesugar(core, sugared)
  }

  "desugaring" should "eliminate node pattern" in {
    val sugared = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(Param("e", TNode(Exp.expTag))), TUnit, Seq(Body(Seq(
        Match(Var("e"), Seq(
          Case(NodePattern(TNode(Exp.letTag), Seq(PatternBinding("name", VarPattern("n")))), Body(
            Assign(Seq("case"), Constant(IntLiteral(2))),
            Yield(Constant(UnitLiteral))
          ))
        ))
      ))))
    ))

    val core = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(Param("e", TNode(Exp.expTag))), TUnit, Seq(Body(Seq(
        Assign(Seq("matchee"), Cast(Var("e"), TNode(Exp.letTag))),
        Assign(Seq("n"), PathAccess(Var("matchee"), NamedLink("name"))),
        Assign(Seq("case"), Constant(IntLiteral(2))),
        Yield(Constant(UnitLiteral))
      ))))
    ))

    assertDesugar(core, sugared)
  }

  "desugaring" should "eliminate node pattern wildcard" in {
    val sugared = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(Param("e", TNode(Exp.expTag))), TUnit, Seq(Body(Seq(
        Match(Var("e"), Seq(
          Case(NodePattern(TNode(Exp.letTag), Seq(PatternBinding("name", WildcardPattern))), Body(
            Assign(Seq("case"), Constant(IntLiteral(3)))
          ))
        ))
      ))))
    ))

    val core = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(Param("e", TNode(Exp.expTag))), TUnit, Seq(Body(Seq(
        Assign(Seq("matchee"), Cast(Var("e"), TNode(Exp.letTag))),
        Assign(Seq("case"), Constant(IntLiteral(3)))
      ))))
    ))

    assertDesugar(core, sugared)
  }

  "desugaring" should "eliminate named pattern" in {
    val sugared = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(Param("e", TNode(Exp.expTag))), TUnit, Seq(Body(Seq(
        Match(Var("e"), Seq(
          Case(NamedPattern("node", NodePattern(TNode(Exp.letTag), Seq(PatternBinding("name", VarPattern("n"))))), Body(
            Assign(Seq("case"), Constant(IntLiteral(3)))
          ))
        ))
      ))))
    ))

    val core = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(Param("e", TNode(Exp.expTag))), TUnit, Seq(Body(Seq(
        Assign(Seq("node"), Var("e")),
        Assign(Seq("matchee"), Cast(Var("e"), TNode(Exp.letTag))),
        Assign(Seq("n"), PathAccess(Var("matchee"), NamedLink("name"))),
        Assign(Seq("case"), Constant(IntLiteral(3)))
      ))))
    ))

    assertDesugar(core, sugared)
  }

  "desugaring" should "eliminate tuple pattern" in {
    val sugared = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(Param("e", TTuple(Seq(TNode(Exp.expTag), TNode(Exp.expTag), TNode(Exp.expTag))))), TUnit, Seq(Body(Seq(
        Match(Var("e"), Seq(
          Case(TuplePattern(Seq(VarPattern("x1"), VarPattern("x2"), VarPattern("x3"))), Body(
            Assign(Seq("case"), Constant(IntLiteral(4)))
          ))
        ))
      ))))
    ))

    val core = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(Param("e", TTuple(Seq(TNode(Exp.expTag), TNode(Exp.expTag), TNode(Exp.expTag))))), TUnit, Seq(Body(Seq(
        Assign(Seq("matchee_tuple0", "matchee_tuple1", "matchee_tuple2"), Var("e")),
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
      PatternFunction(None, "foo", Seq(), TUnit, Seq(Body(Seq(
        Match(one, Seq(
          Case(LiteralPattern(IntLiteral(123)), Body(
            Assign(Seq("case"), Constant(IntLiteral(5)))
          ))
        ))
      ))))
    ))

    val core = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), TUnit, Seq(Body(Seq(
        Assert(Eq(one, Constant(IntLiteral(123)))),
        Assign(Seq("case"), Constant(IntLiteral(5)))
      ))))
    ))

    assertDesugar(core, sugared)
  }

  "desugaring" should "eliminate two literal cases" in {
    val sugared = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), TUnit, Seq(Body(Seq(
        Match(Constant(StringLiteral("0")), Seq(
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
      PatternFunction(None, "foo", Seq(), TUnit, Seq(
        Body(Seq(
          Assert(Eq(Constant(StringLiteral("0")), Constant(StringLiteral("abc")))),
          Assign(Seq("case"), Constant(IntLiteral(1)))
        )),
        Body(Seq(
          Assert(Neq(Constant(StringLiteral("0")), Constant(StringLiteral("abc")))),
          Assert(Eq(Constant(StringLiteral("0")), Constant(StringLiteral("def")))),
          Assign(Seq("case"), Constant(IntLiteral(2)))
        ))))
    ))

    assertDesugar(core, sugared)
  }

  "desugaring" should "eliminate three literal cases" in {
    val sugared = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), TUnit, Seq(Body(Seq(
        Match(Constant(StringLiteral("0")), Seq(
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
      PatternFunction(None, "foo", Seq(), TUnit, Seq(
        Body(Seq(
          Assert(Eq(Constant(StringLiteral("0")), Constant(StringLiteral("abc")))),
          Assign(Seq("case"), Constant(IntLiteral(1)))
        )),
        Body(Seq(
          Assert(Neq(Constant(StringLiteral("0")), Constant(StringLiteral("abc")))),
          Assert(Eq(Constant(StringLiteral("0")), Constant(StringLiteral("def")))),
          Assign(Seq("case"), Constant(IntLiteral(2)))
        )),
        Body(Seq(
          Assert(Neq(Constant(StringLiteral("0")), Constant(StringLiteral("abc")))),
          Assert(Neq(Constant(StringLiteral("0")), Constant(StringLiteral("def")))),
          Assert(Eq(Constant(StringLiteral("0")), Constant(StringLiteral("ghi")))),
          Assign(Seq("case"), Constant(IntLiteral(3)))
        ))
      ))
    ))

    assertDesugar(core, sugared)
  }

  "desugaring" should "eliminate two tuple cases" in {
    val sugared = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), TUnit, Seq(Body(Seq(
        Match(Tuple(Seq(Constant(StringLiteral("0")), Constant(StringLiteral("0")))), Seq(
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
      PatternFunction(None, "foo", Seq(), TUnit, Seq(
        Body(Seq(
          Assign(Seq("matchee_tuple0", "matchee_tuple1"), Tuple(Seq(Constant(StringLiteral("0")), Constant(StringLiteral("0"))))),
          Assert(Eq(Var("matchee_tuple0"), Constant(StringLiteral("abc")))),
          Assert(Eq(Var("matchee_tuple1"), Constant(StringLiteral("abc")))),
          Assign(Seq("case"), Constant(IntLiteral(1)))
        )),
        Body(Seq(
          Assign(Seq("matchee_tuple0_0", "matchee_tuple1_0"), Tuple(Seq(Constant(StringLiteral("0")), Constant(StringLiteral("0"))))),
          Assert(Neq(Var("matchee_tuple0_0"), Constant(StringLiteral("abc")))),
          Assign(Seq("matchee_tuple0_1", "matchee_tuple1_1"), Tuple(Seq(Constant(StringLiteral("0")), Constant(StringLiteral("0"))))),
          Assert(Eq(Var("matchee_tuple0_1"), Constant(StringLiteral("def")))),
          Assert(Eq(Var("matchee_tuple1_1"), Constant(StringLiteral("def")))),
          Assign(Seq("case"), Constant(IntLiteral(2)))
        )),
        Body(Seq(
          Assign(Seq("matchee_tuple0_0", "matchee_tuple1_0"), Tuple(Seq(Constant(StringLiteral("0")), Constant(StringLiteral("0"))))),
          Assert(Neq(Var("matchee_tuple1_0"), Constant(StringLiteral("abc")))),
          Assign(Seq("matchee_tuple0_1", "matchee_tuple1_1"), Tuple(Seq(Constant(StringLiteral("0")), Constant(StringLiteral("0"))))),
          Assert(Eq(Var("matchee_tuple0_1"), Constant(StringLiteral("def")))),
          Assert(Eq(Var("matchee_tuple1_1"), Constant(StringLiteral("def")))),
          Assign(Seq("case"), Constant(IntLiteral(2)))
        ))))
    ))

    assertDesugar(core, sugared)
  }

  "desugaring" should "eliminate three tuple cases" in {
    val sugared = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), TUnit, Seq(Body(Seq(
        Match(Tuple(Seq(Constant(StringLiteral("0")), Constant(StringLiteral("0")))), Seq(
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
      PatternFunction(None, "foo", Seq(), TUnit, Seq(
        Body(Seq(
          Assign(Seq("matchee_tuple0", "matchee_tuple1"), Tuple(Seq(Constant(StringLiteral("0")), Constant(StringLiteral("0"))))),
          Assert(Eq(Var("matchee_tuple0"), Constant(StringLiteral("abc")))),
          Assert(Eq(Var("matchee_tuple1"), Constant(StringLiteral("abc")))),
          Assign(Seq("case"), Constant(IntLiteral(1)))
        )),
        Body(Seq(
          Assign(Seq("matchee_tuple0_0", "matchee_tuple1_0"), Tuple(Seq(Constant(StringLiteral("0")), Constant(StringLiteral("0"))))),
          Assert(Neq(Var("matchee_tuple0_0"), Constant(StringLiteral("abc")))),
          Assign(Seq("matchee_tuple0_1", "matchee_tuple1_1"), Tuple(Seq(Constant(StringLiteral("0")), Constant(StringLiteral("0"))))),
          Assert(Eq(Var("matchee_tuple0_1"), Constant(StringLiteral("def")))),
          Assert(Eq(Var("matchee_tuple1_1"), Constant(StringLiteral("def")))),
          Assign(Seq("case"), Constant(IntLiteral(2)))
        )),
        Body(Seq(
          Assign(Seq("matchee_tuple0_0", "matchee_tuple1_0"), Tuple(Seq(Constant(StringLiteral("0")), Constant(StringLiteral("0"))))),
          Assert(Neq(Var("matchee_tuple1_0"), Constant(StringLiteral("abc")))),
          Assign(Seq("matchee_tuple0_1", "matchee_tuple1_1"), Tuple(Seq(Constant(StringLiteral("0")), Constant(StringLiteral("0"))))),
          Assert(Eq(Var("matchee_tuple0_1"), Constant(StringLiteral("def")))),
          Assert(Eq(Var("matchee_tuple1_1"), Constant(StringLiteral("def")))),
          Assign(Seq("case"), Constant(IntLiteral(2)))
        )),
        Body(Seq(
          Assign(Seq("matchee_tuple0_0", "matchee_tuple1_0"), Tuple(Seq(Constant(StringLiteral("0")), Constant(StringLiteral("0"))))),
          Assert(Neq(Var("matchee_tuple0_0"), Constant(StringLiteral("abc")))),
          Assign(Seq("matchee_tuple0_2", "matchee_tuple1_2"), Tuple(Seq(Constant(StringLiteral("0")), Constant(StringLiteral("0"))))),
          Assert(Neq(Var("matchee_tuple0_2"), Constant(StringLiteral("def")))),
          Assign(Seq("matchee_tuple0_3", "matchee_tuple1_3"), Tuple(Seq(Constant(StringLiteral("0")), Constant(StringLiteral("0"))))),
          Assert(Eq(Var("matchee_tuple0_3"), Constant(StringLiteral("ghi")))),
          Assert(Eq(Var("matchee_tuple1_3"), Constant(StringLiteral("ghi")))),
          Assign(Seq("case"), Constant(IntLiteral(3)))
        )),
        Body(Seq(
          Assign(Seq("matchee_tuple0_0", "matchee_tuple1_0"), Tuple(Seq(Constant(StringLiteral("0")), Constant(StringLiteral("0"))))),
          Assert(Neq(Var("matchee_tuple0_0"), Constant(StringLiteral("abc")))),
          Assign(Seq("matchee_tuple0_2", "matchee_tuple1_2"), Tuple(Seq(Constant(StringLiteral("0")), Constant(StringLiteral("0"))))),
          Assert(Neq(Var("matchee_tuple1_2"), Constant(StringLiteral("def")))),
          Assign(Seq("matchee_tuple0_3", "matchee_tuple1_3"), Tuple(Seq(Constant(StringLiteral("0")), Constant(StringLiteral("0"))))),
          Assert(Eq(Var("matchee_tuple0_3"), Constant(StringLiteral("ghi")))),
          Assert(Eq(Var("matchee_tuple1_3"), Constant(StringLiteral("ghi")))),
          Assign(Seq("case"), Constant(IntLiteral(3)))
        )),
        Body(Seq(
          Assign(Seq("matchee_tuple0_0", "matchee_tuple1_0"), Tuple(Seq(Constant(StringLiteral("0")), Constant(StringLiteral("0"))))),
          Assert(Neq(Var("matchee_tuple1_0"), Constant(StringLiteral("abc")))),
          Assign(Seq("matchee_tuple0_2", "matchee_tuple1_2"), Tuple(Seq(Constant(StringLiteral("0")), Constant(StringLiteral("0"))))),
          Assert(Neq(Var("matchee_tuple0_2"), Constant(StringLiteral("def")))),
          Assign(Seq("matchee_tuple0_3", "matchee_tuple1_3"), Tuple(Seq(Constant(StringLiteral("0")), Constant(StringLiteral("0"))))),
          Assert(Eq(Var("matchee_tuple0_3"), Constant(StringLiteral("ghi")))),
          Assert(Eq(Var("matchee_tuple1_3"), Constant(StringLiteral("ghi")))),
          Assign(Seq("case"), Constant(IntLiteral(3)))
        )),
        Body(Seq(
          Assign(Seq("matchee_tuple0_0", "matchee_tuple1_0"), Tuple(Seq(Constant(StringLiteral("0")), Constant(StringLiteral("0"))))),
          Assert(Neq(Var("matchee_tuple1_0"), Constant(StringLiteral("abc")))),
          Assign(Seq("matchee_tuple0_2", "matchee_tuple1_2"), Tuple(Seq(Constant(StringLiteral("0")), Constant(StringLiteral("0"))))),
          Assert(Neq(Var("matchee_tuple1_2"), Constant(StringLiteral("def")))),
          Assign(Seq("matchee_tuple0_3", "matchee_tuple1_3"), Tuple(Seq(Constant(StringLiteral("0")), Constant(StringLiteral("0"))))),
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
      PatternFunction(None, "foo", Seq(Param("e", TNode(Exp.expTag))), TUnit, Seq(Body(Seq(
        Match(Var("e"), Seq(
          Case(NodePattern(TNode(Exp.addTag), Seq(PatternBinding("lhs", LiteralPattern(StringLiteral("abc"))))), Body(
            Assign(Seq("case"), Constant(IntLiteral(1)))
          )),
          Case(NodePattern(TNode(Exp.multTag), Seq(PatternBinding("rhs", LiteralPattern(StringLiteral("def"))))), Body(
            Assign(Seq("case"), Constant(IntLiteral(2)))
          ))
        ))
      ))))
    ))

    val core = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(Param("e", TNode(Exp.expTag))), TUnit, Seq(
        Body(Seq(
          Assign(Seq("matchee"), Cast(Var("e"), TNode(Exp.addTag))),
          Assert(Eq(PathAccess(Var("matchee"), NamedLink("lhs")), Constant(StringLiteral("abc")))),
          Assign(Seq("case"), Constant(IntLiteral(1)))
        )),
        Body(Seq(
          Assert(NotInstanceOf(Var("e"), TNode(Exp.addTag))),
          Assign(Seq("matchee_1"), Cast(Var("e"), TNode(Exp.multTag))),
          Assert(Eq(PathAccess(Var("matchee_1"), NamedLink("rhs")), Constant(StringLiteral("def")))),
          Assign(Seq("case"), Constant(IntLiteral(2)))
        )),
        Body(Seq(
          Assign(Seq("matchee_0"), Cast(Var("e"), TNode(Exp.addTag))),
          Assert(Neq(PathAccess(Var("matchee_0"), NamedLink("lhs")), Constant(StringLiteral("abc")))),
          Assign(Seq("matchee_1"), Cast(Var("e"), TNode(Exp.multTag))),
          Assert(Eq(PathAccess(Var("matchee_1"), NamedLink("rhs")), Constant(StringLiteral("def")))),
          Assign(Seq("case"), Constant(IntLiteral(2)))
        ))))
    ))

    assertDesugar(core, sugared, options.copy(stopOnError = false))
  }

  "desugaring" should "eliminate three node cases" in {
    val sugared = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(Param("e", TNode(Exp.expTag))), TUnit, Seq(Body(Seq(
        Match(Var("e"), Seq(
          Case(NodePattern(TNode(Exp.addTag), Seq(PatternBinding("lhs", LiteralPattern(StringLiteral("abc"))))), Body(
            Assign(Seq("case"), Constant(IntLiteral(1)))
          )),
          Case(NodePattern(TNode(Exp.multTag), Seq(PatternBinding("rhs", LiteralPattern(StringLiteral("def"))))), Body(
            Assign(Seq("case"), Constant(IntLiteral(2)))
          )),
          Case(NodePattern(TNode(Exp.notTag), Seq(PatternBinding("e", LiteralPattern(StringLiteral("ghi"))))), Body(
            Assign(Seq("case"), Constant(IntLiteral(3)))
          ))
        ))
      ))))
    ))

    val core = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(Param("e", TNode(Exp.expTag))), TUnit, Seq(
        Body(Seq(
          Assign(Seq("matchee"), Cast(Var("e"), TNode(Exp.addTag))),
          Assert(Eq(PathAccess(Var("matchee"), NamedLink("lhs")), Constant(StringLiteral("abc")))),
          Assign(Seq("case"), Constant(IntLiteral(1)))
        )),
        Body(Seq(
          Assert(NotInstanceOf(Var("e"), TNode(Exp.addTag))),
          Assign(Seq("matchee_1"), Cast(Var("e"), TNode(Exp.multTag))),
          Assert(Eq(PathAccess(Var("matchee_1"), NamedLink("rhs")), Constant(StringLiteral("def")))),
          Assign(Seq("case"), Constant(IntLiteral(2)))
        )),
        Body(Seq(
          Assign(Seq("matchee_0"), Cast(Var("e"), TNode(Exp.addTag))),
          Assert(Neq(PathAccess(Var("matchee_0"), NamedLink("lhs")), Constant(StringLiteral("abc")))),
          Assign(Seq("matchee_1"), Cast(Var("e"), TNode(Exp.multTag))),
          Assert(Eq(PathAccess(Var("matchee_1"), NamedLink("rhs")), Constant(StringLiteral("def")))),
          Assign(Seq("case"), Constant(IntLiteral(2)))
        )),
        Body(Seq(
          Assert(NotInstanceOf(Var("e"), TNode(Exp.addTag))),
          Assert(NotInstanceOf(Var("e"), TNode(Exp.multTag))),
          Assign(Seq("matchee_3"), Cast(Var("e"), TNode(Exp.notTag))),
          Assert(Eq(PathAccess(Var("matchee_3"), NamedLink("e")), Constant(StringLiteral("ghi")))),
          Assign(Seq("case"), Constant(IntLiteral(3)))
        )),
        Body(Seq(
          Assert(NotInstanceOf(Var("e"), TNode(Exp.addTag))),
          Assign(Seq("matchee_2"), Cast(Var("e"), TNode(Exp.multTag))),
          Assert(Neq(PathAccess(Var("matchee_2"), NamedLink("rhs")), Constant(StringLiteral("def")))),
          Assign(Seq("matchee_3"), Cast(Var("e"), TNode(Exp.notTag))),
          Assert(Eq(PathAccess(Var("matchee_3"), NamedLink("e")), Constant(StringLiteral("ghi")))),
          Assign(Seq("case"), Constant(IntLiteral(3)))
        )),
        Body(Seq(
          Assign(Seq("matchee_0"), Cast(Var("e"), TNode(Exp.addTag))),
          Assert(Neq(PathAccess(Var("matchee_0"), NamedLink("lhs")), Constant(StringLiteral("abc")))),
          Assert(NotInstanceOf(Var("e"), TNode(Exp.multTag))),
          Assign(Seq("matchee_3"), Cast(Var("e"), TNode(Exp.notTag))),
          Assert(Eq(PathAccess(Var("matchee_3"), NamedLink("e")), Constant(StringLiteral("ghi")))),
          Assign(Seq("case"), Constant(IntLiteral(3)))
        )),
        Body(Seq(
          Assign(Seq("matchee_0"), Cast(Var("e"), TNode(Exp.addTag))),
          Assert(Neq(PathAccess(Var("matchee_0"), NamedLink("lhs")), Constant(StringLiteral("abc")))),
          Assign(Seq("matchee_2"), Cast(Var("e"), TNode(Exp.multTag))),
          Assert(Neq(PathAccess(Var("matchee_2"), NamedLink("rhs")), Constant(StringLiteral("def")))),
          Assign(Seq("matchee_3"), Cast(Var("e"), TNode(Exp.notTag))),
          Assert(Eq(PathAccess(Var("matchee_3"), NamedLink("e")), Constant(StringLiteral("ghi")))),
          Assign(Seq("case"), Constant(IntLiteral(3)))
        ))
      ))
    ))

    assertDesugar(core, sugared, options.copy(stopOnError = false))
  }

  "desugaring" should "eliminate var pattern eliminates remaining patterns" in {
    val sugared = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), TUnit, Seq(Body(Seq(
        Match(one, Seq(
          Case(VarPattern("n"), Body(
            Assign(Seq("case"), Constant(IntLiteral(1)))
          )),
          Case(NodePattern(TNode(Exp.letTag), Seq(PatternBinding("name", VarPattern("n")))), Body(
            Assign(Seq("case"), Constant(IntLiteral(2)))
          )),
          Case(NamedPattern("node", NodePattern(TNode(Exp.letTag), Seq(PatternBinding("name", VarPattern("n"))))), Body(
            Assign(Seq("case"), Constant(IntLiteral(3)))
          )),
          Case(TuplePattern(Seq(VarPattern("x1"), VarPattern("x2"), VarPattern("x3"))), Body(
            Assign(Seq("case"), Constant(IntLiteral(4)))
          )),
          Case(LiteralPattern(StringLiteral("abc")), Body(
            Assign(Seq("case"), Constant(IntLiteral(5)))
          )),
          Case(NodePattern(TNode(Exp.letTag), Seq(PatternBinding("name", WildcardPattern))), Body(
            Assign(Seq("case"), Constant(IntLiteral(6)))
          )),
          Case(WildcardPattern, Body(
            Assign(Seq("case"), Constant(IntLiteral(7)))
          ))
        ))
      ))))
    ))

    val core = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), TUnit, Seq(Body(Seq(
        Assign(Seq("n"), one),
        Assign(Seq("case"), Constant(IntLiteral(1)))
      ))))
    ))

    assertDesugar(core, sugared)
  }


  "desugaring" should "implement match semantics" in {
    val module = Module("Test_Match", Seq(), Seq(
      PatternFunction(None, "integerlits", Seq(), TLiteral.Int, Seq(Body(Seq(
        Values("root", TNode(Exp.expTag)),
        Assert(Undef(PathAccess(Var("root"), ParentLink))),
        Yield(
          Call("integerlits_rec",
            Seq(Var("root"))
          )
        )
      )))),

      PatternFunction(None, "integerlits_rec", Seq(Param("e", TNode(Exp.expTag))), TLiteral.Int, Seq(Body(Seq(
        Match(Var("e"), Seq(
          Case(
            NodePattern(TNode(Exp.intTag), Seq(PatternBinding("value", VarPattern("v")))),
            Body(Yield(Var("v")))),
          Case(
            NodePattern(TNode(Exp.addTag), Seq(PatternBinding("lhs", VarPattern("e1")))),
            Body(Yield(Call("integerlits_rec",
              Seq(Var("e1")))))),
          Case(
            NodePattern(TNode(Exp.multTag), Seq(PatternBinding("rhs", VarPattern("e1")))),
            Body(Yield(Call("integerlits_rec",
              Seq(Var("e1"))))))
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

    assertMatchFunModule(module, "integerlits_rec", input) { matcher =>
      assert(matcher.getAllMatches.size() == 7)
    }

    assertMatchFunModule(module, "integerlits", input) { matcher =>
      assert(matcher.getAllMatches.size() == 1)
      matcher.getAllMatchArrays should contain theSameElementsAs Seq(Array(2))
    }
  }
}
