package inca.ir.extension.modulesystem

import org.scalatest.funsuite.AnyFunSuiteLike
import inca.ir.*
import inca.ir.extension.arithmetic as arith
import inca.ir.extension.module.{Lowering, MainHint}
import inca.ir.typing.Typechecker
import inca.ir.string2name
import inca.ir.term2Arg

class ModuleTest extends AnyFunSuiteLike:
  def typechecker(): Typechecker = new Typechecker {}

  test("Test module system") {
    val lang = BaseIR.language + arith.IR

    val ac = Module("AbstractConfiguration", lang, Seq(
      ProvideRelation("magicNumber", Seq(Param("x", arith.TInt))),
      Relation("private_number", Seq(Param("x", arith.TInt)), Seq(
        Body(Seq(
          Eq(Var("x"), arith.IntNum(41))
        ))
      )),
      Relation("magicNumber", Seq(Param("x", arith.TInt)), Seq(
        Body(Seq(
          Call("private_number", Seq(Var("x")))
        ))
      ))
    ))

    val c1 = Module("Configuration", lang, Seq(
      RequireRelation("super$magicNumber", Seq(Param("x", arith.TInt))),
      ProvideRelation("magicNumber", Seq(Param("x", arith.TInt))),
      Relation("magicNumber", Seq(Param("x", arith.TInt)), Seq(
        Body(Seq(
          Eq(Var("x"), arith.IntNum(42))
        )),
        Body(Seq(
          Call("super$magicNumber", Seq(Var("x")))
        ))
      ))
    ))

    val c2 = Module("ConfigurationTwo", lang, Seq(
      RequireRelation("super$magicNumber", Seq(Param("x", arith.TInt))),
      ProvideRelation("magicNumber", Seq(Param("x", arith.TInt))),
      Relation("magicNumber", Seq(Param("x", arith.TInt)), Seq(
        Body(Seq(
          Eq(Var("x"), arith.IntNum(43))
        )),
        Body(Seq(
          Call("super$magicNumber", Seq(Var("x")))
        ))
      ))
    ))

    val client = Module("Client", lang, Seq(
      RequireRelation("magicNumber", Seq(Param("x", arith.TInt))),
      ProvideRelation("magicNumber", Seq(Param("x", arith.TInt))),
    ))

    val main = Module("Main", lang, Seq(
      Import("AbstractConfiguration", "AC"),
      Import("Configuration", "C1", Seq(
        RelationSubstitution("super$magicNumber", Seq(Param("x", arith.TInt)), Seq("AC", "magicNumber"), Seq(Param("x", arith.TInt)))
      )),
      Import("Client", "Cl1", Seq(
        RelationSubstitution("magicNumber", Seq(Param("x", arith.TInt)), Seq("C1", "magicNumber"), Seq(Param("x", arith.TInt)))
      )),

      Import("ConfigurationTwo", "C2", Seq(
        RelationSubstitution("super$magicNumber", Seq(Param("x", arith.TInt)), Seq("AC", "magicNumber"), Seq(Param("x", arith.TInt)))
      )),
      Import("Client", "Cl2", Seq(
        RelationSubstitution("magicNumber", Seq(Param("x", arith.TInt)), Seq("C2", "magicNumber"), Seq(Param("x", arith.TInt)))
      )),
      Relation("main", Seq(Param("x", arith.TInt)), Seq(
        Body(Seq(
          Call(Seq("Cl2", "magicNumber"), Seq(Var("x")))
        ))
      ))
    )).addHint(MainHint)

    val mods = Seq(ac, c1, c2, client, main)
    //mods.foreach(m => { println(); println(m) } )

    var checker = typechecker()
    checker.checkProgram(mods)
    checker.failOnWarnings()
    checker.failOnError()

    //mods.foreach(m => { println(); println(m) } )


    val linking = new Lowering {}
    val linked = linking.lower(mods)

    println(linked)

    checker = typechecker()
    checker.checkProgram(Seq(linked))
    checker.failOnWarnings()
    checker.failOnError()

    println(linked)
  }

  test("Provide required") {
    val lang = BaseIR.language + arith.IR
    val defaultSig = Seq(Param("x", arith.TInt))

    val a = Module("A", lang, Seq(
      ProvideRelation("R", defaultSig),
      Relation("R", defaultSig, Seq(
        Body(Seq(
          Eq(Var("x"), arith.IntNum(1))
        ))
      ))
    ))

    val b = Module("B", lang, Seq(
      RequireRelation("Q", defaultSig),
      ProvideRelation("Q", defaultSig),
    ))

    val c = Module("C", lang, Seq(
      RequireRelation("S", defaultSig),
      ProvideRelation("T", defaultSig),
      Relation("T", defaultSig, Seq(
        Body(Seq(
          Call("S", Seq(Var("x")))
        ))
      ))
    ))

    val d = Module("D", lang, Seq(
      Import("A", "MyA"),
      Import("B", "MyB", Seq(
        RelationSubstitution("Q", defaultSig, Seq("MyA", "R"), defaultSig),
      )),
      Import("C", "MyC", Seq(
        RelationSubstitution("S", defaultSig, Seq("MyB", "Q"), defaultSig),
      )),
      // TODO: Allow qualified names in Calls
      /*Relation("Main", defaultSig, Seq(Body(
        Seq(Call("S", Seq(Var("x"))))
      )))*/
    )).addHint(MainHint)

    val mods = Seq(a, b, c, d)
    mods.foreach(m => { println(); println(m) } )

    var checker = typechecker()
    checker.checkProgram(mods)

    checker.failOnWarnings()
    checker.failOnError()

    val linking = new Lowering {}
    val linked = linking.lower(mods)

    checker = typechecker()
    checker.checkProgram(Seq(linked))
    checker.failOnWarnings()
    checker.failOnError()

    println(linked)
  }

  test("Module composition") {
    // Stage 1
    val lang = BaseIR.language + arith.IR

    val c1 = Module("Configuration", lang, Seq(
      RequireRelation("super$magicNumber", Seq(Param("x", arith.TInt))),
      ProvideRelation("magicNumber", Seq(Param("x", arith.TInt))),
      Relation("magicNumber", Seq(Param("x", arith.TInt)), Seq(
        Body(Seq(
          Eq(Var("x"), arith.IntNum(42))
        )),
        Body(Seq(
          Call("super$magicNumber", Seq(Var("x")))
        ))
      ))
    ))

    val c2 = Module("ConfigurationTwo", lang, Seq(
      RequireRelation("super$magicNumber", Seq(Param("x", arith.TInt))),
      ProvideRelation("magicNumber", Seq(Param("x", arith.TInt))),
      Relation("magicNumber", Seq(Param("x", arith.TInt)), Seq(
        Body(Seq(
          Eq(Var("x"), arith.IntNum(43))
        )),
        Body(Seq(
          Call("super$magicNumber", Seq(Var("x")))
        ))
      ))
    ))

    val client = Module("Client", lang, Seq(
      RequireRelation("magicNumber", Seq(Param("x", arith.TInt))),
      ProvideRelation("magicNumber", Seq(Param("x", arith.TInt))),
    ))

    val aMain = Module("AbstractMain", lang, Seq(
      RequireRelation("P", Seq(Param("x", arith.TInt))),
      Import("Configuration", "C1", Seq(
        RelationSubstitution("super$magicNumber", Seq(Param("x", arith.TInt)), Seq("P"), Seq(Param("x", arith.TInt)))
      )),
      Import("Client", "Cl1", Seq(
        RelationSubstitution("magicNumber", Seq(Param("x", arith.TInt)), Seq("C1", "magicNumber"), Seq(Param("x", arith.TInt)))
      )),

      Import("ConfigurationTwo", "C2", Seq(
        RelationSubstitution("super$magicNumber", Seq(Param("x", arith.TInt)), Seq("P"), Seq(Param("x", arith.TInt)))
      )),
      Import("Client", "Cl2", Seq(
        RelationSubstitution("magicNumber", Seq(Param("x", arith.TInt)), Seq("C2", "magicNumber"), Seq(Param("x", arith.TInt)))
      )),
    )).addHint(MainHint)

    var mods = Seq(c1, c2, client, aMain)
    mods.foreach(m => {
      println(); println(m)
    })

    var checker = typechecker()
    checker.checkProgram(mods)
    checker.failOnWarnings()
    checker.failOnError()

    //mods.foreach(m => { println(); println(m) } )


    val stage1Linking = new Lowering {}
    val stage1 = stage1Linking.lower(mods)

    checker = typechecker()
    checker.checkProgram(Seq(stage1))
    checker.failOnWarnings()
    checker.failOnError()

    println()
    println("After Stage 1:")

    // Stage 2:

    val ac = Module("AbstractConfiguration", lang, Seq(
      ProvideRelation("magicNumber", Seq(Param("x", arith.TInt))),
      Relation("private_number", Seq(Param("x", arith.TInt)), Seq(
        Body(Seq(
          Eq(Var("x"), arith.IntNum(41))
        ))
      )),
      Relation("magicNumber", Seq(Param("x", arith.TInt)), Seq(
        Body(Seq(
          Call("private_number", Seq(Var("x")))
        ))
      ))
    ))
    val main = Module("Main", lang, Seq(
      Import("AbstractConfiguration", "AC"),
      Import("AbstractMain", "AM", Seq(
        RelationSubstitution("P", Seq(Param("x", arith.TInt)), Seq("AC", "magicNumber"), Seq(Param("x", arith.TInt))))
      ),
    )).addHint(MainHint)

    mods = Seq(stage1, main, ac)

    mods.foreach(m => { println(); println(m) })

    checker = typechecker()
    checker.checkProgram(mods)
    checker.failOnWarnings()
    checker.failOnError()

    val stage2Linking = new Lowering {}
    val stage2 = stage2Linking.lower(mods)

    checker = typechecker()
    checker.checkProgram(Seq(stage2))
    checker.failOnWarnings()
    checker.failOnError()

    println()
    println("After Stage 2:")
    println(stage2)
  }
