package inca.frontend.objectoriented.measurements

import inca.backend.hints.MagicSetHints
import inca.backend.hints.MagicSetHints.NoInputRelation
import inca.backend.ir.Datalog
import inca.backend.ir.Datalog.{Call, Eq, IntConstant, Neq, TScala, TScalaBoolean, TScalaInt, TScalaString}
import inca.backend.optimize.{ConstantPropagation, EliminateAliases, EliminateClones, EliminateNonproductiveRelations, EvalFusion, ExtractLargeBodies, FoldConstantAtoms, InferVarTypes, InlineSimpleRelations, Optimization}
import inca.backend.transform.Transformation
import inca.backend.transform.magic.demand.{DemandTransformation, DeriveDemandPatterns}
import inca.compiler.{CompiledModule, Options, SourceLocation}
import inca.runtime.context.DataModel
import inca.util.Scala
import inca.frontend.ir.{EDBChange, Relation, Relation1, Relation2, Relation3, UnitRelation, Datalog => DatalogAPI}
import inca.util.Scala.{symbolOf, typeOf}

import scala.meta.XtensionQuasiquoteTerm

trait EExp
case class EVar(name: String) extends EExp
case class ENum(i: Int) extends EExp
case class EAdd(lhs: EExp, rhs: EExp) extends EExp

case class DDef(name: String, exp: EExp)

trait DDefList
case class DNil() extends DDefList
case class DCons(hd: DDef, tl: DDefList) extends DDefList

object ASGIRModule {
  val TDef = TScala(Scala(typeOf[DDef]))
  val TExp = TScala(Scala(typeOf[EExp]))
  val TDefList = TScala(Scala(typeOf[DDefList]))

  val defSym = symbolOf[DDef]
  val addSym = symbolOf[EAdd]
  val varSym = symbolOf[EVar]
  val numSym = symbolOf[ENum]
  val consSym = symbolOf[DCons]
  val nilSym = symbolOf[DNil]

  val defTy = typeOf[DDef]
  val defListTy = typeOf[DDefList]
  val expTy = typeOf[EExp]
  val addTy = typeOf[EAdd]
  val varTy = typeOf[EVar]
  val numTy = typeOf[ENum]
  val consTy = typeOf[DCons]
  val nilTy = typeOf[DNil]

  def module: CompiledModule = new CompiledModule {
    override val options: Options = new Options {
      override def optimizations: Seq[Optimization] = Seq()
      override def transformations: Seq[Transformation] = Seq(DeriveDemandPatterns, DemandTransformation)
      override def stopOnError: Boolean = true
      override def stopOnWarning: Boolean = true
      override def withOptimizations(opts: Seq[Optimization]): Options = throw new RuntimeException("No optimizations")
      override def withTransformations(trans: Seq[Transformation]): Options = throw new RuntimeException("No transformations")
    }

    override def name: Datalog.Name = "ASG"
    override def sourceLocation: SourceLocation = SourceLocation.NoSourceLocation
    override def dataModel: DataModel = new DataModel()

    def v(name: String) = Datalog.Var(name)

    override def ir: Datalog.Module = Datalog.Module("ASG", Seq(), Seq(
      Datalog.Pattern(None, "main",
        Seq(
          Datalog.Param("endNode", Datalog.TScalaInt),
          Datalog.Param("step", Datalog.TScalaInt),
          Datalog.Param("from", TDef),
          Datalog.Param("to", TDef)
        ),
        Seq(
          Datalog.Body(Seq(
            Call("makeProg", Seq(IntConstant(0), v("endNode"),v("step"), v("defs"))),
            Call("edgesDefs", Seq(v("defs"), v("from"), v("to")))
          ))
        )
      ).addHint(
        MagicSetHints.Main(Seq(true, true))
      ), //Seq(false, false)

      Datalog.Pattern(None, "makeProg",
        Seq(
          Datalog.Param("from", Datalog.TScalaInt),
          Datalog.Param("to", Datalog.TScalaInt),
          Datalog.Param("step", Datalog.TScalaInt),
          Datalog.Param("defs", TDefList)
        ),
        Seq(
          Datalog.Body(Seq(
            Datalog.Computed(Datalog.True, Datalog.Evaluation(
              Seq(
                v("from") -> Datalog.TScalaInt,
                v("to") -> Datalog.TScalaInt
              ),
              TScalaBoolean,
              Scala(q"(f: Int, t: Int) => f < t")
            )),
            Datalog.Computed(v("_next"), Datalog.Evaluation(
              Seq(
                v("from") -> Datalog.TScalaInt,
                v("step") -> Datalog.TScalaInt
              ),
              Datalog.TScalaInt,
              Scala(q"(f: Int, s: Int) => f + s")
            )),
            //Eq(v("_next"), IntConstant(10)),
            Call("makeLine", Seq(v("from"), v("_next"), v("line"))),
            Datalog.Computed(v("circle"), Datalog.Evaluation(
              Seq(
                v("from") -> Datalog.TScalaInt,
                v("_next") -> Datalog.TScalaInt
              ),
              TDef,
              Scala(q"""(f: Int, n: Int) => $defSym("a" + f, $addSym($varSym("a" + n), $numSym(f)))""")
            )),
            Call("makeProg", Seq(v("_next"), v("to"), v("step"), v("rec"))),
            Datalog.Computed(v("tmp"), Datalog.Evaluation(
              Seq(
                v("circle") -> TDef,
                v("rec") -> TDefList
              ),
              TDefList,
              Scala(q"""(c: $defTy, r: $defListTy) => $consSym(c, r)""")
            )),
            Call("concat", Seq(v("line"), v("tmp"), v("defs")))
          )),
          Datalog.Body(Seq(
            Datalog.Computed(Datalog.True, Datalog.Evaluation(
              Seq(
                v("from") -> Datalog.TScalaInt,
                v("to") -> Datalog.TScalaInt
              ),
              TScalaBoolean,
              Scala(q"(f: Int, t: Int) => f >= t")
            )),
            Datalog.Computed(v("circle"), Datalog.Evaluation(
              Seq(
                v("from") -> Datalog.TScalaInt,
              ),
              TDef,
              Scala(q"""(f: Int) => $defSym("a" + f, $addSym($varSym("a" + 0), $numSym(f)))""")
            )),
            Datalog.Computed(v("defs"), Datalog.Evaluation(
              Seq(
                v("circle") -> TDef,
              ),
              TDefList,
              Scala(q"""(c: $defTy) => $consSym(c, $nilSym())""")
            )),
          ))
        )
      ),

      Datalog.Pattern(None, "makeLine",
        Seq(
          Datalog.Param("i", Datalog.TScalaInt),
          Datalog.Param("to", Datalog.TScalaInt),
          Datalog.Param("defs", TDefList)
        ),
        Seq(
          Datalog.Body(Seq(
            Datalog.Computed(Datalog.True, Datalog.Evaluation(
              Seq(
                v("i") -> Datalog.TScalaInt,
                v("to") -> Datalog.TScalaInt
              ),
              TScalaBoolean,
              Scala(q"(i: Int, t: Int) => i < t")
            )),
            Datalog.Computed(v("n"), Datalog.Evaluation(
              Seq(
                v("i") -> Datalog.TScalaInt,
              ),
              TScalaInt,
              Scala(q"(i: Int) => i + 1")
            )),
            Datalog.Computed(v("d"), Datalog.Evaluation(
              Seq(
                v("i") -> Datalog.TScalaInt,
                v("n") -> Datalog.TScalaInt
              ),
              TDef,
              Scala(q"""(i: Int, n: Int) => $defSym("a" + i, $addSym($varSym("a" + n), $numSym(i)))""")
            )),
            Call("makeLine", Seq(v("n"), v("to"), v("ds"))),
            Datalog.Computed(v("defs"), Datalog.Evaluation(
              Seq(
                v("d") -> TDef,
                v("ds") -> TDefList
              ),
              TDefList,
              Scala(q"""(d: $defTy, ds: $defListTy) => $consSym(d, ds)""")
            ))
          )),
          Datalog.Body(Seq(
            Datalog.Computed(Datalog.True, Datalog.Evaluation(
              Seq(
                v("i") -> Datalog.TScalaInt,
                v("to") -> Datalog.TScalaInt
              ),
              TScalaBoolean,
              Scala(q"(i: Int, t: Int) => i >= t")
            )),
            Datalog.Computed(v("defs"), Datalog.Evaluation(
              Seq(),
              TDefList,
              Scala(q"""() => $nilSym()""")
            ))
          ))
        )
      ),

      Datalog.Pattern(None, "concat",
        Seq(
          Datalog.Param("l1", TDefList),
          Datalog.Param("l2", TDefList),
          Datalog.Param("res", TDefList)
        ),
        Seq(
          Datalog.Body(Seq(
            Datalog.Computed(Datalog.True, Datalog.Evaluation(
              Seq(
                v("l1") -> TDefList
              ),
              TScalaBoolean,
              Scala(q"(l1: $defListTy) => l1.isInstanceOf[${typeOf[DNil]}]")
            )),
            Eq(v("res"), v("l2"))
          )),
          Datalog.Body(Seq(
            Datalog.Computed(Datalog.True, Datalog.Evaluation(
              Seq(
                v("l1") -> TDefList
              ),
              TScalaBoolean,
              Scala(q"(l1: $defListTy) => l1.isInstanceOf[$consTy]")
            )),
            Datalog.Computed(v("hd"), Datalog.Evaluation(
              Seq(
                v("l1") -> TDefList
              ),
              TDef,
              Scala(q"(l1: $defListTy) => l1.asInstanceOf[$consTy].hd")
            )),
            Datalog.Computed(v("tl"), Datalog.Evaluation(
              Seq(
                v("l1") -> TDefList
              ),
              TDefList,
              Scala(q"(l1: $defListTy) => l1.asInstanceOf[$consTy].tl")
            )),
            Call("concat", Seq(v("tl"), v("l2"), v("tmp"))),
            Datalog.Computed(v("res"), Datalog.Evaluation(
              Seq(
                v("hd") -> TDef,
                v("tmp") -> TDefList
              ),
              TDefList,
              Scala(q"(hd: $defTy, tmp: $defListTy) => $consSym(hd, tmp)")
            )),
          )),
        )
      ),

      Datalog.Pattern(None, "findDef",
        Seq(
          Datalog.Param("defs", TDefList),
          Datalog.Param("name", TScalaString),
          Datalog.Param("def", TDef)
        ),
        Seq(
          Datalog.Body(Seq(
            Datalog.Computed(Datalog.True, Datalog.Evaluation(
              Seq(
                v("defs") -> TDefList
              ),
              TScalaBoolean,
              Scala(q"(defs: $defListTy) => defs.isInstanceOf[$consTy]")
            )),
            Datalog.Computed(v("def"), Datalog.Evaluation(
              Seq(
                v("defs") -> TDefList
              ),
              TDef,
              Scala(q"(defs: $defListTy) => defs.asInstanceOf[$consTy].hd")
            )),
            Datalog.Computed(v("name"), Datalog.Evaluation(
              Seq(
                v("def") -> TDef
              ),
              TScalaString,
              Scala(q"(d: $defTy) => d.name")
            )),
          )),
          Datalog.Body(Seq(
            Datalog.Computed(Datalog.True, Datalog.Evaluation(
              Seq(
                v("defs") -> TDefList
              ),
              TScalaBoolean,
              Scala(q"(defs: $defListTy) => defs.isInstanceOf[$consTy]")
            )),
            Datalog.Computed(v("hd"), Datalog.Evaluation(
              Seq(
                v("defs") -> TDefList
              ),
              TDef,
              Scala(q"(defs: $defListTy) => defs.asInstanceOf[$consTy].hd")
            )),
            Datalog.Computed(v("tl"), Datalog.Evaluation(
              Seq(
                v("defs") -> TDefList
              ),
              TDefList,
              Scala(q"(defs: $defListTy) => defs.asInstanceOf[$consTy].tl")
            )),
            Datalog.Computed(v("defname"), Datalog.Evaluation(
              Seq(
                v("hd") -> TDef
              ),
              TScalaString,
              Scala(q"(d: $defTy) => d.name")
            )),
            Neq(v("defname"), v("name")),
            Call("findDef", Seq(v("tl"), v("name"), v("def")))
          )),
        )
      ),

      Datalog.Pattern(None, "target",
        Seq(
          Datalog.Param("defs", TDefList),
          Datalog.Param("e", TExp),
          Datalog.Param("def", TDef)
        ),
        Seq(
          Datalog.Body(Seq(
            Datalog.Computed(Datalog.True, Datalog.Evaluation(
              Seq(
                v("e") -> TExp
              ),
              TScalaBoolean,
              Scala(q"(exp: $expTy) => exp.isInstanceOf[$varTy]")
            )),
            Datalog.Computed(v("name"), Datalog.Evaluation(
              Seq(
                v("e") -> TExp
              ),
              TScalaString,
              Scala(q"(exp: $expTy) => exp.asInstanceOf[$varTy].name")
            )),
            Call("findDef", Seq(v("defs"), v("name"), v("def")))
          )),
          Datalog.Body(Seq(
            Datalog.Computed(Datalog.True, Datalog.Evaluation(
              Seq(
                v("e") -> TExp
              ),
              TScalaBoolean,
              Scala(q"(exp: $expTy) => exp.isInstanceOf[$addTy]")
            )),
            Datalog.Computed(v("e1"), Datalog.Evaluation(
              Seq(
                v("e") -> TExp
              ),
              TExp,
              Scala(q"(exp: $expTy) => exp.asInstanceOf[$addTy].lhs")
            )),
            Call("target", Seq(v("defs"), v("e1"), v("def")))
          )),
          Datalog.Body(Seq(
            Datalog.Computed(Datalog.True, Datalog.Evaluation(
              Seq(
                v("e") -> TExp
              ),
              TScalaBoolean,
              Scala(q"(exp: $expTy) => exp.isInstanceOf[$addTy]")
            )),
            Datalog.Computed(v("e2"), Datalog.Evaluation(
              Seq(
                v("e") -> TExp
              ),
              TExp,
              Scala(q"(exp: $expTy) => exp.asInstanceOf[$addTy].rhs")
            )),
            Call("target", Seq(v("defs"), v("e2"), v("def")))
          )),
        )
      ),

      Datalog.Pattern(None, "edgesDef",
        Seq(
          Datalog.Param("defs", TDefList),
          Datalog.Param("def", TDef),
          Datalog.Param("from", TDef),
          Datalog.Param("to", TDef)
        ),
        Seq(
          Datalog.Body(Seq(
            Datalog.Computed(v("e"), Datalog.Evaluation(
              Seq(
                v("def") -> TDef
              ),
              TExp,
              Scala(q"(d: $defTy) => d.exp")
            )),
            Call("target", Seq(v("defs"), v("e"), v("to"))),
            Eq(v("from"), v("def"))
          )),
          Datalog.Body(Seq(
            Datalog.Computed(v("e"), Datalog.Evaluation(
              Seq(
                v("def") -> TDef
              ),
              TExp,
              Scala(q"(d: $defTy) => d.exp")
            )),
            Call("target", Seq(v("defs"), v("e"), v("trg"))),
            Call("edgesDef", Seq(v("defs"), v("trg"), v("from"), v("to")))
          ))
        )
      ),

      Datalog.Pattern(None, "edgesDefs",
        Seq(
          Datalog.Param("defs", TDefList),
          Datalog.Param("from", TDef),
          Datalog.Param("to", TDef)
        ),
        Seq(
          Datalog.Body(Seq(
            Datalog.Computed(Datalog.True, Datalog.Evaluation(
              Seq(
                v("defs") -> TDefList
              ),
              TScalaBoolean,
              Scala(q"(ds: $defListTy) => ds.isInstanceOf[$consTy]")
            )),
            Datalog.Computed(v("hd"), Datalog.Evaluation(
              Seq(
                v("defs") -> TDefList
              ),
              TDef,
              Scala(q"(ds: $defListTy) => ds.asInstanceOf[$consTy].hd")
            )),
            Call("edgesDef", Seq(v("defs"), v("hd"), v("from"), v("to")))
          )),
          Datalog.Body(Seq(
            Datalog.Computed(Datalog.True, Datalog.Evaluation(
              Seq(
                v("defs") -> TDefList
              ),
              TScalaBoolean,
              Scala(q"(ds: $defListTy) => ds.isInstanceOf[$consTy]")
            )),
            Datalog.Computed(v("tl"), Datalog.Evaluation(
              Seq(
                v("defs") -> TDefList
              ),
              TDefList,
              Scala(q"(ds: $defListTy) => ds.asInstanceOf[$consTy].tl")
            )),
            Call("edgesDefs", Seq(v("tl"), v("from"), v("to")))
          ))
        )
      ),
    ), Seq())
  }

  def run(): Unit = {
    val datalog: DatalogAPI = new DatalogAPI(module)
    val start = System.nanoTime()
    val edb = EDBChange.insertions(Seq(Relation.from("ext_input$main$bb", Seq("endNode", "step"), Seq(Seq(10, 10)))))
    //datalog.update(edb)
    //val mainRel = datalog.read(UnitRelation("main"))
    datalog.measure("main", edb)
    //println(mainRel.asTable)
    val diff = System.nanoTime() - start
    println("diff: " + diff.toDouble/1000000d)
  }

  def main(args: Array[String]): Unit = {
    run()
  }
}
