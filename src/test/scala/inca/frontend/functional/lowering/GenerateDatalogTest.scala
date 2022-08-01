package inca.frontend.functional.lowering

import inca.backend.ir.DatalogScala
import inca.compiler.Compiler
import inca.examples.functional.AST
import inca.examples.functional.Code
import inca.examples.functional.ControlDataFlow
import inca.examples.functional.HigherOrder
import inca.frontend.functional.compiler.FunctionalOptions
import inca.util.Scala
import org.scalatest.funsuite.AnyFunSuite
import scala.meta.XtensionQuasiquoteTerm

class GenerateDatalogTest extends AnyFunSuite {

  def gpmodule(content: DatalogScala.Pattern*): DatalogScala.Module =
    DatalogScala.Module("Main", Seq(), content, Seq())

  val baseExampleGP: DatalogScala.Module = gpmodule(
    DatalogScala.Pattern(
      None,
      "main",
      Seq(DatalogScala.Param("out$0", DatalogScala.host.TScalaInt)),
      Seq(
        DatalogScala.Body(
          Seq(
            DatalogScala.Computed(
              DatalogScala.Var("lit$0"),
              DatalogScala.Evaluation(Seq(), DatalogScala.host.TScalaInt, Scala(q"() => 7 + (12 * 3)"))
            ),
            DatalogScala.Eq(DatalogScala.Var("out$0"), DatalogScala.Var("lit$0"))
          )
        )
      )
    )
  )

  val baseExampleGP2: DatalogScala.Module = gpmodule(
    DatalogScala.Pattern(
      None,
      "main",
      Seq(DatalogScala.Param("out$0", DatalogScala.host.TScalaInt)),
      Seq(
        DatalogScala.Body(
          Seq(
            DatalogScala.Computed(
              DatalogScala.Var("lit$0"),
              DatalogScala.Evaluation(Seq(), DatalogScala.host.TScalaInt, Scala(q"() => 7"))
            ),
            DatalogScala.Computed(
              DatalogScala.Var("lit$1"),
              DatalogScala.Evaluation(Seq(), DatalogScala.host.TScalaInt, Scala(q"() => 12"))
            ),
            DatalogScala.Computed(
              DatalogScala.Var("lit$2"),
              DatalogScala.Evaluation(Seq(), DatalogScala.host.TScalaInt, Scala(q"() => 3"))
            ),
            DatalogScala.Computed(
              DatalogScala.Var("eval$0"),
              DatalogScala.Evaluation(
                Seq(
                  DatalogScala.Var("lit$1") -> DatalogScala.host.TScalaInt,
                  DatalogScala.Var("lit$2") -> DatalogScala.host.TScalaInt
                ),
                DatalogScala.host.TScalaInt,
                Scala(q"(left: Int, right: Int) => left * right")
              )
            ),
            DatalogScala.Computed(
              DatalogScala.Var("eval$1"),
              DatalogScala.Evaluation(
                Seq(
                  DatalogScala.Var("lit$0") -> DatalogScala.host.TScalaInt,
                  DatalogScala.Var("eval$0") -> DatalogScala.host.TScalaInt
                ),
                DatalogScala.host.TScalaInt,
                Scala(q"(left: Int, right: Int) => left + right")
              )
            ),
            DatalogScala.Eq(DatalogScala.Var("out$0"), DatalogScala.Var("eval$1"))
          )
        )
      )
    )
  )

  val varExampleGP: DatalogScala.Module = gpmodule(
    DatalogScala.Pattern(
      None,
      "main",
      Seq(DatalogScala.Param("out$0", DatalogScala.host.TScalaInt)),
      Seq(
        DatalogScala.Body(
          Seq(
            DatalogScala.Computed(
              DatalogScala.Var("lit$0"),
              DatalogScala.Evaluation(Seq(), DatalogScala.host.TScalaInt, Scala(q"() => 7"))
            ),
            DatalogScala.Eq(DatalogScala.Var("x"), DatalogScala.Var("lit$0")),
            DatalogScala.Computed(
              DatalogScala.Var("lit$1"),
              DatalogScala.Evaluation(Seq(), DatalogScala.host.TScalaInt, Scala(q"() => 3"))
            ),
            DatalogScala.Eq(DatalogScala.Var("y"), DatalogScala.Var("lit$1")),
            DatalogScala.Computed(
              DatalogScala.Var("lit$2"),
              DatalogScala.Evaluation(Seq(), DatalogScala.host.TScalaInt, Scala(q"() => 12"))
            ),
            DatalogScala.Computed(
              DatalogScala.Var("eval$0"),
              DatalogScala.Evaluation(
                Seq(
                  DatalogScala.Var("lit$2") -> DatalogScala.host.TScalaInt,
                  DatalogScala.Var("y") -> DatalogScala.host.TScalaInt
                ),
                DatalogScala.host.TScalaInt,
                Scala(q"(left: Int, right: Int) => left * right")
              )
            ),
            DatalogScala.Computed(
              DatalogScala.Var("eval$1"),
              DatalogScala.Evaluation(
                Seq(
                  DatalogScala.Var("x") -> DatalogScala.host.TScalaInt,
                  DatalogScala.Var("eval$0") -> DatalogScala.host.TScalaInt
                ),
                DatalogScala.host.TScalaInt,
                Scala(q"(left: Int, right: Int) => left + right")
              )
            ),
            DatalogScala.Eq(DatalogScala.Var("out$0"), DatalogScala.Var("eval$1"))
          )
        )
      )
    )
  )

  val ifExampleGP: DatalogScala.Module = gpmodule(
    DatalogScala.Pattern(
      None,
      "main",
      Seq(DatalogScala.Param("out$0", DatalogScala.host.TScalaInt)),
      Seq(
        DatalogScala.Body(
          Seq(
            DatalogScala.Computed(
              DatalogScala.Var("lit$0"),
              DatalogScala.Evaluation(Seq(), DatalogScala.host.TScalaInt, Scala(q"() => 7"))
            ),
            DatalogScala.Eq(DatalogScala.Var("x"), DatalogScala.Var("lit$0")),
            DatalogScala.Computed(
              DatalogScala.Var("lit$1"),
              DatalogScala.Evaluation(Seq(), DatalogScala.host.TScalaInt, Scala(q"() => 0"))
            ),
            DatalogScala.Computed(
              DatalogScala.Var("eval$0"),
              DatalogScala.Evaluation(
                Seq(
                  DatalogScala.Var("x") -> DatalogScala.host.TScalaInt,
                  DatalogScala.Var("lit$1") -> DatalogScala.host.TScalaInt
                ),
                DatalogScala.host.TScalaBoolean,
                Scala(q"(left: Int, right: Int) => left > right")
              )
            ),
            DatalogScala.Eq(DatalogScala.Var("eval$0"), DatalogScala.host.True),
            DatalogScala.Eq(DatalogScala.Var("out$0"), DatalogScala.Var("x"))
          )
        ),
        DatalogScala.Body(
          Seq(
            DatalogScala.Computed(
              DatalogScala.Var("lit$0"),
              DatalogScala.Evaluation(Seq(), DatalogScala.host.TScalaInt, Scala(q"() => 7"))
            ),
            DatalogScala.Eq(DatalogScala.Var("x"), DatalogScala.Var("lit$0")),
            DatalogScala.Computed(
              DatalogScala.Var("lit$1"),
              DatalogScala.Evaluation(Seq(), DatalogScala.host.TScalaInt, Scala(q"() => 0"))
            ),
            DatalogScala.Computed(
              DatalogScala.Var("eval$0"),
              DatalogScala.Evaluation(
                Seq(
                  DatalogScala.Var("x") -> DatalogScala.host.TScalaInt,
                  DatalogScala.Var("lit$1") -> DatalogScala.host.TScalaInt
                ),
                DatalogScala.host.TScalaBoolean,
                Scala(q"(left: Int, right: Int) => left > right")
              )
            ),
            DatalogScala.Eq(DatalogScala.Var("eval$0"), DatalogScala.host.False),
            DatalogScala.Computed(
              DatalogScala.Var("lit$2"),
              DatalogScala.Evaluation(Seq(), DatalogScala.host.TScalaInt, Scala(q"() => -1"))
            ),
            DatalogScala.Computed(
              DatalogScala.Var("eval$1"),
              DatalogScala.Evaluation(
                Seq(
                  DatalogScala.Var("x") -> DatalogScala.host.TScalaInt,
                  DatalogScala.Var("lit$2") -> DatalogScala.host.TScalaInt
                ),
                DatalogScala.host.TScalaInt,
                Scala(q"(left: Int, right: Int) => left * right")
              )
            ),
            DatalogScala.Eq(DatalogScala.Var("out$0"), DatalogScala.Var("eval$1"))
          )
        )
      )
    )
  )

  val ifExample2GP: DatalogScala.Module = gpmodule(
    DatalogScala.Pattern(
      None,
      "main",
      Seq(DatalogScala.Param("out$0", DatalogScala.host.TScalaInt)),
      Seq(
        DatalogScala.Body(
          Seq(
            DatalogScala.Computed(
              DatalogScala.Var("lit$0"),
              DatalogScala.Evaluation(Seq(), DatalogScala.host.TScalaInt, Scala(q"() => 7"))
            ),
            DatalogScala.Eq(DatalogScala.Var("x"), DatalogScala.Var("lit$0")),
            DatalogScala.Computed(
              DatalogScala.Var("lit$1"),
              DatalogScala.Evaluation(Seq(), DatalogScala.host.TScalaInt, Scala(q"() => -3"))
            ),
            DatalogScala.Eq(DatalogScala.Var("y"), DatalogScala.Var("lit$1")),
            DatalogScala.Computed(
              DatalogScala.Var("lit$2"),
              DatalogScala.Evaluation(Seq(), DatalogScala.host.TScalaInt, Scala(q"() => 0"))
            ),
            DatalogScala.Computed(
              DatalogScala.Var("eval$0"),
              DatalogScala.Evaluation(
                Seq(
                  DatalogScala.Var("x") -> DatalogScala.host.TScalaInt,
                  DatalogScala.Var("lit$2") -> DatalogScala.host.TScalaInt
                ),
                DatalogScala.host.TScalaBoolean,
                Scala(q"(left: Int, right: Int) => left > right")
              )
            ),
            DatalogScala.Eq(DatalogScala.Var("eval$0"), DatalogScala.host.True),
            DatalogScala.Computed(
              DatalogScala.Var("lit$4"),
              DatalogScala.Evaluation(Seq(), DatalogScala.host.TScalaInt, Scala(q"() => 0"))
            ),
            DatalogScala.Computed(
              DatalogScala.Var("eval$2"),
              DatalogScala.Evaluation(
                Seq(
                  DatalogScala.Var("y") -> DatalogScala.host.TScalaInt,
                  DatalogScala.Var("lit$4") -> DatalogScala.host.TScalaInt
                ),
                DatalogScala.host.TScalaBoolean,
                Scala(q"(left: Int, right: Int) => left > right")
              )
            ),
            DatalogScala.Eq(DatalogScala.Var("eval$2"), DatalogScala.host.True),
            DatalogScala.Computed(
              DatalogScala.Var("eval$4"),
              DatalogScala.Evaluation(
                Seq(DatalogScala.Var("x") -> DatalogScala.host.TScalaInt, DatalogScala.Var("y") -> DatalogScala.host.TScalaInt),
                DatalogScala.host.TScalaInt,
                Scala(q"(left: Int, right: Int) => left + right")
              )
            ),
            DatalogScala.Eq(DatalogScala.Var("out$0"), DatalogScala.Var("eval$4"))
          )
        ),
        DatalogScala.Body(
          Seq(
            DatalogScala.Computed(
              DatalogScala.Var("lit$0"),
              DatalogScala.Evaluation(Seq(), DatalogScala.host.TScalaInt, Scala(q"() => 7"))
            ),
            DatalogScala.Eq(DatalogScala.Var("x"), DatalogScala.Var("lit$0")),
            DatalogScala.Computed(
              DatalogScala.Var("lit$1"),
              DatalogScala.Evaluation(Seq(), DatalogScala.host.TScalaInt, Scala(q"() => -3"))
            ),
            DatalogScala.Eq(DatalogScala.Var("y"), DatalogScala.Var("lit$1")),
            DatalogScala.Computed(
              DatalogScala.Var("lit$2"),
              DatalogScala.Evaluation(Seq(), DatalogScala.host.TScalaInt, Scala(q"() => 0"))
            ),
            DatalogScala.Computed(
              DatalogScala.Var("eval$0"),
              DatalogScala.Evaluation(
                Seq(
                  DatalogScala.Var("x") -> DatalogScala.host.TScalaInt,
                  DatalogScala.Var("lit$2") -> DatalogScala.host.TScalaInt
                ),
                DatalogScala.host.TScalaBoolean,
                Scala(q"(left: Int, right: Int) => left > right")
              )
            ),
            DatalogScala.Eq(DatalogScala.Var("eval$0"), DatalogScala.host.True),
            DatalogScala.Computed(
              DatalogScala.Var("lit$4"),
              DatalogScala.Evaluation(Seq(), DatalogScala.host.TScalaInt, Scala(q"() => 0"))
            ),
            DatalogScala.Computed(
              DatalogScala.Var("eval$2"),
              DatalogScala.Evaluation(
                Seq(
                  DatalogScala.Var("y") -> DatalogScala.host.TScalaInt,
                  DatalogScala.Var("lit$4") -> DatalogScala.host.TScalaInt
                ),
                DatalogScala.host.TScalaBoolean,
                Scala(q"(left: Int, right: Int) => left > right")
              )
            ),
            DatalogScala.Eq(DatalogScala.Var("eval$2"), DatalogScala.host.False),
            DatalogScala.Computed(
              DatalogScala.Var("lit$5"),
              DatalogScala.Evaluation(Seq(), DatalogScala.host.TScalaInt, Scala(q"() => -1"))
            ),
            DatalogScala.Computed(
              DatalogScala.Var("eval$3"),
              DatalogScala.Evaluation(
                Seq(
                  DatalogScala.Var("y") -> DatalogScala.host.TScalaInt,
                  DatalogScala.Var("lit$5") -> DatalogScala.host.TScalaInt
                ),
                DatalogScala.host.TScalaInt,
                Scala(q"(left: Int, right: Int) => left * right")
              )
            ),
            DatalogScala.Computed(
              DatalogScala.Var("eval$4"),
              DatalogScala.Evaluation(
                Seq(
                  DatalogScala.Var("x") -> DatalogScala.host.TScalaInt,
                  DatalogScala.Var("eval$3") -> DatalogScala.host.TScalaInt
                ),
                DatalogScala.host.TScalaInt,
                Scala(q"(left: Int, right: Int) => left + right")
              )
            ),
            DatalogScala.Eq(DatalogScala.Var("out$0"), DatalogScala.Var("eval$4"))
          )
        ),
        DatalogScala.Body(
          Seq(
            DatalogScala.Computed(
              DatalogScala.Var("lit$0"),
              DatalogScala.Evaluation(Seq(), DatalogScala.host.TScalaInt, Scala(q"() => 7"))
            ),
            DatalogScala.Eq(DatalogScala.Var("x"), DatalogScala.Var("lit$0")),
            DatalogScala.Computed(
              DatalogScala.Var("lit$1"),
              DatalogScala.Evaluation(Seq(), DatalogScala.host.TScalaInt, Scala(q"() => -3"))
            ),
            DatalogScala.Eq(DatalogScala.Var("y"), DatalogScala.Var("lit$1")),
            DatalogScala.Computed(
              DatalogScala.Var("lit$2"),
              DatalogScala.Evaluation(Seq(), DatalogScala.host.TScalaInt, Scala(q"() => 0"))
            ),
            DatalogScala.Computed(
              DatalogScala.Var("eval$0"),
              DatalogScala.Evaluation(
                Seq(
                  DatalogScala.Var("x") -> DatalogScala.host.TScalaInt,
                  DatalogScala.Var("lit$2") -> DatalogScala.host.TScalaInt
                ),
                DatalogScala.host.TScalaBoolean,
                Scala(q"(left: Int, right: Int) => left > right")
              )
            ),
            DatalogScala.Eq(DatalogScala.Var("eval$0"), DatalogScala.host.False),
            DatalogScala.Computed(
              DatalogScala.Var("lit$3"),
              DatalogScala.Evaluation(Seq(), DatalogScala.host.TScalaInt, Scala(q"() => -1"))
            ),
            DatalogScala.Computed(
              DatalogScala.Var("eval$1"),
              DatalogScala.Evaluation(
                Seq(
                  DatalogScala.Var("x") -> DatalogScala.host.TScalaInt,
                  DatalogScala.Var("lit$3") -> DatalogScala.host.TScalaInt
                ),
                DatalogScala.host.TScalaInt,
                Scala(q"(left: Int, right: Int) => left * right")
              )
            ),
            DatalogScala.Computed(
              DatalogScala.Var("lit$4"),
              DatalogScala.Evaluation(Seq(), DatalogScala.host.TScalaInt, Scala(q"() => 0"))
            ),
            DatalogScala.Computed(
              DatalogScala.Var("eval$2"),
              DatalogScala.Evaluation(
                Seq(
                  DatalogScala.Var("y") -> DatalogScala.host.TScalaInt,
                  DatalogScala.Var("lit$4") -> DatalogScala.host.TScalaInt
                ),
                DatalogScala.host.TScalaBoolean,
                Scala(q"(left: Int, right: Int) => left > right")
              )
            ),
            DatalogScala.Eq(DatalogScala.Var("eval$2"), DatalogScala.host.True),
            DatalogScala.Computed(
              DatalogScala.Var("eval$4"),
              DatalogScala.Evaluation(
                Seq(
                  DatalogScala.Var("eval$1") -> DatalogScala.host.TScalaInt,
                  DatalogScala.Var("y") -> DatalogScala.host.TScalaInt
                ),
                DatalogScala.host.TScalaInt,
                Scala(q"(left: Int, right: Int) => left + right")
              )
            ),
            DatalogScala.Eq(DatalogScala.Var("out$0"), DatalogScala.Var("eval$4"))
          )
        ),
        DatalogScala.Body(
          Seq(
            DatalogScala.Computed(
              DatalogScala.Var("lit$0"),
              DatalogScala.Evaluation(Seq(), DatalogScala.host.TScalaInt, Scala(q"() => 7"))
            ),
            DatalogScala.Eq(DatalogScala.Var("x"), DatalogScala.Var("lit$0")),
            DatalogScala.Computed(
              DatalogScala.Var("lit$1"),
              DatalogScala.Evaluation(Seq(), DatalogScala.host.TScalaInt, Scala(q"() => -3"))
            ),
            DatalogScala.Eq(DatalogScala.Var("y"), DatalogScala.Var("lit$1")),
            DatalogScala.Computed(
              DatalogScala.Var("lit$2"),
              DatalogScala.Evaluation(Seq(), DatalogScala.host.TScalaInt, Scala(q"() => 0"))
            ),
            DatalogScala.Computed(
              DatalogScala.Var("eval$0"),
              DatalogScala.Evaluation(
                Seq(
                  DatalogScala.Var("x") -> DatalogScala.host.TScalaInt,
                  DatalogScala.Var("lit$2") -> DatalogScala.host.TScalaInt
                ),
                DatalogScala.host.TScalaBoolean,
                Scala(q"(left: Int, right: Int) => left > right")
              )
            ),
            DatalogScala.Eq(DatalogScala.Var("eval$0"), DatalogScala.host.False),
            DatalogScala.Computed(
              DatalogScala.Var("lit$3"),
              DatalogScala.Evaluation(Seq(), DatalogScala.host.TScalaInt, Scala(q"() => -1"))
            ),
            DatalogScala.Computed(
              DatalogScala.Var("eval$1"),
              DatalogScala.Evaluation(
                Seq(
                  DatalogScala.Var("x") -> DatalogScala.host.TScalaInt,
                  DatalogScala.Var("lit$3") -> DatalogScala.host.TScalaInt
                ),
                DatalogScala.host.TScalaInt,
                Scala(q"(left: Int, right: Int) => left * right")
              )
            ),
            DatalogScala.Computed(
              DatalogScala.Var("lit$4"),
              DatalogScala.Evaluation(Seq(), DatalogScala.host.TScalaInt, Scala(q"() => 0"))
            ),
            DatalogScala.Computed(
              DatalogScala.Var("eval$2"),
              DatalogScala.Evaluation(
                Seq(
                  DatalogScala.Var("y") -> DatalogScala.host.TScalaInt,
                  DatalogScala.Var("lit$4") -> DatalogScala.host.TScalaInt
                ),
                DatalogScala.host.TScalaBoolean,
                Scala(q"(left: Int, right: Int) => left > right")
              )
            ),
            DatalogScala.Eq(DatalogScala.Var("eval$2"), DatalogScala.host.False),
            DatalogScala.Computed(
              DatalogScala.Var("lit$5"),
              DatalogScala.Evaluation(Seq(), DatalogScala.host.TScalaInt, Scala(q"() => -1"))
            ),
            DatalogScala.Computed(
              DatalogScala.Var("eval$3"),
              DatalogScala.Evaluation(
                Seq(
                  DatalogScala.Var("y") -> DatalogScala.host.TScalaInt,
                  DatalogScala.Var("lit$5") -> DatalogScala.host.TScalaInt
                ),
                DatalogScala.host.TScalaInt,
                Scala(q"(left: Int, right: Int) => left * right")
              )
            ),
            DatalogScala.Computed(
              DatalogScala.Var("eval$4"),
              DatalogScala.Evaluation(
                Seq(
                  DatalogScala.Var("eval$1") -> DatalogScala.host.TScalaInt,
                  DatalogScala.Var("eval$3") -> DatalogScala.host.TScalaInt
                ),
                DatalogScala.host.TScalaInt,
                Scala(q"(left: Int, right: Int) => left + right")
              )
            ),
            DatalogScala.Eq(DatalogScala.Var("out$0"), DatalogScala.Var("eval$4"))
          )
        )
      )
    )
  )

  val incFunGP = DatalogScala.Pattern(
    None,
    "inc",
    Seq(DatalogScala.Param("n", DatalogScala.host.TScalaInt), DatalogScala.Param("out$0", DatalogScala.host.TScalaInt)),
    Seq(
      DatalogScala.Body(
        Seq(
          DatalogScala.Computed(
            DatalogScala.Var("lit$0"),
            DatalogScala.Evaluation(Seq(), DatalogScala.host.TScalaInt, Scala(q"() => 1"))
          ),
          DatalogScala.Computed(
            DatalogScala.Var("eval$0"),
            DatalogScala.Evaluation(
              Seq(DatalogScala.Var("n") -> DatalogScala.host.TScalaInt, DatalogScala.Var("lit$0") -> DatalogScala.host.TScalaInt),
              DatalogScala.host.TScalaInt,
              Scala(q"(left: Int, right: Int) => left + right")
            )
          ),
          DatalogScala.Eq(DatalogScala.Var("out$0"), DatalogScala.Var("eval$0"))
        )
      )
    )
  )
  val incMainGP = DatalogScala.Pattern(
    None,
    "main",
    Seq(DatalogScala.Param("out$0", DatalogScala.host.TScalaInt)),
    Seq(
      DatalogScala.Body(
        Seq(
          DatalogScala.Computed(
            DatalogScala.Var("lit$0"),
            DatalogScala.Evaluation(Seq(), DatalogScala.host.TScalaInt, Scala(q"() => 0"))
          ),
          DatalogScala.Call(
            "inc",
            Seq(DatalogScala.Var("lit$0"), DatalogScala.Var("call$0")),
            transitive = false,
            neg = false
          ),
          DatalogScala.Eq(DatalogScala.Var("out$0"), DatalogScala.Var("call$0"))
        )
      )
    )
  )
  val incModuleGP = gpmodule(incFunGP, incMainGP)

  val factFunGP = DatalogScala.Pattern(
    None,
    "fact",
    Seq(DatalogScala.Param("n", DatalogScala.host.TScalaInt), DatalogScala.Param("out$0", DatalogScala.host.TScalaInt)),
    Seq(
      DatalogScala.Body(
        Seq(
          DatalogScala.Computed(
            DatalogScala.Var("lit$0"),
            DatalogScala.Evaluation(Seq(), DatalogScala.host.TScalaInt, Scala(q"() => 1"))
          ),
          DatalogScala.Computed(
            DatalogScala.Var("eval$0"),
            DatalogScala.Evaluation(
              Seq(DatalogScala.Var("n") -> DatalogScala.host.TScalaInt, DatalogScala.Var("lit$0") -> DatalogScala.host.TScalaInt),
              DatalogScala.host.TScalaBoolean,
              Scala(q"(left: Int, right: Int) => left == right")
            )
          ),
          DatalogScala.Eq(DatalogScala.Var("eval$0"), DatalogScala.host.True),
          DatalogScala.Computed(
            DatalogScala.Var("lit$1"),
            DatalogScala.Evaluation(Seq(), DatalogScala.host.TScalaInt, Scala(q"() => 1"))
          ),
          DatalogScala.Eq(DatalogScala.Var("out$0"), DatalogScala.Var("lit$1"))
        )
      ),
      DatalogScala.Body(
        Seq(
          DatalogScala.Computed(
            DatalogScala.Var("lit$0"),
            DatalogScala.Evaluation(Seq(), DatalogScala.host.TScalaInt, Scala(q"() => 1"))
          ),
          DatalogScala.Computed(
            DatalogScala.Var("eval$0"),
            DatalogScala.Evaluation(
              Seq(DatalogScala.Var("n") -> DatalogScala.host.TScalaInt, DatalogScala.Var("lit$0") -> DatalogScala.host.TScalaInt),
              DatalogScala.host.TScalaBoolean,
              Scala(q"(left: Int, right: Int) => left == right")
            )
          ),
          DatalogScala.Eq(DatalogScala.Var("eval$0"), DatalogScala.host.False),
          DatalogScala.Computed(
            DatalogScala.Var("lit$2"),
            DatalogScala.Evaluation(Seq(), DatalogScala.host.TScalaInt, Scala(q"() => 1"))
          ),
          DatalogScala.Computed(
            DatalogScala.Var("eval$1"),
            DatalogScala.Evaluation(
              Seq(DatalogScala.Var("n") -> DatalogScala.host.TScalaInt, DatalogScala.Var("lit$2") -> DatalogScala.host.TScalaInt),
              DatalogScala.host.TScalaInt,
              Scala(q"(left: Int, right: Int) => left - right")
            )
          ),
          DatalogScala.Call(
            "fact",
            Seq(DatalogScala.Var("eval$1"), DatalogScala.Var("call$0")),
            transitive = false,
            neg = false
          ),
          DatalogScala.Computed(
            DatalogScala.Var("eval$2"),
            DatalogScala.Evaluation(
              Seq(
                DatalogScala.Var("n") -> DatalogScala.host.TScalaInt,
                DatalogScala.Var("call$0") -> DatalogScala.host.TScalaInt
              ),
              DatalogScala.host.TScalaInt,
              Scala(q"(left: Int, right: Int) => left * right")
            )
          ),
          DatalogScala.Eq(DatalogScala.Var("out$0"), DatalogScala.Var("eval$2"))
        )
      )
    )
  )
  val factMainGP = DatalogScala.Pattern(
    None,
    "main",
    Seq(DatalogScala.Param("n", DatalogScala.host.TScalaInt), DatalogScala.Param("out$0", DatalogScala.host.TScalaInt)),
    Seq(
      DatalogScala.Body(
        Seq(
          DatalogScala.Call(
            "fact",
            Seq(DatalogScala.Var("n"), DatalogScala.Var("call$0")),
            transitive = false,
            neg = false
          ),
          DatalogScala.Eq(DatalogScala.Var("out$0"), DatalogScala.Var("call$0"))
        )
      )
    )
  )
  val factModuleGP = gpmodule(factFunGP, factMainGP)

  val options = FunctionalOptions()

  test("base example") {
    val result = Compiler.compileFunctional(AST.baseExample, options).ir
    assert(result == baseExampleGP)
  }

  test("base example 2") {
    val result = Compiler.compileFunctional(AST.baseExample2, options).ir
    assert(result == baseExampleGP2)
  }

  test("var example") {
    val result = Compiler.compileFunctional(AST.varExample, options).ir
    assert(result == varExampleGP)
  }

  test("if example") {
    val result = Compiler.compileFunctional(AST.ifExample, options).ir
    assert(result == ifExampleGP)
  }

  test("if example 2") {
    val compiled = Compiler.compileFunctional(AST.ifExample2, options)
    println(compiled.fun)
    println(compiled.ir)
    println(compiled.transformed)
    println(compiled.optimized)
    assert(compiled.ir == ifExample2GP)
  }

  test("inc example") {
    val result = Compiler.compileFunctional(AST.incModule, options).ir
    assert(result == incModuleGP)
  }

  test("fact example") {
    val result = Compiler.compileFunctional(AST.factModule, options).ir
    assert(result == factModuleGP)
  }

  test("plus example") {
    val compiled = Compiler.compileFunctional(AST.plusModule, options)
    println(compiled.fun)
    println(compiled.ir)
    println(compiled.transformed)
    println(compiled.optimized)
  }
//  test("running example for section 5") {
//    val result = Compiler.compileFunctional(
//      s"""module Test
//         |data Node = BusStation(String) | TrainStation(String)
//         |
//         |def Edges(): Set[(Node, Node, Int)] = {
//         |  (TrainStation("A"), BusStation("B"), 12),
//         |  (TrainStation("A"), BusStation("C"), 5),
//         |  (TrainStation("D"), BusStation("B"), 145),
//         |  (BusStation("B"), BusStation("C"), 1)
//         |}
//         |
//         |def isBusStation(n: Node): `Boolean` = n match {
//         |  case BusStation(name) => true
//         |  case TrainStation(name) => false
//         |}
//         |
//         |def connectedBusStations(from: Node): Set[Node] =
//         | {to | isBusStation(to), (from, to, d) in Edges()}
//         |
//         |@main def main(): Set[Node] = connectedBusStations(TrainStation("A"))
//         |
//         |
//         |""".stripMargin, options).ir
//    println(result)
//  }

  test("plus real example") {
    val result = Compiler.compileFunctional(AST.plusRealModule, options).ir
    println(result)
  }

  test("set constants") {
    val result = Compiler.compileFunctional(Code.setConstModule, options).ir
    println(result)
  }

  test("set operations") {
    val result = Compiler.compileFunctional(Code.setOperationsModule, options).ir
    println(result)
  }

  test("applyFun") {
    val result = Compiler.compileFunctional(HigherOrder.applyFun, options).ir
    println(result)
  }

  test("lambda") {
    val result = Compiler.compileFunctional(HigherOrder.lambda, options).ir
    println(result)
  }

  test("lambdaHigherOrder") {
    val result = Compiler.compileFunctional(HigherOrder.lambdaHigherOrder, options).ir
    println(result)
  }

  test("composeFun") {
    val result = Compiler.compileFunctional(HigherOrder.composeFun, options).ir
    println(result)
  }

  test("composeLambdas") {
    val result = Compiler.compileFunctional(HigherOrder.composeLambdas, options).ir
    println(result)
  }

  test("cflow") {
    val result = Compiler.compileFunctional(ControlDataFlow.cflowModule, options).ir
    println(result)
  }
}
