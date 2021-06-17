package inca.frontend.functional.measurements.dataflow
trait Exp {
  // toString prints case class rep
  def toSouffle: String
  def toFormulog: String
}
case class Var(n: String) extends Exp {
  override def toString: String = "Var(\"" + n + "\")"
  override def toSouffle: String = "$" + toString
  override def toFormulog: String = s"""var(\"$n\")"""
}
case class Num(n: Int) extends Exp {
  override def toSouffle: String = "$" + toString
  override def toFormulog: String = s"""num($n)"""
}
case class GreaterThan(l: Exp, r: Exp) extends Exp {
  override def toSouffle: String = "$" + s"""GreaterThan(${l.toSouffle},${r.toSouffle})"""
  override def toFormulog: String = s"""greaterThan(${l.toFormulog}, ${r.toFormulog})"""
}
case class Add(l: Exp, r: Exp) extends Exp {
  override def toSouffle: String = "$" + s"""Add(${l.toSouffle},${r.toSouffle})"""
  override def toFormulog: String = s"""add(${l.toFormulog}, ${r.toFormulog})"""
}


trait Stm {
  def toInnerFormulog(id: Int): (Int, String)
  def toFormulog: String = toInnerFormulog(1)._2

  def toInnerSouffle(id: Int): (Int, String)
  final def toSouffle: String = toInnerSouffle(1)._2
}

case class Assign(n: String, e: Exp) extends Stm {
  override def toString: String = s"""Assign(\"$n\",${e.toString})"""
  override def toInnerFormulog(id: Int): (Int, String) = {
    val eString = e.toFormulog
    (id + 1, s"""assign($id,\"$n\",$eString)""")
  }

  override def toInnerSouffle(id: Int): (Int, String) = {
    val eString = e.toSouffle
    (id + 1, "$" + s"""Assign($id,\"$n\",$eString)""")
  }
}

case class Skip() extends Stm {
  override def toInnerFormulog(id: Int): (Int, String) = (id + 1, s"skip($id)")
  override def toInnerSouffle(id: Int): (Int, String) = (id + 1, "$" + s"Skip($id)")
}

case class Sequence(s1: Stm, s2: Stm) extends Stm {
  override def toInnerFormulog(id: Int): (Int, String) = {
    val (s1Count, s1String) = s1.toInnerFormulog(id + 1)
    val (s2Count, s2String) = s2.toInnerFormulog(s1Count)
    (s2Count, s"""sequence($id,$s1String,$s2String)""")
  }

  override def toInnerSouffle(id: Int): (Int, String) = {
    val (s1Count, s1String) = s1.toInnerSouffle(id + 1)
    val (s2Count, s2String) = s2.toInnerSouffle(s1Count)
    (s2Count, "$" + s"""Sequence($id,$s1String,$s2String)""")
  }
}

case class If(c: Exp, t: Stm, e: Stm) extends Stm {
  override def toInnerFormulog(id: Int): (Int, String) = {
    val (tCount, tString) = t.toInnerFormulog(id + 1)
    val (eCount, eString) = e.toInnerFormulog(tCount)
    (eCount, s"""if_($id,${c.toFormulog},$tString,$eString)""")
  }

  override def toInnerSouffle(id: Int): (Int, String) = {
    val (tCount, tString) = t.toInnerSouffle(id + 1)
    val (eCount, eString) = e.toInnerSouffle(tCount)
    (eCount, "$" + s"""If($id,${c.toSouffle},$tString,$eString)""")
  }
}

case class While(c: Exp, b: Stm) extends Stm {
  override def toInnerFormulog(id: Int): (Int, String) = {
    val (bCount, bString) = b.toInnerFormulog(id + 1)
    (bCount, s"""while($id,${c.toFormulog},$bString)""")
  }

  override def toInnerSouffle(id: Int): (Int, String) = {
    val (bCount, bString) = b.toInnerSouffle(id + 1)
    (bCount, "$" + s"""While($id,${c.toSouffle},$bString)""")
  }
}

object ExamplePrograms {
  val ex1: Stm =
    Sequence(
      Assign("x", Num(2)),
      Sequence(
        Assign("y", Num(2)),
        While(GreaterThan(Var("x"), Num(1)),
          Sequence(
            Assign("y", Add(Var("x"), Var("y"))),
            Sequence(
              Skip(),
              Assign("x", Add(Var("x"), Num(2))))))))

  val ex2: Stm =
    Sequence(
      Assign("x", Num(1)),
      Sequence(
        Assign("y", Num(2)),
        Sequence(
          Assign("z", Num(3)),
          Sequence(
            If(
              GreaterThan(Num(3), Var("x")),
              Assign("x", Num(6)),
              Assign("y", Num(7))),
            Sequence(
              While(
                GreaterThan(Num(10), Var("x")),
                Assign("y", Add(Var("y"), Num(1)))),
              While(
                GreaterThan(Var("x"), Num(6)),
                Sequence(
                  Assign("x", Add(Var("x"), Var("y"))),
                  While(
                    GreaterThan(Var("y"), Num(7)),
                    Sequence(
                      Assign("y", Add(Var("x"), Var("y"))),
                      While(
                        GreaterThan(Var("z"), Num(2)),
                        Assign("z", Num(2)))
                    )
                  )
                )
              )
            )
          )
        )
      )
    )

  /*
    x = 10
    y = 1
    z = 4
    if (x > 1) {
      y = 2
      while (z > 2) {
        z = z + y
      }
    } else {
      y = 3
      while (z > 2) {
        z = z + y
      }
      while (z > x) {
        x = x + 100
      }
    }
    while (x > y) {
      x = x + z
    }
   */
  val ex3: Stm =
    Sequence(
      Assign("x", Num(10)),
      Sequence(
        Assign("y", Num(1)),
        Sequence(
          Assign("z", Num(4)),
          Sequence(
            If(
              GreaterThan(Var("x"), Num(1)),
              Sequence(
                Assign("y", Num(2)),
                While(
                  GreaterThan(Var("z"), Num(2)),
                  Assign("z", Add(Var("z"), Var("y")))
                )
              ),
              Sequence(
                Assign("y", Num(3)),
                Sequence(
                  While(
                    GreaterThan(Var("z"), Num(2)),
                    Assign("z", Add(Var("z"), Var("y")))
                  ),
                  While(
                    GreaterThan(Var("z"), Var("x")),
                    Assign("x", Add(Var("x"), Num(100)))
                  )
                )
              )
            ),
            While(
              GreaterThan(Var("x"), Var("y")),
              Assign("x", Add(Var("x"), Var("z")))
            )
          )
        )
      )
    )
}

object GeneratePrograms extends App {
  println("Functional Frontend")
  println(ExamplePrograms.ex1.toString)
  println(ExamplePrograms.ex2.toString)
  println(ExamplePrograms.ex3.toString)

  println("Souffle")
  println(ExamplePrograms.ex1.toSouffle)
  println(ExamplePrograms.ex2.toSouffle)
  println(ExamplePrograms.ex3.toSouffle)

  println("Formulog")
  println(ExamplePrograms.ex1.toFormulog)
  println(ExamplePrograms.ex2.toFormulog)
  println(ExamplePrograms.ex3.toFormulog)
}