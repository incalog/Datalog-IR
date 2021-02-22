package inca.examples

import inca.examples.ADT.Nat_code

object Code {
  def module(content: String*): String =
    s"""module Main
       |${content.mkString("\n")}
       |""".stripMargin

  val baseExample: String = module(
    s"""@main def main(): `Int` = `7 + (12 * 3)`
       |""".stripMargin
  )

  val baseExample2a: String = module(
    s"""@main def main(): `Int` = `7` + (`12` * `3`)
       |""".stripMargin
  )

  val baseExample2b: String = module(
    s"""@main def main(): `Int` = 7 + (12 * 3)
       |""".stripMargin
  )

  val varExample: String = module(
    s"""@main def main(): `Int` =
       |  let x = 7 in
       |    let y = 3 in
       |      x + (12 * y)
       |""".stripMargin
  )

  val ifExample: String = module(
    s"""@main def main(): `Int` =
       |  let x = 7 in
       |    if (x > 0)
       |      x
       |    else
       |      x * -1
       |""".stripMargin
  )

  val ifExample2: String = module(
    s"""@main def main(): `Int` =
       |  let x = 7 in
       |    let y = -3 in
       |      (if (x > 0) x else x * -1) + (if (y > 0) y else y * -1)
       |""".stripMargin
  )

  val incModule: String = module(
    s"""def inc(n: `Int`): `Int` = n + 1""",
    s"""@main def main(): `Int` = inc(0)"""
  )

  val factModule: String = module(
    s"""def fact(n: `Int`): `Int` =
       |  if (n == 1)
       |    1
       |  else
       |    n * fact(n - 1)
       |""".stripMargin,
    s"""@main def main(n: `Int`): `Int` =
       |  fact(n)
       |""".stripMargin
  )

  val plusModule: String = module(
    Nat_code,
    s"""def plus(m: Nat, n: Nat): Nat = m match {
       |  case Zero() => n
       |  case Succ(pred) => Succ(plus(pred, n))
       |}
       |""".stripMargin,
    s"""@main def main(): Nat =
       |  plus(Succ(Succ(Succ(Zero()))), Succ(Succ(Zero())))
       |""".stripMargin
  )

  val plusNoMainModule: String = module(
    Nat_code,
    s"""@main def plus(m: Nat, n: Nat): Nat = m match {
       |  case Zero() => n
       |  case Succ(pred) => Succ(plus(pred, n))
       |}
       |""".stripMargin,
  )

  val plusRealModule: String = module(
    Nat_code,
    s"""def plus(m: Nat, n: Nat): Nat = m match {
       |  case Zero() => n
       |  case Succ(pred) => Succ(plus(pred, n))
       |}
       |""".stripMargin,
    s"""@main def main(m: Nat, n: Nat): Nat =
       |  plus(m, n)
       |""".stripMargin
  )

  val fibModule: String =
    """module Fib
      |@main def main(n: `Int`): `Int` =
      |  fib(n)
      |def fib(n: `Int`): `Int` =
      |  if (n == 0)
      |    0
      |  else if (n == 1)
      |    1
      |  else
      |    fib(n - 1) + fib(n - 2)
      |""".stripMargin
}
