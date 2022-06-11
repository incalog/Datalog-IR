package inca.examples.functional

import inca.examples.functional.ADT.Nat_code

object Code {
  def module(content: String*): String =
    s"""module Main
      |${content.mkString("\n")}
      |""".stripMargin

  val baseExample: String = module(
    s"""@main def main(): Int = `7 + (12 * 3)`
      |""".stripMargin
  )

  val baseExample2a: String = module(
    s"""@main def main(): Int = `7` + (`12` * `3`)
      |""".stripMargin
  )

  val baseExample2b: String = module(
    s"""@main def main(): Int = 7 + (12 * 3)
      |""".stripMargin
  )

  val varExample: String = module(
    s"""@main def main(): Int =
      |  let x = 7 in
      |    let y = 3 in
      |      x + (12 * y)
      |""".stripMargin
  )

  val ifExample: String = module(
    s"""@main def main(): Int =
      |  let x = 7 in
      |    if (x > 0)
      |      x * 1
      |    else
      |      x * -1
      |""".stripMargin
  )

  val ifExample3: String = module(
    s"""@main def main(): Int =
      |  let x = -1 in
      |    if (x > 0)
      |      x * 1
      |    else
      |      x * -1
      |""".stripMargin
  )

  val ifExample2: String = module(
    s"""@main def main(): Int =
      |  let x = 7 in
      |    let y = -3 in
      |      (if (x > 0) x else x * -1) + (if (y > 0) y else y * -1)
      |""".stripMargin
  )

  val incModule: String = module(
    s"""def inc(n: Int): Int = n + 1""",
    s"""@main def main(): Int = inc(0)"""
  )

  val factModule: String =
    s"""module Main
      |def fact(n: Int): Int =
      |  if (n == 1)
      |    1
      |  else
      |    n * fact(n - 1)
      |@main def main(n: Int): Int =
      |  fact(n)
      |""".stripMargin

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
      |""".stripMargin
  )

  val plusRealModule: String = module(
    Nat_code,
    s"""def plus(m: Nat, n: Nat): Nat = m match {
      |  case Zero() => n
      |  case Succ(pred) => Succ(plus(pred, n))
      |}
      |""".stripMargin,
    s"""@main def main(x: Nat, y: Nat): Nat =
      |  plus(x, y)
      |""".stripMargin
  )

  val plusRealModuleExtra: String = module(
    Nat_code,
    s"""def plus(m: Nat, n: Nat): Nat =
      | let x = 1 + 2 in
      | m match {
      |  case Zero() => n
      |  case Succ(pred) => Succ(plus(pred, n))
      |}
      |""".stripMargin,
    s"""@main def main(x: Nat, y: Nat): Nat =
      |  plus(x, y)
      |""".stripMargin
  )
  val unaryModule: String =
    s"""module M
      |@main def main(n: Int): Int = -n
      |""".stripMargin

  val methodCallModule: String =
    s"""module M
      |@main def main(n: String): Boolean = n.`contains`("bcd")
      |""".stripMargin

  val fibModule: String =
    s"""module Fib
      |@main def main(x: Int): Int =
      |  fib(x)
      |def fib(n: Int): Int =
      |  if (n == 0)
      |    0
      |  else if (n == 1)
      |    1
      |  else
      |    fib(n - 1) + fib(n - 2)
      |""".stripMargin

  val setConstModule: String =
    s"""module Foo
      |
      |@main def flip: Set[Int] = {0, 1}
      |@main def grades: Set[String] = {"1.0", "1.3", "1.7", "2.0", "2.3", "2.7", "3.0", "3.3", "3.7", "4.0", "5.0"}
      |
      |""".stripMargin

  val setOperationsModule: String =
    s"""module Foo
      |
      |def flip: Set[Int] = {0, 1}
      |def grades: Set[String] = {"1.0", "1.3", "1.7", "2.0", "2.3", "2.7", "3.0", "3.3", "3.7", "4.0", "5.0"}
      |
      |def member: `Boolean` = "1.0" in grades()
      |def union: Set[Int] = flip() ++ flip()
      |
      |def const: Set[Int] = {0 | true}
      |def enum: Set[String] = {g | g in grades()}
      |def project: Set[String] = { (g + " grade") | g in grades()}
      |def filter: Set[String] = { g | g in grades(), g < "3.0" }
      |def cross: Set[(Int, String)] = { (i,g) | i in flip(), g in grades() }
      |""".stripMargin

  val simpleFoldIntModule: String =
    s"""module Foo
      |
      |def add(n1: Int, n2: Int): Int = n1 + n2
      |
      |def fromTo(start: Int, end: Int): Set[Int] =
      |  if (start > end)
      |    {}
      |  else
      |    {start} ++ fromTo(start + 1, end)
      |
      |@main def sum(start: Int, end: Int): Int =
      |  fold(0, add, fromTo(start, end))
      |
      |""".stripMargin

  val simpleFoldModule: String =
    s"""module Foo
      |data Num = V(Int)
      |
      |def add(n1: Num, n2: Num): Num = n1 match {
      |  case V(i1) => n2 match {
      |    case V(i2) => V(i1 + i2)
      |  }
      |}
      |
      |def fromTo(start: Int, end: Int): Set[Num] =
      |  if (start > end)
      |    {}
      |  else
      |    {V(start)} ++ fromTo(start + 1, end)
      |
      |@main def sum(start: Int, end: Int): Num =
      |  fold(V(0), add, fromTo(start, end))
      |
      |""".stripMargin

  val fixPointFun: String =
    s"""module Fix
      |
      |@main def main(n: Int): Int = main(n)
      |""".stripMargin
}
