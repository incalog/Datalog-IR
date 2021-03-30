package inca.examples

object HigherOrder {
  def module(content: String*): String =
    s"""module Main
       |${content.mkString("\n")}
       |""".stripMargin

  val applyFun: String =
    s"""module Foo
       |
       |def apply(f: `Int` => `Int`, x: `Int`): `Int` = f(x)
       |
       |def inc(n: `Int`): `Int` = n + 1
       |
       |@main def main(): `Int` = apply(inc, 5)
       |""".stripMargin

  val lambda: String =
    s"""module Foo
       |
       |@main def main(): `Int` = ((x: `Int`) => x * 3)(7)
       |""".stripMargin

  val lambdaHigherOrder: String =
    s"""module Foo
       |
       |@main def main(): `Int` =
       |  ((f: `Int` => `Int`) => (n: `Int`) => f(f(n)))((x: `Int`) => x * 3)(7)
       |""".stripMargin

  val composeFun: String =
    s"""module Foo
       |
       |def compose(f: `Int` => `Double`, g: `Double` => `String`): `Int` => `String` =
       |  (n: `Int`) => g(f(n))
       |
       |def sqrt(n: `Int`): `Double` =  `Math.sqrt`(n)
       |def doubleString(d: `Double`): `String` = `String.valueOf`(d)
       |
       |@main def main(): `String` = compose(sqrt, doubleString)(2)
       |""".stripMargin

  val composeLambdas: String =
    s"""module Foo
       |
       |def compose(f: `Int` => `Double`, g: `Double` => `String`): `Int` => `String` =
       |  (n: `Int`) => g(f(n))
       |
       |@main def main(): `String` =
       |  compose((n: `Int`) => `Math.sqrt`(n), (d: `Double`) => `String.valueOf`(d))(2)
       |""".stripMargin


  val transitiveWrong: String =
    s"""module Foo
       |
       |def transitive(r: Set[(`Int`, `Int`)]): Set[(`Int`, `Int`)] =
       |  r ++ {(n1,n3) | (n1,n2) in r, (n2,n3) in transitive(r)}
       |
       |@main def main(): Set[(`Int`, `Int`)] =
       |  transitive({(1,2), (2,3), (3,1)})
       |""".stripMargin

  val transitive: String =
    s"""module Foo
       |
       |def transitive(r: () => Set[(`Int`, `Int`)]): Set[(`Int`, `Int`)] =
       |  r() ++ {(n1,n3) | (n1,n2) in r(), (n2,n3) in transitive(r)}
       |
       |@main def main(): Set[(`Int`, `Int`)] =
       |  transitive(() => {(1,2), (2,3), (3,1)})
       |""".stripMargin
}
