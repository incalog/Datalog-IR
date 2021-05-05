package inca.frontend.constraint.parser
import fastparse._

/** Utils for the Parser.
  *
  * @author Ronja Schnur (rschnur@students.uni-mainz.de)
  *         Julian Cichorius (jcichori@students.uni-mainz.de)
  */
object ParserUtils {

  /** Parser consuming all spaces by ignoring them. */
  def sp[_: P]: P[Unit] = CharsWhileIn(" ").?

  /** Parser for line endings */
  def nl_![_: P]: P[Unit] = P("\n" | "\r\n")

  /** A parser for string literals
    * @todo implement character escaping
    */
  def string[_: P]: P[String] =
    P("\"\"".!.map(_ => "") | "\"" ~~ CharsWhile(_ != '\"').! ~~ "\"")

  def rawInteger[_: P]: P[String] =
    P(CharsWhileIn("0-9").!)

  def fail[_: P](msg: String)(implicit ctx: P[_]): P[Nothing] = {
    val res = ctx.freshFailure()
    if (ctx.verboseFailures) ctx.setMsg(ctx.index, () => msg)
    res
  }
}
