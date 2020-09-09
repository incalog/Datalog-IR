package inca.frontend.parser
import fastparse._
import NoWhitespace._

/** Utils for the CoreParser.
  *
  * @author Ronja Schnur (rschnur@students.uni-mainz.de)
  *         Julian Cichorius (jcichori@students.uni-mainz.de)
  */
object ParserUtils {

  /** Parser consuming all whitespaces by ignoring them. */
  def w_i[_: P]: P[Unit] = CharsWhileIn("\n \t\r").?

  /** Parser consuming all spaces by ignoring them. */
  def s_i[_: P]: P[Unit] = CharsWhileIn(" ").?
  
  def sn_i[_: P]: P[Unit] = CharsWhileIn(" \n\r").?

  /** Parser consuming all tabulators by ignoring them. */
  def t_i[_: P]: P[Unit] = CharsWhileIn("\t").?

  /** Parser consuming all newline characters by ignoring them. */
  def n_i[_: P]: P[Unit] = CharsWhileIn("\r\n").?

  /** Parser for line endings */
  def n_[_:P]:P[Unit] = P("\n" | "\r\n")

  /** A parser for integer literals in base 10. It does not allow leading zeroes */
  def integer[_: P]: P[Int] = P(rawInteger).map(_.toInt)

  /** A parser for long literals in base 10. It does not allow leading zeroes */
  def long[_: P]: P[Long] = P(rawInteger).map(_.toLong)

  /** A parser for double literals in base 10 */
  def double[_: P]: P[Double] = P(rawDouble).map(_.toDouble)

  /** A parser for string literals
    * @todo implement character escaping
    */
  def string[_: P]: P[String] = P("\"\"".!.map(_ => "") | "\"" ~ CharsWhile(_ != '\"').! ~ "\"")

  private def rawInteger[_: P] =
    P(
      (("+" | "-").? ~ CharIn("1-9") ~ CharsWhileIn("0-9").?).! | P("0" ~ CharsWhile(_.isSpaceChar, 1) | "0" ~ End).!
    )

  private def rawDouble[_: P] =
    P(
      P((rawInteger | "0") ~ "d") | (("0" | rawInteger) ~ "." ~ CharsWhileIn("0-9", 1).? ~ "d".?)
    ).!
}
