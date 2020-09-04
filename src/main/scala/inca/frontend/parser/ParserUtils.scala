package inca.frontend.parser
import fastparse._
import ScalaWhitespace._

/**
  * @todo implement parsers for names and primitive literals
  * @version 0.0.1
  * @author Ronja Schnur (rschnur@students.uni-mainz.de)
  *         Julian Cichorius (jcichori@students.uni-mainz.de)
  */
object ParserUtils {

  /** Parse a variable identifier */
  def identifier[_: P]: P[String] =
    P(P(CharIn("a-z", "A-Z")) ~ P(CharIn("a-z", "A-Z", "0-9", "_").rep(0))).!

  /** A parser for integer literals in base 10. It does not allow leading zeroes */
  def integer[_: P]: P[Int] =
    P(P(CharIn("1-9") ~ CharsWhileIn("0-9").?).! | P("0" ~ End).!).map(_.toInt)

  /** Parser consuming all whitespaces by ignoring them. */
  def w_i[_:P]:P[Unit] = CharsWhileIn("\n \t").?
  /** Parser consuming all spaces by ignoring them. */
  def s_i[_:P]:P[Unit] = CharsWhileIn(" ").?
  /** Parser consuming all tabulators by ignoring them. */
  def t_i[_:P]:P[Unit] = CharsWhileIn("\t").?
  /** Parser consuming all newline characters by ignoring them. */
  def n_i[_:P]:P[Unit] = CharsWhileIn("\n").?
}
