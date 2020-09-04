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

  // I don't know if this version works, as I already wrote the test cases for the other one.
  // /**
  //  * A parser for identifiers typically used in programming languages. They might only contain non-special
  //  * ASCII characters and not start with a digit.
  //  */
  // def identifier[_: P]: P[String] = P((CharIn("a-z").! | CharIn("A-Z").!) ~ (CharIn("a-z").! | CharIn("A-Z").! | digit).rep)
  //   .map(t => t._1 + t._2.mkString)

  /** Parse a variable identifier */
  def identifier[_: P]: P[String] =
    P(P(CharIn("a-z", "A-Z")) ~ P(CharIn("a-z", "A-Z", "0-9", "_").rep(0))).!

  /**
   * A parser for integer literals in base 10. It does not allow leading zeroes
   */
  def integer[_: P]: P[Int] = P("0 ".! | (CharIn("1-9").! ~ digit.rep).map(t => t._1 + t._2.mkString)).map(_.toInt)

  private def digit[_: P] = P(CharIn("0-9").!)

}
