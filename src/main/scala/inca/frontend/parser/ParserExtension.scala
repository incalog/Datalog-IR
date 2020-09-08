package inca.frontend.parser

import fastparse._
import NoWhitespace._ 
import inca.frontend.core.Core._

trait ParserExtension
{
    /** This function should return a Sequence of expression parsers,
      * which don't require left hand recursion.
      * 
      * An example would be: def parser[_:P]: P[Exp] = P(ConstantParser ~ ...)
      * The constant parser is a example parser not calling any recursion on exp.
      * 
      * @return Sequence of parsers with type P[Exp]
      */
    def anchorExpressions : Seq[P[Exp]] = Seq.empty

    /**
      * This function should return a sequence of expressions parsers that would
      * require left hand recursion. 
      * Therefore these parsers must accept a @see Exp as function argument.
      * 
      * An example would be: def parser[_:P](e: Exp) : P[Exp] = P(ConstantParser ~ ...)
      * The constant parser is a example parser not calling any recursion on exp.
      *
      * @return Sequence of parser functions with type Exp => P[Exp]
      */
    def recursiveExpressions : Seq[Exp => P[Exp]] = Seq.empty


    /**
      * This function should return a sequence of statement parsers.
      *
      * @return Sequence of parser functions with type P[Statement]
      */
    def statement : Seq[P[Statement]] = Seq.empty
}
