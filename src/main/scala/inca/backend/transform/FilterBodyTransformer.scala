package inca.backend.transform

import inca.backend.ir.GP.{Body, Pattern}

class FilterBodyTransformer(predicate: Body => Boolean) extends Transformer {
  override def transformBody(body: Body, pat: Pattern): Seq[Body] =
    if (predicate(body))
      Seq(body)
    else
      Seq(Body(Seq()))
}
