package org.inca.generators.gp

import org.inca.generators.gp.sdk.queryspecification.QuerySpecificationGenerator._
import org.inca.lang.gp.Content.GraphPattern
import org.inca.generators.gp.Transformers.transformPattern

import scala.meta._


class GPGenerator {

  /**
   * Generates a ast with quasiquotes of scalameta from a IncA graph pattern
   *
   * @param pattern        GraphPattern which will be transformed into scalameta tree
   * @param collectionName Name of the file (mps) where pattern is saved
   * @return
   */
    def generate(pattern: GraphPattern, collectionName: String): Source = {

    // todo put pipeline here
    val transformedPattern = transformPattern(pattern)
    val source = generateQuerySpeicfication(transformedPattern, collectionName)

    // todo remove, just simple test
    println(source)
    source
  }
}
