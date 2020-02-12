package org.inca.gen.gp

import java.io.{File, PrintWriter}

import org.inca.gen.gp.sdk.queryspecification.QuerySpecificationGenerator._
import org.inca.lang.gp.Content.GraphPattern
import Transformers.transformPattern

import scala.meta._


class GPGenerator {

  /**
   * Generates a ast with quasiquotes of scalameta from a IncA graph pattern
   *
   * @param pattern GraphPattern which will be transformed into scalameta tree
   * @return
   */
  def generate(pattern: GraphPattern): Source = {

    // todo put pipeline here
    val transformedPattern = transformPattern(pattern)
    val source = generateQuerySpeicfication(transformedPattern)

    // todo remove, just simple test
    val file = new File(s"generated/${pattern.name}.scala")
    if (!file.exists)
      if (file.createNewFile) {
        new PrintWriter(file) {
          write(source.toString())
          close()
        }
      }

    //    println(source)
    source
  }
}
