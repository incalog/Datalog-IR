package inca.backend.transform

import inca.runtime.context.LanguageMetaInfo

trait Transformation {
  def transformer(languageMetaInfo: LanguageMetaInfo): Transformer
}
