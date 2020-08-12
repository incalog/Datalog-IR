package inca.backend.optimize

import inca.runtime.context.LanguageMetaInfo

trait Optimization {
  def optimizer(languageMetaInfo: LanguageMetaInfo): Optimizer
}
