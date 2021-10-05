package inca.frontend.functionalxsouffle.typechecker

import inca.frontend.souffle.Souffle.{RuleSignature, Module, Type}

object ExtractRelationSignatures {
  def extract(souffle: Module): Seq[RuleSignature] = {
    souffle.contents.collect {
      case rs@RuleSignature(name, params, _) => rs
    }
  }
}
