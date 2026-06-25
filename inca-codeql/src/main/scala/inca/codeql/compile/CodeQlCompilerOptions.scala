package inca.codeql.compile

import _root_.inca.util.compileroptions.CompilerOptions

final class CodeQlCompilerOptions private(defaults: Seq[(String, Seq[(String, Any)])]) extends CompilerOptions(defaults):
  private[compile] def initialize(): this.type =
    setDefaults()
    this

object CodeQlCompilerOptions:
  implicit val default: CodeQlCompilerOptions =
    CodeQlCompilerOptions(Seq.empty).initialize()
