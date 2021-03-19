package inca.souffle

import inca.compiler.{CompiledFunModule, Options}
import inca.frontend.core.Module
import inca.souffle.Syntax.{Input, PrintSize, RuleSignature}

class CompiledSouffleFrontendModule(override val fun: Module,
                                    val inputs: Seq[(RuleSignature, Input)],
                                    val printSizes: Seq[PrintSize],
                                    override val options: Options) extends CompiledFunModule(fun, options)