package inca.souffle

import inca.compiler.functional.CompiledFunctionalModule
import inca.compiler.FunctionalOptions
import inca.frontend.functional.core.Module
import inca.souffle.Syntax.{Input, PrintSize, RuleSignature}

class CompiledSouffleFrontendModule(override val fun: Module,
                                    val inputs: Seq[(RuleSignature, Input)],
                                    val printSizes: Seq[PrintSize],
                                    override val options: FunctionalOptions) extends CompiledFunctionalModule(fun, options)