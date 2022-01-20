package inca.frontend.souffle.compiler

import inca.frontend.functional.compiler.{CompiledFunctionalModule, FunctionalOptions}
import inca.frontend.functional.core.Module
import inca.frontend.souffle.Syntax.{Input, PrintSize, RuleSignature}

class CompiledSouffleFrontendModule(override val fun: Module,
                                    val inputs: Seq[(RuleSignature, Input)],
                                    val printSizes: Seq[PrintSize],
                                    override val options: FunctionalOptions) extends CompiledFunctionalModule(fun, options)