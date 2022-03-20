package inca.frontend.souffle.compiler

import inca.frontend.functional.compiler.CompiledFunctionalModule
import inca.frontend.functional.compiler.FunctionalOptions
import inca.frontend.functional.core.Module
import inca.frontend.souffle.Syntax.Input
import inca.frontend.souffle.Syntax.PrintSize
import inca.frontend.souffle.Syntax.RuleSignature

class CompiledSouffleFrontendModule(
    override val fun: Module,
    val inputs: Seq[(RuleSignature, Input)],
    val printSizes: Seq[PrintSize],
    override val options: FunctionalOptions)
    extends CompiledFunctionalModule(fun, options)
