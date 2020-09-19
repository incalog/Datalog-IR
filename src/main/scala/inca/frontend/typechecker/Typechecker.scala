package inca.frontend.typechecker

import inca.frontend.typechecker.extensions._
import inca.runtime.context.LanguageMetaInfo
import inca.frontend.parser._
import inca.frontend.util.Program

/** Typechecker Frontend 
  *
  * @author  Ronja Schnur (rschnur@students.uni-mainz.de)
  *          Julian Cichorius (jcichori@students.uni-mainz.de)
  */
object Typechecker 
{
    def typecheck(smi : LanguageMetaInfo, prog : Program) = {
        val tc = new CoreTypechecker(smi, prog, Seq(BoolOpsTypechecker, MatchTypechecker, IfThenElseTypechecker))
        tc.typecheck()
    }
}