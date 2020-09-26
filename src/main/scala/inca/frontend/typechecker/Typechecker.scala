package inca.frontend.typechecker

import inca.frontend.typechecker.extensions._
import inca.frontend.util.Program
import inca.runtime.context.LanguageMetaInfo

/** Typechecker Frontend 
  *
  * @author  Ronja Schnur (rschnur@students.uni-mainz.de)
  *          Julian Cichorius (jcichori@students.uni-mainz.de)
  */
object Typechecker 
{
    def typecheck(smi : LanguageMetaInfo, prog : Program): TypecheckResult = {
        val tc = new CoreTypechecker(
          smi,
          prog,
          Seq(
            BoolOpsTypechecker,
            MatchTypechecker,
            IfThenElseTypechecker,
            SwitchTypechecker,
            EnumTypechecker,
            CastTypechecker,
            ForeachTypechecker,
            ForAllExistsTypechecker
        ))
        tc.typecheck()
    }
}