package inca.souffle.frontend.compile

import inca.ir
import inca.ir.Language
import inca.ir.extension.{block, bool, demand, disjunction, typeparam}

class GenerateIR {
  val irLang: Language = new Language(Set(ir.BaseIR)
    + irarith.IR + block.IR + bool.IR + irdata.IR + irmatch.IR
    + demand.IR + disjunction.IR + irnot.IR + irset.IR + irmap.IR + irstring.IR + irtuple.IR
    + iragg.IR + iraggset.IR + typeparam.IR
  )
  
}
