package inca.frontend.constraint.core

import inca.frontend.constraint.desugar.Desugarable
import inca.frontend.constraint.extensions
import inca.frontend.constraint.parser.CoreParser
import inca.frontend.constraint.typechecker.CoreTypechecker
import inca.runtime.context.DataModel

//trait Frontend extends CoreParser with CoreTypechecker {
//  final lazy val allDesugarables: Seq[Desugarable] = this.desugarables
//
//  protected def desugarables: Seq[Desugarable] = Seq()
//
//  def parseModule(code: String): fastparse.Parsed[tree.Module] = {
//    fastparse.parse(code, module(_), verboseFailures = true)
//  }
//}
//
//object Frontend {
//  def Core(langInfo: LanguageMetaInfo): Frontend =
//    new Frontend {
//    }
//
//  def Inca(): Frontend =
//    new Frontend
//      with extensions.boolOps.Frontend
//      with extensions.evalCall.Frontend
//      //      with EnumFrontend
//      with extensions.forallExists.Frontend
//      with extensions.foreach.Frontend
//      with extensions.ifThenElse.Frontend
//      with extensions.match_.Frontend
//      with extensions.switch_.Frontend {
//
//    }
//}