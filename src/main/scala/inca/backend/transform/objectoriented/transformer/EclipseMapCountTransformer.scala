package inca.backend.transform.objectoriented.transformer

import inca.backend.ir.Datalog
import inca.backend.ir.Datalog._
import inca.backend.transform.objectoriented.transformer.EclipseMapCountTransformer.{TCounter, counterMetaTy, oidTy}
import inca.runtime.data.objectoriented.Identity
import inca.util.Scala
import org.eclipse.collections.api.factory.Maps
import org.eclipse.collections.api.map.ImmutableMap

import scala.meta.{XtensionQuasiquoteTerm, XtensionQuasiquoteType}

object EclipseMapCountTransformer {
  val oidTy = TScala(Scala(Scala.typeOf[Identity]))

  val counterMetaTy = t"org.eclipse.collections.api.map.ImmutableMap[(${Scala.typeOf[Identity]}, String), Int]" //Scala.typeOf[ImmutableMap[(Identity, String), Int]]
  object TCounter extends TScala(Scala(counterMetaTy))
}

abstract class EclipseMapCountTransformer(override val rootPatternHint: String,
                                          override val leafPatternHint: String,
                                          override val rootParamName: String,
                                          override val inParamName: String,
                                          override val outParamName: String)
  extends CountTransformer(
    rootPatternHint,
    leafPatternHint,
    rootParamName,
    inParamName,
    outParamName
  )(
    TCounter
  ) {

  override private[objectoriented] def produceInitialCounter(): (Term, Seq[Atom]) = {
    val initName = gensym.fresh("init")
    (Var(initName), Seq(Computed(Var(initName), Evaluation(
      Seq(),
      TCounter,
      Scala(q"() => {val a: $counterMetaTy = org.eclipse.collections.api.factory.Maps.immutable.empty(); a } ")
    ))))
  }

  private[objectoriented] def incIntValue(value: Var): (Var, Computed) = {
    val counterOutVar = Var(gensym.fresh(value.name))
    val (counterInArg, counterInParam) = createScalaTermAndParam(inParamName, TScalaInt)
    (counterOutVar, Computed(
      counterOutVar, Evaluation(Seq(value -> TScalaInt), TScalaInt, Scala(q"($counterInParam) => $counterInArg + 1"))
    ))
  }

  private[objectoriented] def getCounter(counterIn: Var, obj: Term, field: Name): (Var, Computed) = {
    val tsMaxVar = Var(gensym.fresh(rootParamName + "Max"))
    val fname = meta.Lit.String(field)
    val (counterInArg, counterInParam) = createScalaTermAndParam(inParamName, TCounter)
    val (objInArg, objInParam) = createScalaTermAndParam("obj", oidTy)
    (tsMaxVar, Computed(tsMaxVar, Evaluation(
        Seq(
          counterIn -> TCounter,
          obj -> oidTy
        ),
        TScalaInt,
        Scala(q"($counterInParam, $objInParam) => $counterInArg.getOrDefault(($objInArg, $fname), 0)")
    )))
  }
  private[objectoriented] def updateCounter(counterIn: Var, oldValue: Term, obj: Term, field: Name): (Var, Computed) = {
    val fname = meta.Lit.String(field)

    val counterOutVar = Var(gensym.fresh(outParamName))
    val (counterInArg, counterInParam) = createScalaTermAndParam(inParamName, TCounter)
    val (objInArg, objInParam) = createScalaTermAndParam("obj", oidTy)
    val (oldValueArg, oldValueParam) = createScalaTermAndParam("value", TScalaInt)
    (counterOutVar, Computed(
      counterOutVar, Evaluation(
        Seq(
          counterIn -> TCounter,
          obj -> oidTy,
          oldValue -> Datalog.TScalaInt
        ),
        TCounter,
        Scala(q"($counterInParam, $objInParam, $oldValueParam) => $counterInArg.newWithKeyValue(($objInArg, $fname), $oldValueArg)")
      )
    ))
  }
}