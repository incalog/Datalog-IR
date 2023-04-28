package inca.backend.transform.objectoriented

import inca.backend.hints.{ObjectHints, OptimizationHints}
import inca.backend.ir.Datalog._
import inca.backend.ir.util.CollectVars
import inca.backend.transform.{Transformation, Transformer}
import inca.runtime.context.DataModel
import inca.util.Scala

import scala.collection.immutable.Seq
import scala.meta.XtensionQuasiquoteTerm

/**
 * This transformation does the following things:
 * 1. Introduce an allocation counter in the AllocationRoot with the name `alloc` and initialize it with 0.
 * 2. Modify all affected methods that are neither an Allocation (leaf), nor a root to take an `allocIn` and `allocOut`
 *    parameter.
 * 3. Modify the embedded Computed with the hint `AllocationInit` inside the Allocation (leafs) to use the `allocIn`
 *    argument as second parameter for the ObjectID creation. Increase the `allocIn` argument by one and assign the
 *    result to `allocOut` .
 */
object AllocationTransformation extends Transformation {
  override def transformer(dataModel: DataModel): Transformer = new CountTransformer(
    ObjectHints.AllocationRootKey,
    ObjectHints.AllocationConstructorKey,
    "alloc", "allocIn", "allocOut"
  ) {

    private def containsConstructorCall(pat: Pattern): Boolean = {
      pat.bodies.exists { b =>
        b.atoms.exists(isAllocInit)
      }
    }

    override def transformModule(mod: Module): Module = {
      // annotate all leaf pattern
      // a pattern is a leaf pattern, if it constructs and object and is not the root pattern
      mod.pats.foreach { p =>
        if (containsConstructorCall(p) && !p.hasHint(ObjectHints.AllocationRootKey))
          p.addHint(ObjectHints.AllocationConstructor)
        else
          p
      }
      super.transformModule(mod)
    }

    private def isAllocInit(atom: Atom): Boolean =
      atom.hasHint(ObjectHints.AllocationInitKey)

    // wrap a Computed(Var, Evaluation) inside a lambda, that uses the dummy variable from the input call
    def transComputedEvaluation(lhs: Term, eval: Evaluation, allocInName: String): Computed = {
      val orgFun = eval.code.tree
      val q"(..$params) => $f(..$args)" = orgFun
      val allocInArg = scala.meta.Term.Name(allocInName)
      val allocInParam = scala.meta.Term.Param(Nil, allocInArg, Some(TScalaInt.asScala), None)
      val newParams = params :+ allocInParam
      val newArgs = args :+ allocInArg
      val fun = q"(..$newParams) => $f(..$newArgs)"
      Computed(lhs, Evaluation(eval.evalArgs :+ Var(allocInName) -> TScalaInt, eval.resultType, Scala(fun)))
    }

    override def transformLeafPattern(leafPat: Pattern, affectedPattern: Set[Pattern]): Pattern = gensym.scoped  {
      gensym.register(CollectVars.transPattern(leafPat))

      val Pattern(vis, name, params, bodies) = leafPat
      val allocInName = gensym.fresh(inParamName)
      val allocOutName = gensym.fresh(outParamName)

      val affectedPatternNames = affectedPattern.map(_.name)
      val newBodies = bodies.map { body =>
        gensym.scoped {
          var allocVar = Var(allocInName)
          val atoms = body.atoms.flatMap {
            case comp@Computed(lhs, eval: Evaluation) if isAllocInit(comp) =>
              val (allocOut, incComp) = incCounter(allocVar)
              val res = Seq(transComputedEvaluation(lhs, eval, allocVar.name).withHints(comp), incComp)
              allocVar = allocOut
              res
            case c: Call if affectedPatternNames.contains(c.name) =>
              val (allocOutVar, transAtom) = transformCall(c, allocVar)
              allocVar = allocOutVar
              transAtom
            case Computed(lhs, c: CustomAggregation) if affectedPatternNames.contains(c.patName) =>
              Seq(Computed(lhs, transformAgg(c, allocVar)))
            case a =>
              Seq(a)
          }
          val assignAllocOut = Eq(Var(allocOutName), allocVar)
          Body(atoms :+ assignAllocOut).withHints(body)
        }
      }
      val newParams = params :+ Param(allocInName, TScalaInt) :+ Param(allocOutName, TScalaInt)
      Pattern(vis, name, newParams, newBodies)
        .withHints(leafPat)
        .addHint(OptimizationHints.NoInline)
    }

    private def isRecursive(pat: Pattern): Boolean = {
      pat.bodies.exists { b =>
        b.atoms.exists {
          case Call(name, _, _, _) => name == pat.name
          case _ => false
        }
      }
    }

    override def transformRootPattern(rootPat: Pattern, affectedPattern: Set[Pattern]): Pattern = gensym.scoped {
      gensym.register(CollectVars.transPattern(rootPat))

      if (isRecursive(rootPat))
        throw new IllegalArgumentException(s"Root pattern must not be recursive!")

      // name of all calls that end up calling the leaf pattern
      val affectedPatternNames = affectedPattern.map(_.name)

      val bodies = rootPat.bodies.map { body =>
        gensym.scoped {
          var allocVar = Var(gensym.fresh(rootParamName))
          val countInit = Eq(allocVar, Constant(IntLiteral(1)))

          Body(countInit +: body.atoms.flatMap {
            case c: Call if affectedPatternNames.contains(c.name) =>
              val (allocOutVar, transAtom) = transformCall(c, allocVar)
              allocVar = allocOutVar
              transAtom
            case comp@Computed(lhs, c: CustomAggregation) if affectedPatternNames.contains(c.patName) =>
              val newHints = hintWithAdjustedFixedAdornment(comp, c.args.size, Seq(true, false))
              Seq(Computed(lhs, transformAgg(c, allocVar)).withHints(newHints))
            case comp@Computed(lhs, eval: Evaluation) if isAllocInit(comp) =>
              val (allocOut, incComp) = incCounter(allocVar)
              val res = Seq(transComputedEvaluation(lhs, eval, allocVar.name).withHints(comp), incComp)
              allocVar = allocOut
              res
            case a => Seq(a)
          }).withHints(body)
        }
      }
      Pattern(rootPat.vis, rootPat.name, rootPat.params, bodies).withHints(rootPat)
    }
  }
}
