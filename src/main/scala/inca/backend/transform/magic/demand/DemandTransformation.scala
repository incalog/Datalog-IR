package inca.backend.transform.magic.demand

import inca.backend.hints.MagicSetHints.{Adornments, InputCall, InputCallKey}
import inca.backend.hints.{Hints, MagicSetHints, OptimizationHints}
import inca.backend.ir.Datalog._
import inca.backend.ir.util.CollectVars
import inca.backend.transform.{FilterBodyTransformer, Transformation, Transformer}
import inca.runtime.context.DataModel
import inca.util.Gensym

import scala.collection.mutable
import scala.collection.mutable.ListBuffer


// This transformation consumes MagicSetHints.IgnoreCall and MagicSetHints.NoInputRelation
object DemandTransformation extends Transformation {

  val demandPatternPrefix = "input$"
  val demandPatternExtensionalPrefix = "ext_input$"
  def inputPatternName(name: Name, demandPat: Seq[Boolean]): String = demandPatternPrefix + name + "$" + demandPat.map(a => if (a) "b" else "f").mkString

  def extensionalInputPatternName(name: Name, demandPat: Seq[Boolean]): String = demandPatternExtensionalPrefix + name + "$" + demandPat.map(a => if (a) "b" else "f").mkString

  val PREFIX_THRESHHOLD: Int = Int.MaxValue

//  private val extractCandidates: ListBuffer[Seq[Atom]] = ListBuffer.empty

  override def transformer(dataModel: DataModel): Transformer = new Transformer {

    val gensym = new Gensym(Seq())

    override def transformModule(mod: Module): Module = {
      var insertedInputCallPats = mod.pats.flatMap(transformPattern)
      insertedInputCallPats.foreach(p => gensym.register(CollectVars.transPattern(p)))

      val inputPatterns = mod.pats.flatMap { pat =>
        val demandPats = getDemandPatterns(pat)
        demandPats.adorn.flatMap { adorn =>
          val (inputPat, updatedPatterns) = deriveInputPattern(pat, adorn, insertedInputCallPats)
          insertedInputCallPats = updatedPatterns
          inputPat
        }
      }


      // remove bodies with calls to input relations that don't exist
      val inputPatNames = inputPatterns.map(_.name).toSet
      val allPats = insertedInputCallPats ++ inputPatterns
      val filter = new FilterBodyTransformer({ body =>
        val hasEmptyInput = body.atoms.exists { con =>
          con.hasHint(InputCallKey) && !inputPatNames.contains(con.asInstanceOf[Call].name)
        }
        !hasEmptyInput
      })
      val filteredPats = allPats.flatMap(filter.transformPattern)

      Module(mod.name, mod.imports, filteredPats, mod.scalaContent)
    }

//    def extract(pats: Seq[Pattern], candidates: Vector[Seq[Atom]]): Seq[Pattern] = {
//      val candidatesSorted = candidates.sortBy(- _.size)
//      var result = pats
//      for (c <- candidatesSorted)
//        result = extract(result, c)
//      result
//    }
//
//    def extract(pats: Seq[Pattern], candidate: Seq[Atom]): Seq[Pattern] = {
//      var found = 0
//      val name = gensym.fresh("extracted")
//      val vars = CollectVars.transBody(Body(candidate)).distinct
//      val extractedPattern = Pattern(None, name, vars.map(Param(_, TAny)), Seq(Body(candidate)))
//      val extracted = pats.map { p =>
//        val bs = for (b <- p.bodies) yield {
//          b.atoms.indexOfSlice(candidate) match {
//            case -1 => b
//            case ix =>
//              val pre = b.atoms.slice(0, ix)
//              val extractCall = Call(name)
//              val post = b.atoms.slice(ix + 1, b.atoms.size)
//          }
//        }
//        p.copy(bodies = bs)
//      }
//      if (found > 1) {
//        ???
//      } else {
//        pats
//      }
//    }

    override def transformPattern(pat: Pattern): Seq[Pattern] =
      if (pat.hasHint(MagicSetHints.DemandPatternsKey)) {
        val demandPats = getDemandPatterns(pat).adorn.toSeq
        Seq(insertInputCall(pat, demandPats))
      } else {
        Seq(pat)
      }

    private def shouldDeriveInput(pat: Pattern): Boolean =
      shouldInsertInput(pat) && pat.hasHint(MagicSetHints.DemandPatternsKey)

    private def shouldInsertInput(body: Hints): Boolean =
      !body.hasHint(MagicSetHints.NoInputRelationKey)

    private def getDemandPatterns(pat: Pattern): MagicSetHints.DemandPatterns = {
      if (!pat.hasHint(MagicSetHints.DemandPatternsKey)) MagicSetHints.DemandPatterns(Set())
      else pat.hints(MagicSetHints.DemandPatternsKey).asInstanceOf[MagicSetHints.DemandPatterns]
    }


    private def insertInputCall(pat: Pattern, demandPats: Seq[Seq[Boolean]]): Pattern = {
      if (!shouldInsertInput(pat))
        return pat

      if (pat.bodies.isEmpty) {
        // TODO is this correct?
        val bodies = demandPats.flatMap { demandPat =>
          deriveInputCall(pat, demandPat).map(c => Body(Seq(c)))
        }
        return Pattern(pat.vis, pat.name, pat.params, bodies).withHints(pat)
      }

      val bodies = demandPats.flatMap { demandPat =>
        pat.bodies.map { b =>
          if (shouldInsertInput(b)) {
            val inputCall = deriveInputCall(pat, demandPat)
            Body(inputCall.toSeq ++ b.atoms).withHints(b)
          } else {
            b
          }
        }
      }
      Pattern(pat.vis, pat.name, pat.params, bodies).withHints(pat)
    }

    private def deriveInputCall(pat: Pattern, demandPat: Seq[Boolean]): Option[Call] = {
      val boundParams = deriveBoundParams(pat, demandPat)
      if (boundParams.isEmpty) {
        Some(Call(inputPatternName(pat.name, demandPat), List(Var("_"))).addHint(InputCall(pat.name)))
      } else {
        val args = boundParams.map(p => Var(p.name))
        Some(Call(inputPatternName(pat.name, demandPat), args).addHint(InputCall(pat.name)))
      }
    }

    private def deriveBoundParams(pat: Pattern, demandPat: Seq[Boolean]): Seq[Param] = {
      val indexBoundParams = deriveBoundIndices(pat, demandPat)
      indexBoundParams.map(pat.params)
    }

    private def deriveBoundIndices(pat: Pattern, demandPat: Seq[Boolean]): Seq[Int] = {
      demandPat.zipWithIndex.filter(_._1).map(_._2)
    }

    private def allParamsBound(body: Body, params: Seq[Param]): Boolean = {
      val currentlyBound: mutable.HashSet[Name] = mutable.HashSet()
      def isBound(t: Term): Boolean = t match {
        case Var(name) => currentlyBound.contains(name)
        case Constant(_) => true
      }
      body.atoms.foreach {
        case Undef(t) =>
          throw new UnsupportedOperationException("Currently does not support Undef in demand transformation")
        case Compare(EqComparator, lhs, rhs) =>
          if (isBound(lhs))
            currentlyBound ++= CollectVars.transTerm(rhs)
          else if (isBound(rhs))
            currentlyBound ++= CollectVars.transTerm(lhs)
          else
            () // do nothing
        case Compare(NeqComparator, lhs, rhs) =>
          () // do nothing
        case atom =>
          currentlyBound ++= CollectVars.transAtom(atom)
      }
      params.forall(p => currentlyBound.contains(p.name))
    }

    /** Returns derived input patterns and updated patterns */
    private def deriveInputPattern(pat: Pattern, adorn: Seq[Boolean], patterns: Seq[Pattern]): (Option[Pattern], Seq[Pattern]) = gensym.scoped {
      if (!shouldDeriveInput(pat))
        return (None, patterns)

      val patName = pat.name

      // generate new names for pattern params to avoid name collision
      val params = pat.params.map { p =>
        val name = gensym.fresh(p.name)
        Param(name, p.typ)
      }

      val boundIndices = deriveBoundIndices(pat, adorn)
      val boundParams = boundIndices.map(params)
      val dummyParam =
        if (boundIndices.isEmpty)
          Some(Param(gensym.fresh("dummy"), TScala("Boolean")))
        else
          None
      val dummyBinding = dummyParam.map(p => Eq(Var(p.name), Constant(BooleanLiteral(true))))

      val inputBodies: ListBuffer[Body] = ListBuffer.empty

      val updatedPatterns: ListBuffer[Pattern] = ListBuffer.empty

      // for each body there can be multiple input bodies (due to multiple pattern calls)
      patterns.foreach { visitedPat =>
        val updatedBodies: ListBuffer[Body] = ListBuffer.empty
        visitedPat.bodies.foreach { body =>
          var atoms = body.atoms
          val prefixAtoms: ListBuffer[Atom] = ListBuffer()
          var prefixCallCount = 0

          while (atoms.nonEmpty) {
            val atom = atoms.head
            atoms = atoms.tail
            atom.asCall match {
              case Some((`patName`, args)) if
                (!atom.isInstanceOf[Call] || !atom.asInstanceOf[Call].neg) &&
                !atom.hints.contains(MagicSetHints.IgnoreCallKey) &&
                Adornments.get(atom).contains(adorn) =>

                val bindings = boundIndices.map { i =>
                  Eq(args(i), Var(params(i).name))
                }
                val prefixPatternName = gensym.freshGlobal(visitedPat.name + "$prefix")
                val prefixAtomSeq = prefixAtoms.toSeq

                // check if every param is bound, else we do not generate rule
                if (allParamsBound(Body(prefixAtomSeq ++ bindings ++ dummyBinding), boundParams ++ dummyParam)) {
                  if (prefixCallCount >= PREFIX_THRESHHOLD) {
                    val prefixBody = Body(prefixAtomSeq)
                    val prefixVars = CollectVars.transBody(prefixBody).distinct
                    val prefixPattern = Pattern(None, prefixPatternName,
                      prefixVars.map(v => Param(v, TAny)),
                      Seq(prefixBody)
                    )
                    val prefixCall = Call(prefixPatternName, prefixVars.map(Var.apply))
                    updatedPatterns += prefixPattern
                    prefixAtoms.clear()
                    prefixAtoms += prefixCall
                    prefixCallCount = 1

                    val inputPatternBody = Body(prefixCall +: (bindings ++ dummyBinding)).withHints(body)
                    inputBodies += inputPatternBody
                  } else {
                    val inputPatternBody = Body(prefixAtomSeq ++  bindings ++ dummyBinding).withHints(body)
                    inputBodies += inputPatternBody
                  }
                }
              case _ => // skip atom
            }
            prefixAtoms += atom
            if (atom.isInstanceOf[Call])
              prefixCallCount += 1
          }

          val newBody = Body(prefixAtoms.toSeq).withHints(body)
          updatedBodies += newBody
        }
        val newPat = Pattern(visitedPat.vis, visitedPat.name, visitedPat.params, updatedBodies.toSeq).withHints(visitedPat)
        updatedPatterns += newPat
      }


      val extensionalBody = if (pat.hasHint(MagicSetHints.MainKey)) {
        val extCall = ExtensionalCall(extensionalInputPatternName(pat.name, adorn), boundParams.map(p => Var(p.name)))
        Some(Body(Seq(extCall) ++ dummyBinding))
      } else {
        None
      }

      val inputPat = Pattern(None,
        inputPatternName(pat.name, adorn),
        boundParams ++ dummyParam,
        inputBodies.toSeq ++ extensionalBody)
        .addHint(MagicSetHints.InputRelation)
      if (pat.hasHint(OptimizationHints.NoInlineInputKey)) {
        inputPat.addHint(OptimizationHints.NoInline)
      }

      (Option.when(inputPat.bodies.nonEmpty)(inputPat), updatedPatterns.toSeq)
    }

    private def collectBodiesCallingPat(caller: Pattern, callee: Pattern): Seq[Body] =
      caller.bodies.filter {
        _.atoms.exists {
          case Call(name, _, _, _) if name == callee.name => true
          case _ => false
        }
      }
  }
}
