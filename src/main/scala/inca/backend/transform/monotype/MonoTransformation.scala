package inca.backend.transform.monotype

import scala.meta._
import inca.backend.ir.Datalog
import inca.backend.ir.util.Substitute
import inca.backend.ir.Datalog.{AddMono, Atom, Body, Call, Computed, Constant, CustomAggregation, Evaluation, IntConstant, MkMono, Module, Param, Pattern, ResultMono, TScala, Var}
import inca.backend.transform.Transformer
import inca.backend.transform.Transformation
import inca.runtime.context.DataModel
import inca.util.Scala

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

final case class MonoTransException(private val message: String = "",
                                 private val cause: Throwable = None.orNull)
  extends Exception(message, cause)


/** Convert a Datalog program to a mono-types free program.

  Assumptions:
  1. Mono-type atoms do not exist in rule head (as they are not real relations,
     they have different semantics);
  2. It is supposed to be performed after demand transformation, because there will always exist
     a control flow when using mono-types;

  The translation does three things:
  1. Create collection relations and rules for MonoAdd;
  2. Replace MkMono(m, cls) by m = new cls();
  3. Replace ResultMono by aggregation.
 */
object MonoTransformation extends Transformation {
  override def transformer(dataModel: DataModel) : Transformer = new Transformer {
    private def isMonoTypeAtom(atom: Atom): Boolean = {
      atom.isInstanceOf[AddMono] || atom.isInstanceOf[ResultMono] || atom.isInstanceOf[MkMono]
    }

    private type TypCtx = Map[String, Seq[Param]]

    val monoCtx : mutable.Map[Datalog.Type, Seq[Datalog.Type]] = mutable.Map()

    override def transformModule(module: Datalog.Module): Datalog.Module = {
      // Collects the type context for each pattern
      val ctx : TypCtx = module.pats.map(pat => pat.name -> pat.params).toMap

      var pats : Seq[Pattern] = module.pats


      // First step: replace MkMono by Computed, replace MonoResult by aggregation
      pats = pats.map(p => transformMkMono(p))
      pats = pats.map(p => transformResultMono(p, ctx))

      // Third step: collect all of the MonoAdd side effects
      val collPats = pats.flatMap(p => transformAddMono(p, ctx))

      // Last step: remove all of the Mono atoms
      pats = pats.map(p => removeMonoTypes(p))


      Module(module.name, module.imports, pats ++ collPats, module.scalaContent)
    }

    override def transformPattern(pat: Pattern): Seq[Pattern] = ???


    /** Determine the type of term in the given body and type context.
     */
    def findTyp(term: Datalog.Term, atoms : Seq[Atom], ctx: TypCtx, patName: String): Datalog.Type = {
      term match {
        case Constant(lit) => lit.typ
        case Var(v) =>
          // if the variable is in the head, we can find its type directly
          for (param <- ctx(patName) if param.name == v)
            return param.typ
          // otherwise, we need to derive its type through other atoms in the same body
          for (atom <- atoms) {
            atom match {
              case Computed(lhs, computation) =>
                lhs match {
                  case Var(l) =>
                    if (l == v) computation match {
                      case Evaluation(_, resultType, _) =>
                        return resultType
                      case _ => ???
                    }
                  case _ => ???
                }
              case Call(name, args, _, _) =>
                for ((arg, param) <- args zip ctx(name) if arg == Var(v))
                    return param.typ
              case _ =>
            }
          }
          throw MonoTransException(s"Can't determine the type of $v in $atoms and $ctx")
      }
    }

    def transformMkMono(pat: Pattern): Pattern = {
      // Store the bodies after transformation
      val bodies: ListBuffer[Body] = ListBuffer()

      for (body <- pat.bodies) {
        val atoms: ListBuffer[Atom] = ListBuffer()
        for (atom <- body.atoms) {
          atom match {
            case MkMono(m, cls, annotation) =>
              atoms += Computed(m, Evaluation(Seq(), cls, Scala(meta.Term.Function(List(), s"new ${cls.asScala.toString()}()".parse[meta.Term].get))))
              monoCtx(cls) = annotation
            case x => atoms += x
          }
        }
        bodies += Body(atoms.toSeq)
      }
      Pattern(pat.vis, pat.name, pat.params, bodies.toSeq)
    }

    /** Transform all of the MkMono and MonoResult to Datalog IR terms.
     *
     */
    def transformResultMono(pat : Pattern, ctx: TypCtx) : Pattern = {
      // used to distinguish different Aggregation atoms
      var aggCounter: Int = 0
      var tmpCounter: Int = 0

      // Store the bodies after transformation
      val bodies : ListBuffer[Body] = ListBuffer()

      for (body <- pat.bodies) {
        val atoms : ListBuffer[Atom] = ListBuffer()
        for (atom <- body.atoms) {
          atom match {
            case ResultMono(m, t) =>
              val monoTyp = findTyp(m, body.atoms, ctx, pat.name)
              val outputTyp = findTyp(t, body.atoms, ctx, pat.name)
              // We assume the aggregated column is always the last column
              val aggArgs = Seq(m) ++ monoCtx(monoTyp).zipWithIndex.map{case (_, i) => Var("v$"+ aggCounter + "$" + i)}
              // As we can't determine the type of state in mono-types currently,
              // we assume state type is the same as output.
              val agg : CustomAggregation = CustomAggregation(
                outputTyp,
                None,
                Scala(s"new ${monoTyp.asScala.toString()}()".parse[meta.Term].get),
                "Coll$" + monoTyp.asScala.toString.split('.').last,
                aggArgs,
                aggArgs.size - 1
              )
              aggCounter += 1
              val tmpVar = Var("tmp$" + tmpCounter)
              tmpCounter += 1
              val tmp : Computed = Computed(tmpVar, agg)
              val res : Computed = Computed(
                t, Evaluation(
                  Seq(m -> monoTyp, tmpVar -> outputTyp),
                  outputTyp,
                  Scala(q"(m : ${monoTyp.asScala}, tmpVar: ${outputTyp.asScala}) => m.result(tmpVar)")
                )
              )
              atoms ++= ListBuffer(tmp, res)
            case x => atoms += x
          }
        }
        bodies += Body(atoms.toSeq)
      }
      Pattern(pat.vis, pat.name, pat.params, bodies.toSeq)
    }

    def findTuple(v: String, atoms: Seq[Atom]) : Seq[meta.Term] = {
      for (atom <- atoms) {
        atom match {
          case Computed(lhs, computation) =>
            lhs match {
              case Var(name) => if (v == name)
                computation match {
                  case Evaluation(evalArgs, resultType, code) =>
                    code.tree match {
                      case meta.Term.Function(params, body) =>
                        body match {
                          case meta.Term.Tuple(args) => return args
                          case x => return Seq(x)
                        }
                      case _ =>
                    }
                  case _ => ???
                }
              case x => x
            }
          case x => x
        }
      }
      ???
    }

    /**
     * Given a tuple of meta terms, assign each element a name.
     * @param tuple
     * @param body
     * @return
     */
    def decomposeTuple(tuple: Seq[meta.Term]) : Seq[Atom] = {
      val atoms : ListBuffer[Atom] = ListBuffer()
      val name = "v$"
      var counter = 0
      for (elem <- tuple){
        elem match {
          case meta.Lit.Int(n) => atoms += Datalog.Eq(Var(name + counter), IntConstant(n))
          case meta.Lit.String(s) => atoms += Datalog.Eq(Var(name + counter), Datalog.StringConstant(s))
          case meta.Term.Name(s) => atoms += Datalog.Eq(Var(name + counter), Datalog.Var(s))
          case _ => ???
        }
        counter = counter + 1
      }
      atoms.toSeq
    }

    /** This method create relations and rules to describe the effects of all the MonoAdd atoms.
     *
     * @param pat
     * @return
     */
    def transformAddMono(pat: Pattern, ctx: TypCtx): Seq[Pattern] = {
      // Name of the mono type variable
      val monoName: String = "mt$var"

      // Prefix of the collection relation name for MonoAdd
      // (it is not possible to create a unified Coll relation because different mono types
      // may have different indexed keys)
      val collName: String = "Coll$"

      // Mapping from (type of mono-type variable, type of input) to the corresponding bodies
      // e.g. the collecting pattern of CountMono is (m : CountMono, v : (String, Int)) :- ...
      val collPats: mutable.Map[(Datalog.Type, Seq[Datalog.Type]), ListBuffer[Body]] = mutable.Map()

      for (body <- pat.bodies) {
        // Find all of the MonoAdd atoms in the current body
        // (assume there is no dependency relation between these MonoAdds)
        val addMonoAtoms: Seq[AddMono] = body.atoms.collect { case monoAdd: AddMono => monoAdd }

        // Other atoms in the body (assume there are no MkMono and ResultMono if AddMono exists)
        require(addMonoAtoms.isEmpty ||
          !body.atoms.exists(a => a.isInstanceOf[MkMono] || a.isInstanceOf[ResultMono]))
        val otherAtoms: Seq[Atom] = body.atoms.filterNot(_.isInstanceOf[AddMono])

        // Make a Cartesian product between MonoAdd terms and other terms.
        for (addAtom <- addMonoAtoms) {
          val monoTyp = findTyp(addAtom.m, otherAtoms, ctx, pat.name)
          val inputTyp = monoCtx(monoTyp)
          val inputAtoms : ListBuffer[Datalog.Atom] = ListBuffer()
          val subst: mutable.Map[Var, Datalog.Term] = mutable.Map(addAtom.m -> Var(monoName))
          addAtom.t match {
            case Var(v) =>
              inputAtoms ++= decomposeTuple(findTuple(v, otherAtoms))
            case Constant(_) =>
            case _ => ???
          }
          val body = Body(otherAtoms.map(a =>
            Substitute.fromMap(subst.toMap).substAtom(a)) ++ inputAtoms)
          if (!collPats.contains((monoTyp, inputTyp)))
            collPats((monoTyp, inputTyp)) = ListBuffer(body)
          else
            collPats((monoTyp, inputTyp)) += body
        }
      }
      val pats = collPats.toMap map { case ((monoTyp, inputTyp), body) =>
        Pattern(
          None,
          name = collName + monoTyp.asInstanceOf[TScala].ty.toString.split('.').last,
          Seq(Param(monoName, monoTyp)) ++ inputTyp.zipWithIndex.map{case (t, i) => Param("v$"+i, t)},
          body.toSeq
        )
      }
      pats.toSeq
    }

    // Remove all of the mono-type atoms
    def removeMonoTypes(pat : Pattern) : Pattern = {
      val bodies : ListBuffer[Body] = ListBuffer()
      for (body <- pat.bodies){
        bodies += Body(body.atoms.filter(atom => !isMonoTypeAtom(atom)))
      }
      Pattern(pat.vis, pat.name, pat.params, bodies.toSeq)
    }


    override def transformAtom(atom: Datalog.Atom): Seq[Datalog.Atom] = Seq(atom)
  }
}
