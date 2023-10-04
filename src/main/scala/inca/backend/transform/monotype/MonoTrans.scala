package inca.backend.transform.monotype

import scala.meta._
import inca.backend.ir.Datalog
import inca.backend.ir.Datalog.{AddMono, Atom, Body, Computed, CustomAggregation, Evaluation, MkMono, Module, Pattern, ResultMono, TScala, TScalaInt, Var}
import inca.backend.transform.Transformer
import inca.backend.transform.Transformation
import inca.frontend.constraint.core.Eval
import inca.runtime.aggregate.JoinAggregation
import inca.runtime.context.DataModel
import inca.util.{Gensym, Scala}
import jdk.jshell.spi.ExecutionControl.NotImplementedException

import scala.collection.mutable
import scala.collection.mutable.ListBuffer


/** Convert a Datalog program to a mono-types free program.

  Assumptions:
  1. Mono-type atoms do not exist in rule head (as they are not real relations,
     they have different semantics);
  2. It is supposed to be performed after demand transformation, because there will always exist
     a control flow when using mono-types;
  3. There is only one mono-type variable in the program (all of the derived MonoAdd can be used to do
     aggregation).

  The translation have two steps:
  1. Create collection relations and rules for MonoAdd.
  2. Replace ResultMono by aggregation.
 */
object MonoTransformation extends Transformation {

  def replaceTermName(t : Datalog.Term, oldName : String, newName : String) : Datalog.Term = {
    t match {
      case Var(a) => if (a == oldName) Var(newName) else Var(a)
      case x => x
    }
  }

  def repalceComputationName(t : Datalog.Computation, oldName : String, newName : String) : Datalog.Computation = {
    t match {
      case Datalog.Evaluation(evalArgs, resultType, code) =>
        Datalog.Evaluation(
          evalArgs = evalArgs.map{
            case (k : Datalog.Term, typ : Datalog.Type) =>
              (replaceTermName(k, oldName, newName), typ)},
          resultType,
          code
        )
      case x => x
    }
  }

  def replaceAtomName(a : Atom, oldName : String, newName : String) : Atom = {
    a match {
      case Datalog.Call(name, args, transitive, neg) =>
        Datalog.Call(name, args.map(a => replaceTermName(a, oldName, newName)), transitive, neg)
      case Datalog.ExtensionalCall(name, args, neg) =>
        Datalog.ExtensionalCall(name, args.map(a => replaceTermName(a, oldName, newName)), neg)
      case Datalog.Compare(comp, lhs, rhs) =>
        Datalog.Compare(comp, replaceTermName(lhs, oldName, newName), replaceTermName(rhs, oldName, newName))
      case Datalog.HasType(t, typ) => Datalog.HasType(replaceTermName(t, oldName, newName), typ)
      case Datalog.NotHasType(t, typ) => Datalog.NotHasType(replaceTermName(t, oldName, newName), typ)
      case Datalog.Computed(lhs, computation) => Datalog.Computed(replaceTermName(lhs, oldName, newName), repalceComputationName(computation, oldName, newName))
      case x => x
    }
  }

  /** Given a variable and a list of atoms,
   * find if there exists an Computed term in which v is the lhs.
   *
   */
  def findEvaluation(v : String, body : Body) : Option[Datalog.Evaluation] = {
    for (atom <- body.atoms) {
      atom match {
        case Computed(lhs, computation) =>
          lhs match {
            case Var(l) =>
              if (l == v) computation match {
                case Evaluation(evalArgs, resultType, code) =>
                  return Some(Evaluation(evalArgs, resultType, code))
                case x => throw new NotImplementedException("Cannot handle other kinds of inserted terms yet " + x)
              }
          }
        case x => x
      }
    }
    None
  }

  def isMonoTypeAtom(atom : Atom) : Boolean = {
    atom.isInstanceOf[AddMono] || atom.isInstanceOf[ResultMono] || atom.isInstanceOf[MkMono]
  }

  override def transformer(dataModel: DataModel) : Transformer = new Transformer {

    override def transformModule(module: Datalog.Module): Datalog.Module = {
      var pats : ListBuffer[Pattern] = ListBuffer()

      // First step: collect all of the MonoAdd side effects
      pats ++= module.pats.flatMap(p => transformAddMono(p))

      // Second step: replace MonoResult by aggregation
      pats ++= module.pats.map(p => transformResultMono(p))

      // Third step: remove all of the Mono atoms
      pats = pats.map(p => removeMonoTypes(p))


      Module(module.name, module.imports, pats.toSeq, module.scalaContent)
    }

    override def transformPattern(pat: Pattern): Seq[Pattern] = ???


    /** This method Create relation and rules to describe the effects of all the MonoAdd atoms.
     *
     *
     * @param pat
     * @return
     */
    def transformAddMono(pat : Pattern) : Seq[Pattern] = {
      val varName: String = "v@mono" // TODO: use gensym instead
      val collName: String = "Coll"
      val collBodies: ListBuffer[Body] = ListBuffer()
      val monoVars: ListBuffer[Var] = ListBuffer()
      for (body <- pat.bodies) {
        val addMonoAtoms: Seq[AddMono] = body.atoms.collect { case monoAdd : AddMono => monoAdd}
        val otherAtoms: Seq[Atom] = body.atoms.filterNot(_.isInstanceOf[AddMono])
        for (addAtom <- addMonoAtoms) {
          monoVars += addAtom.m
          addAtom.t match {
            // rename the inserted variable by varName in the other atoms
            case Var(v) =>
              collBodies += Datalog.Body(otherAtoms.map(a => replaceAtomName(a, v, varName)))
            case _ => throw new NotImplementedException("Cannot handle other kinds of inserted terms yet")
          }
        }
      }
      // if there is no
      if (collBodies.isEmpty) return Seq()

      // we need to check that there is only one mono-type variable in the
      // rule body
      require(monoVars.toSet.size == 1)
      val monoVar: Var = monoVars.toList.head

      // Find the mono-type parameter in the rule head
      val monoParam : Datalog.Param =
        pat.params.find(p => p.name == monoVar.name).
          getOrElse(throw new NoSuchElementException("Can't find the mono-type parameter in the head " + pat.name))

      // Find the type of terms inserted into mono-type variables
      // TODO: Replace Var with MonoVar to store the type information
      val evaluation = findEvaluation(varName, collBodies.toList.head).getOrElse(throw new RuntimeException("Can't find the evaluation which computes the inserted term of mono-types"))
      val inputTyp = evaluation.resultType


      // if the mono-type variable does not occur in the rule head, we cannot
      // determine its type in the collection pattern.
      require(pat.params.exists(p => p.name == monoVar.name))

      Seq(Pattern(None, collName, Seq(monoParam, Datalog.Param(varName, inputTyp)),
        collBodies.toList))
    }

    /** Transform all of the MonoResult to aggregation and m.result()
     *
     * @param pat
     * @return
     */
    def transformResultMono(pat : Pattern) : Pattern = {
      val bodies : ListBuffer[Body] = ListBuffer()
      for (body <- pat.bodies) {
        val atoms : ListBuffer[Atom] = ListBuffer()
        for (atom <- body.atoms) {
          atom match {
            case ResultMono(m, t) =>
              val agg : CustomAggregation = CustomAggregation(
                TScalaInt,
                None,
                Scala(q"""new inca.backend.transform.monotype.CountMono()"""),
                "Coll",
                Seq(m, Var("v@mono")),
                1
              )
              val tmp : Computed = Computed(
                Var("tmp"),
                agg
              )
              val res : Computed = Computed(
                t, Evaluation(
                  Seq(
                    m -> TScala(Scala(t"inca.backend.transform.monotype.CountMono")),
                    Var("tmp") -> TScalaInt
                  ),
                  TScalaInt,
                  Scala(q"(m : inca.backend.transform.monotype.CountMono, tmp: Int) => m.result(tmp)")
                )
              )
              atoms += tmp
              atoms += res
            case x => atoms += x
          }
        }
        bodies += Body(atoms.toSeq)
      }
      Pattern(pat.vis, pat.name, pat.params, bodies.toSeq)
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
