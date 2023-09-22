package inca.backend.transform.monotype

import scala.meta._
import inca.backend.ir.Datalog
import inca.backend.ir.Datalog.TScalaInt
import inca.runtime.aggregate.JoinAggregation
import inca.util.Scala

import scala.collection.mutable.ListBuffer


case class MaxAgg() extends JoinAggregation[Int] {
  override val name: String = "max"
  override def init: Int = 0
  override def join(v1: Int, v2: Int): Int = v1.max(v2)
  override val isAssociative: Boolean = true
  override val isCommutative: Boolean = true
  override val hasUnjoin: Boolean = false
}



// TODO: generate instance MonoDef from mono-types Scala code directly.
// MonoDef contains the initial state, add and result method.
case class MonoDef(init: meta.Lit, add: meta.Term.Function, result: meta.Term.Function)

/** Eliminating mono-type operations in the datalog program.

  We currently only consider there is only one mono-type variable in
  the program.

  @param monos map class name to its definition
 */
case class MonoTrans (monos : Map[String, MonoDef]) {
  def isMonoAtom(atom : Datalog.Atom) : Boolean = {
    atom match {
      case Datalog.MkMono(_, _) => true
      case Datalog.UpdateMono(_, _, _) => true
      case Datalog.ReadMono(_, _, _) => true
      case _ => false
    }
  }

  def countMonoNum(module: Datalog.Module) : Int = {
    var num : Int = 0
    for {
      pattern <- module.pats
      body <- pattern.bodies
      atom <- body.atoms
      if (atom.isInstanceOf[Datalog.MkMono])
    } {
      num += 1
    }
    num
  }

  /** This method generates a pattern which represents the initial state of mono-type m:
   *
   * @param m the mono-type name.
   * @param cls the name of mono-type
   * @return Update(m, ts, init) :- ts = 0, mono$init(cls, init).
   */
  def mkInitialState(cls: String): Datalog.Pattern = {
    val params : Seq[Datalog.Param] = Seq(
      Datalog.Param("m", Datalog.TScalaString),
      Datalog.Param("ts", Datalog.TScalaInt),
      Datalog.Param("init", Datalog.TAny)
    )

    val bodies : Seq[Datalog.Body] = Seq(Datalog.Body(Seq(
      Datalog.Eq(Datalog.Var("ts"), Datalog.IntConstant(0)),
      Datalog.Eq(Datalog.Var("k"), Datalog.Constant(Datalog.Literal.fromScalaMeta(monos(cls).init).getOrElse(throw ScalaReflectionException("")))),
      Datalog.ExtensionalCall("mono$init", Seq(Datalog.Var(cls), Datalog.Var("k")))
    )))
    Datalog.Pattern(None, "Update", params, bodies)
  }

  /** Generate the relation representing adding a term into the mono-type:
   *
   * @param m the name of mono-type
   * @param t the inserted term
   * @param ts the timestamp of the update operation
   * @param atoms the body of generated rule (containing no-monotype operation)
   * @return Coll(m, a, k) :- R_1(...), ..., R_n(...), k = ts, a = t
   */
  def transUpdateMono(t: Datalog.Term, ts: Int, atoms: Seq[Datalog.Atom]): Datalog.Pattern = {
    val params : Seq[Datalog.Param] = Seq(
      Datalog.Param("m", Datalog.TScalaString),
      Datalog.Param("v", Datalog.TScalaInt),
      Datalog.Param("k", Datalog.TScalaInt)
    )
    val atom1 : Datalog.Atom = Datalog.Eq(Datalog.Var("k"), Datalog.IntConstant(ts))
    val atom2 : Datalog.Atom = Datalog.Eq(Datalog.Var("v"), t)
    val bodies : Seq[Datalog.Body] = Seq(Datalog.Body(atoms :+ atom1 :+ atom2))
    Datalog.Pattern(None, "Coll", params, bodies)
  }

  /** Generate the pattern:
   *     Update(m, i, st1) :- Coll(m, a, cls, i), Update(m, j, st), st1 = cls.add(st, a), i = j + 1.
   *
   */
  def genInductiveUpdate(t: Datalog.Term, cls: String) : Datalog.Pattern = {
    val params : Seq[Datalog.Param] = Seq(
      Datalog.Param("m", Datalog.TScalaString),
      Datalog.Param("i", Datalog.TScalaInt),
      Datalog.Param("st", Datalog.TAny)
    )

    val bodies : Seq[Datalog.Body] = Seq(Datalog.Body(Seq(
      Datalog.Call("Coll", Seq(Datalog.Var("m"), Datalog.Var("a"), Datalog.Var("i"))),
      Datalog.Call("Update", Seq(Datalog.Var("m"), Datalog.Var("j"), Datalog.Var("st"))),
      Datalog.Computed(
        Datalog.Var("st1"),
        Datalog.Evaluation(
          Seq(Datalog.Var("st") -> Datalog.TScalaInt, Datalog.Var("a") -> Datalog.TScalaInt),
          Datalog.TScalaInt,
          Scala(monos(cls).add)
        )),
      Datalog.Computed(Datalog.Var("i"), Datalog.Evaluation(
        Seq(Datalog.Var("j") -> Datalog.TScalaInt),
        Datalog.TScalaInt,
        Scala(q"((x : Int) => x + 1)")
      ))
    )))

    Datalog.Pattern(None, "Update", params, bodies)
  }

  /** Generate aggregation to obtain the state with the maximal timestamp
   *    ReadMono(m, b) :- j = Agg(m, max(i), st), Update(m, j, st), b = cls.result(st)
   * @param m mono-type variable
   * @return
   */
  def genAgg(m : Datalog.Term, cls: String) : Datalog.Pattern = {
    val agg : Datalog.CustomAggregation = Datalog.CustomAggregation(
      Datalog.TScalaInt, None,
      Scala(q"""new inca.backend.transform.monotype.MaxAgg()"""),
      "Coll",
      Seq(Datalog.Var("m"), Datalog.Var("i"), Datalog.Var("st")),
      1
    )
    val params : Seq[Datalog.Param] = Seq(
      Datalog.Param("m", Datalog.TScalaString),
      Datalog.Param("b", Datalog.TScalaInt)
    )

    val bodies : Seq[Datalog.Body] = Seq(Datalog.Body(Seq(
      Datalog.Computed(Datalog.Var("j"), agg),
      Datalog.Call("Update", Seq(Datalog.Var("j"), Datalog.Var("st"))),
      Datalog.Computed(Datalog.Var("b"), Datalog.Evaluation(
        Seq(Datalog.Var("st") -> Datalog.TScalaInt), Datalog.TScalaInt, Scala(monos(cls).result)
      ))
    )))

    Datalog.Pattern(None, "ReadMono", params, bodies)
  }

  def removeMonoAtoms(pattern: Datalog.Pattern) : Datalog.Pattern = {
    val bodies : ListBuffer[Datalog.Body] = ListBuffer()
    for (body <- pattern.bodies) {
      val atoms : ListBuffer[Datalog.Atom] = ListBuffer()
      atoms ++= body.atoms.filter(x => !isMonoAtom(x))
      for (atom <- body.atoms){
        atom match {
          case Datalog.ReadMono(m, t, cls) =>
            atoms += Datalog.Call("ReadMono", Seq(m, t, Datalog.StringConstant(cls)))
          case _ =>
        }
      }
      bodies += Datalog.Body(atoms.toSeq)
    }
    Datalog.Pattern(pattern.vis, pattern.name, pattern.params, bodies.toSeq)
  }

  /** This method iterate over all atoms in the pattern and does four things:
   *
   * 1. For mkMono operation, create an atom `Update(m, 0, init)`
   *    to represent the initial state;
   * 2. For updateMono atom, generate a `Coll : MT * A * TS` relation to record
   *    the value (of type A) being inserted and the current timestamp, for instance,
   *                 Head(..., m) :- R_1(...), ..., Update(m, a), ..., R_n(...).
   *    ----->       Coll(m, a, cls, ts) :- R_1(...), ..., R_n(...)  with ts += 1.
   * 3. Generate the following relations to enable reading mono successfully,
   *        Update(m, i, st) :- Coll(m, a, cls, i), Update(m, j, st'), st = cls.add(st', a), i = j + 1.
   *        Agg(m, max(i), st) :- Update(m, i, st).
   *        ReadMono(m, b, cls) - Agg(m, i, st), b = cls.result(st).
   * 4. In the end, remove all of the rules containing monotype-related atoms.
   */
  def transPattern(pattern : Datalog.Pattern) : Seq[Datalog.Pattern] = {
    // Store the result of transformation
    val transPatterns : ListBuffer[Datalog.Pattern] = ListBuffer[Datalog.Pattern]()

    // Record the timestamp
    var timestamp : Int = 0

    var flag : Boolean = true

    // Generate the initial state relation (1)  and update relation (2)
    for {
       body <- pattern.bodies
       atom <- body.atoms
    }{
      atom match {
        case Datalog.MkMono(m, cls) => transPatterns += mkInitialState(cls)
        case Datalog.UpdateMono(_, t, _) =>
          timestamp += 1
          transPatterns +=
            transUpdateMono(t, timestamp, body.atoms.filter(x => !isMonoAtom(x)))
        case Datalog.ReadMono(m, t, cls) if flag =>
          flag = false
          transPatterns += genInductiveUpdate(t, cls)
          transPatterns += genAgg(m, cls)
        case _ =>
      }
    }

    // Delete all of the updateMono atoms and replace ReadMono atoms by Call atoms.
    transPatterns += removeMonoAtoms(pattern)

    transPatterns.toSeq
  }



  def transModule(module: Datalog.Module) : Datalog.Module = {
    require(countMonoNum(module) == 1)
    Datalog.Module(
      module.name,
      module.imports,
      module.pats.flatMap(p => transPattern(p)),
      module.scalaContent
    )
  }
}
