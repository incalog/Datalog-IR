package inca.backend.optimize
import inca.backend.ir.CollectVarNames
import inca.backend.ir.DatalogScala
import inca.backend.ir.DatalogScala.Evaluation
import inca.backend.ir.DatalogScala.Term
import inca.backend.ir.DatalogScala.Type
import inca.backend.ir.DatalogScala.Var
import inca.runtime.context.DataModel
import inca.util.Gensym
import inca.util.Scala
import scala.collection.immutable.MultiSet
import scala.collection.mutable.ListBuffer

object EvalFusion extends Optimization {
  override def optimizer(dataModel: DataModel): Optimizer = new Optimizer {
    private var evalTerms: Map[Var, Evaluation] = Map()
    private val gensym: Gensym = new Gensym(Iterable.empty)

    override def optimizeBody(body: DatalogScala.Body, pat: DatalogScala.Pattern): Seq[DatalogScala.Body] = {
      val varCount = MultiSet() ++ DatalogScala.collectVarNames.transBody(body) ++ pat.params.map(_.name)
      evalTerms = body.atoms.flatMap {
        case DatalogScala.Computed(v: Var, eval: Evaluation) =>
          if (varCount.get(v.name) == 2) {
            // v is computed here and read only once => do fusion for v
            Some(v -> eval)
          } else {
            // v is read multiple times => no fusion
            None
          }
        case _ => None
      }.toMap
      gensym.scoped {
        super.optimizeBody(body, pat)
      }
    }

    override def optimizeAtom(atom: DatalogScala.Atom): Seq[DatalogScala.Atom] = atom match {
      case DatalogScala.Computed(lhs, Evaluation(args, ty, fun)) =>
        val newArgs = ListBuffer[(Term, Type)]()
        val newParams = ListBuffer[meta.Term.Param]()
        var currentBody = fun.tree.body

        val (remainingArgs, remainingParams) = (args zip fun.tree.params).flatMap {
          case ((v: Var, ty), param) =>
            evalTerms.get(v) match {
              case Some(Evaluation(otherArgs, _, otherFun)) =>
                val freshParams =
                  otherFun.tree.params.map(p => p -> meta.Term.Name(gensym.fresh("fuse")))
                val inlineExp = scalaSubst(
                  otherFun.tree.body,
                  freshParams.map(p => p._1.name.value -> p._2).toMap
                )
                currentBody = scalaSubst(currentBody, Map(param.name.value -> inlineExp))
                freshParams.map(p =>
                  newParams += meta.Term.Param(p._1.mods, p._2, p._1.decltpe, p._1.default))
                newArgs ++= otherArgs
                None
              case _ => Some((v -> ty, param))
            }
          case argParam => Some(argParam)
        }.unzip

        val newFun = meta.Term.Function(remainingParams.toList ++ newParams, currentBody)
        val eval = Evaluation(remainingArgs ++ newArgs, ty, Scala(newFun))
        lhs match {
          case v: Var if evalTerms.contains(v) => evalTerms += v -> eval
          case _ =>
        }
        Seq(DatalogScala.Computed(lhs, eval).withHints(atom))
      case _ => super.optimizeAtom(atom)
    }
  }

  type Env = Map[String, meta.Term]
  def scalaSubst(t: meta.Term, env: Env): meta.Term = {
    if (env.isEmpty) t
    else {
      t.transform { case n @ meta.Term.Name(name) =>
        env.getOrElse(name, n)
      }.asInstanceOf[meta.Term]
    }
  }
}
