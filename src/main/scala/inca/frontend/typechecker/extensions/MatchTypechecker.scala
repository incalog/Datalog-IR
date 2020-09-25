package inca.frontend.typechecker.extensions

import inca.frontend.core.Core._
import inca.frontend.extensions._
import inca.frontend.typechecker.CoreTypechecker
import inca.frontend.typechecker.TypecheckerExtension
import inca.frontend.core.Core
import inca.frontend.typechecker.TypeContext
import inca.frontend.typechecker.TypeError

/** Match Typechecker Extension
  *
  * @author  Ronja Schnur (rschnur@students.uni-mainz.de)
  *          Julian Cichorius (jcichori@students.uni-mainz.de)
  */
object MatchTypechecker extends TypecheckerExtension 
{

  def typecheck(c : TNode, pb : PatternBinding)(implicit context: TypeContext) : CoreTypechecker.TypeEnvironment = {
    val PatternBinding(field, pattern) = pb 
    
    typechecker.lmi.links.get((c.prettyprint, field)) match {
      case None => 
      case Some(value) => 
    }
    pb.typed(???)

    typecheck(pb)._2

    ???
  }

  private def typecheck(pat : Pattern)(implicit context: TypeContext) : (TypeAnno, CoreTypechecker.TypeEnvironment) = {
    pat match {
      case NodePattern(c, bindings) if (typechecker.subtype(matchee_t, TAnyLinked)) => ???
      case TuplePattern(pats) => matchee_t match {
        case TTuple(ts) if (pats.length == ts.length) => 
          val ne = ts.zip(pats).map(x => {
            val (_, ev) = typecheck(x._1, x._2)
            ev
          }).foldLeft[CoreTypechecker.TypeEnvironment](context.tenv)({case (a, b) => typechecker.union(a, b)})
          (matchee_t, ne)
        case _ => 
          typechecker.errors.addOne(TypeError(s"Matchee is not a Tuple or sizes do not match (${typechecker.where})."))
          (matchee_t, context.tenv)
      }
      case VarPattern(name) => 
        if (context.tenv.contains(name))  {
          typechecker.errors.addOne(TypeError(s"Variable name is already in use (${typechecker.where}, Code: 0x01)."))
          (matchee_t, context.tenv)
        }
        else 
          (matchee_t, context.tenv + (name -> matchee_t))
      case NamedPattern(name, pat) =>
        if (context.tenv.contains(name))  {
          typechecker.errors.addOne(TypeError(s"Variable name is already in use (${typechecker.where}, Code: 0x02)."))
          (matchee_t, context.tenv)
        }
        else {
          val (t, ne) = typecheck(matchee_t, pat)
          (matchee_t, typechecker.union(context.tenv + (name -> matchee_t), ne))
        }
      case LiteralPattern(v) => 
        val t = typechecker.typecheck(v)
        if (t != matchee_t)
          typechecker.errors.addOne(TypeError(s"Unable to match type $matchee_t with $t (${typechecker.where})."))
        (matchee_t, context.tenv)
      case WildcardPattern => (matchee_t, context.tenv)
      case _ =>
        typechecker.errors.addOne(TypeError(s"Error occured ${pat.prettyprint("")} (${typechecker.where})."))
        (matchee_t, context.tenv)
    }
  }

  private def typecheck(matchee_t : TypeAnno, cs: Case)(implicit context: TypeContext) : TypeAnno = {
    val Case(pattern, body) = cs
    val (t, te) = typecheck(matchee_t, pattern)
    typechecker.typecheck(body)(new TypeContext(context, te))
  }

  override def typecheck(s: Core.Statement, last_in_body: Boolean)(implicit context: TypeContext): (Option[Core.TypeAnno], CoreTypechecker.TypeEnvironment, Boolean) = {
    s match {
      case Match(matchee, cases) => {
        val (t, te) = typechecker.typecheck(matchee)
        val return_types  = cases.map(typecheck(t, _))
        
        if (last_in_body) {
          // check if all return values are the same
          if (return_types.exists(x => !typechecker.subtype(return_types.head, x)))
            typechecker.errors.addOne(
              TypeError(
                s"Body has multiple return values (${typechecker.where})."
              )
            )
  
          val rt = if (return_types.nonEmpty) return_types.head
                    else TUnit
          
          (Some(rt), te, true)
        }
        else 
          (None, te, true)
      }
      case _ => (None, context.tenv, false)
    }
  }
}
