package inca.trans.gp

import inca.MetaElements.{DefinedNodeLink, ListFirstLink, ListNextLink, ListType, NamedLink, NodeType, ParentLink}
import inca.lang.GraphPatternLang._

import scala.meta._

object DeriveImportStatements {

  val nNodeType = Name.Indeterminate(classOf[NodeType].getSimpleName)
  val nListType = Name.Indeterminate(classOf[ListType].getSimpleName)
  val nNamedLink = Name.Indeterminate(classOf[NamedLink].getSimpleName)
  val nListFirstLink = Name.Indeterminate(classOf[ListFirstLink].getSimpleName)
  val nListNextLink = Name.Indeterminate(classOf[ListNextLink].getSimpleName)

  def apply(pat: GraphPattern): List[Stat] = {
    q"""
      import org.eclipse.viatra.query.runtime.api.{GenericPatternMatcher, ViatraQueryEngine}
      import org.eclipse.viatra.query.runtime.api.scope.{QueryScope => ViatraQueryScope}
      import org.eclipse.viatra.query.runtime.matchers.psystem.{PBody, PVariable}
      import org.eclipse.viatra.query.runtime.matchers.psystem.queries.{BasePQuery, PParameter, PVisibility}
      import org.eclipse.viatra.query.runtime.matchers.tuple.Tuples
      import org.eclipse.viatra.query.runtime.matchers.psystem.basicdeferred.ExportedParameter
      import org.eclipse.viatra.query.runtime.matchers.context.common.JavaTransitiveInstancesKey

      import java.util

      import inca.backend.indices.{InputKey, QueryScope, TFQuerySpecification}
      ..${imports(pat)}
    """.stats
  }

  private def imports(pat: GraphPattern): List[Stat] = {
    val paramImports = importForParams(pat.params)
    val bodiesImports = pat.bodies.flatMap(transAlternative)
    val allImports = paramImports ++ bodiesImports
    val importsMap = allImports.map { i => i.toString -> i }.toMap
    importsMap.values.toList
  }

  private def importForParams(params: Seq[Param]): List[Stat] = {
    params.toList.flatMap { p =>
      if (p.typ.isEmpty) Nil
      else p.typ.get match {
        case TType(NodeType(_)) => List(basicenumerables("TypeConstraint"), metaelements(nNodeType))
        case TType(ListType(_)) => List(basicenumerables("TypeConstraint"), metaelements(nListType))
        case _ => List()
      }
    }
  }

  private def transAlternative(alt: Alternative): List[Stat] = alt.constraints.flatMap(transConstraint).toList

  private def transConstraint(constraint: Constraint): List[Stat] = constraint match {
    case Composition(call, neg) =>
      if (neg) List(basicdeferred("NegativePatternCall"))
      else if (call.transitive) List(basicenumerables("BinaryTransitiveClosure"))
      else List(basicenumerables("PositivePatternCall"))
    case Compare(EqComparator, lhs, rhs) => List(basicdeferred("Equality"))
    case Compare(NeqComparator, lhs, rhs) => List(basicdeferred("Inequality"))
    case Concept(v, typ) =>
      val clazzName = typ match {
        case TType(NodeType(_)) => Some(nNodeType)
        case TType(ListType(_)) => Some(nListType)
        case _ => None
      }
      List(basicenumerables("TypeConstraint")) ++ (if (clazzName.isDefined) List(metaelements(clazzName.get)) else Nil)
    case Path(src, trg, link, typ) =>
      val linkImport = link match {
        case NamedLink(typ, field) => metaelements(nNamedLink)
        case ListFirstLink(typ) => metaelements(nListFirstLink)
        case ListNextLink() => metaelements(nListNextLink)
        case DefinedNodeLink(typ, field) => throw new IllegalArgumentException("TODO support defined node link")
        case ParentLink => virtual("ParentKey")
      }
      List(basicenumerables("TypeConstraint"), linkImport)
    case Check(code) => List()
  }

  private def basicdeferred(clazz: String): Stat =
    q"import org.eclipse.viatra.query.runtime.matchers.psystem.basicdeferred.${Name.Indeterminate(clazz)}"

  private def basicenumerables(clazz: String): Stat =
    q"import org.eclipse.viatra.query.runtime.matchers.psystem.basicenumerables.${Name.Indeterminate(clazz)}"

  private def metaelements(clazz: Name.Indeterminate): Stat =
    q"import inca.MetaElements.$clazz"

  private def virtual(clazz: String): Stat =
    q"import inca.backend.virtual.${Name.Indeterminate(clazz)}"
}
