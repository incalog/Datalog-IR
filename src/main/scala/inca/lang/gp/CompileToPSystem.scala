package inca.lang.gp

import inca.lang.gp.GP._
import inca.runtime.index.MetaElements.{ListType, NamedLink, NodeType, PrimitiveType}
import inca.runtime.index.{LinkKey, NodeTypeKey, PrimitiveTypeKey}
import inca.runtime.virtual.list.ListNextKey
import inca.runtime.virtual.tree.ParentKey
import inca.util.Gensym
import inca.util.Meta._

import scala.meta._

class CompileToPSystem(analysis: Seq[Object]) {
  val PARAMPREFIX = "param_"
  val VARPREFIX = "var_"
  val LITPREFIX = "lit_"

  val tyNodeTypeKey = typeOf[NodeTypeKey]
  val tyPrimitiveKey = typeOf[PrimitiveTypeKey]
  val tyLinkKey = typeOf[LinkKey]

  val oParentKey = objectOf(ParentKey)
  val oNextKey = objectOf(ListNextKey)

  val tNodeType = symbolOf[NodeType]
  val tListType = symbolOf[ListType]
  val tPrimitiveType = symbolOf[PrimitiveType]

  val tNamedLink = symbolOf[NamedLink]

  // TODO transform module
  type Analysis = Seq[Object]
  val modules = analysis.collect { case m: Module => m }
  val funToModule = modules.flatMap { m => m.pats.map { gp => gp.name -> m.name } }.toMap

  def transAnalysis(): Seq[Source] = {
    //TODO What is the exact visibiltity?
    modules.flatMap(transModule)
  }

  def transModule(module: Module): Seq[Source] = {
    module.pats.map(transGraphPattern)
  }

  def transGraphPattern(pat: Rule): Source = {
    val fileNameType = Type.Name(genQueryClassName(pat.name))
    val fileNameTerm = Term.Name(genQueryClassName(pat.name))
    val fileNameLit = Lit.String(genQueryClassName(pat.name))

    val superClassParam = Init(
      Type.Name("IncaQuerySpecification"),
      Name.Anonymous(),
      List(List(q"$fileNameTerm.GeneratedPQuery.INSTANCE")))

    val paramNames = pat.params.map(_.name)
    val paramTermNames = paramNames.map { n => Term.Name(s"$PARAMPREFIX${n}") }
    val paramLitName = paramNames.map { n => Lit.String(n) }
    val allVars = CollectVars(pat).toSet
    val gensym = new Gensym(allVars)

    // TODO there is a hard coded package for the resulting class
    source"""
      package inca.trans.generated

      import org.eclipse.viatra.query.runtime.api.{GenericPatternMatcher, ViatraQueryEngine}
      import org.eclipse.viatra.query.runtime.api.scope.{QueryScope => ViatraQueryScope}
      import org.eclipse.viatra.query.runtime.matchers.psystem.{PBody, PVariable}
      import org.eclipse.viatra.query.runtime.matchers.psystem.queries.{BasePQuery, PParameter, PVisibility}
      import org.eclipse.viatra.query.runtime.matchers.tuple.Tuples
      import org.eclipse.viatra.query.runtime.matchers.psystem.basicdeferred.ExportedParameter

      import java.util

      import inca.runtime.IncaQuerySpecification
      import inca.runtime.context.QueryScope
      import inca.runtime.index.InputKey
      import inca.runtime.virtual.tree.ParentKey
      import inca.runtime.virtual.list.ListNextKey

      import org.eclipse.viatra.query.runtime.matchers.psystem.basicdeferred._
      import org.eclipse.viatra.query.runtime.matchers.psystem.basicenumerables._
      import inca.runtime.index.MetaElements._

      class $fileNameType extends $superClassParam {
         override def instantiate(engine: ViatraQueryEngine): GenericPatternMatcher = {
            var matcher: GenericPatternMatcher = engine.getExistingMatcher(this)
            if (matcher == null) matcher = engine.getMatcher(this)
            matcher
         }
         override def getPreferredScopeClass: Class[_ <: ViatraQueryScope] = classOf[QueryScope]
      }

      object $fileNameTerm {
        def instance(): $fileNameType = LazyHolder.INSTANCE

        private final object LazyHolder {
          val INSTANCE: $fileNameType = make()
          def make(): $fileNameType = new $fileNameType()
        }


        private final object GeneratedPQuery extends BasePQuery(PVisibility.PUBLIC) {
          val INSTANCE: GeneratedPQuery.type = this
          ..${pat.params.map(genPParam).toList}
          {}
          override protected def doGetContainedBodies(): util.Set[PBody] = {
            val bodies: util.Set[PBody] = util.Set.of(
              ..${pat.bodies.map { body =>
                    q"""{
                        val body: PBody = new PBody(this)
                        ..${pat.params.map(genBodyParam).toList}
                        ()
                        val exportedParams = new util.ArrayList[ExportedParameter]()
                        ..${pat.params.map { param =>
                              q"""exportedParams.add(new ExportedParameter(body,
                                ${Term.Name(s"$VARPREFIX${param.name}")},
                                ${Term.Name(s"$PARAMPREFIX${param.name}")}))"""
                          }.toList
                        }
                        body.setSymbolicParameters(exportedParams)

                        ..${(CollectVars.transAlternative(body).distinct.diff(paramNames)).map(genTempVar).toList}
                        ..${CollectLits.transAlternative(body).distinct.map(genLiteralVar(_)(gensym)).toList}
                        ..${pat.params.flatMap(genParamConstraint).toList}
                        ..${body.constraints.flatMap(genConstraints).toList}
                        body
                      }"""
                  }.toList
              }
            )
            bodies
          }

          override def getFullyQualifiedName: String = $fileNameLit
          override def getParameters: util.List[PParameter] = util.List.of(..${paramTermNames.toList})
          override def getParameterNames: util.List[String] = util.List.of(..${paramLitName.toList})
        }
    }"""
  }

  def genQueryClassName(patName: String): String = {
    val containingModuleName = funToModule(patName)
    containingModuleName + "_" + patName + "QuerySpecification"
  }

  def genPParam(param: Param): Stat = {
    val pparam = param.typ match {
      case Some(typ) =>
        val gentyp = genType(typ)
        val key = genInputKey(typ)
        q"new PParameter(${Lit.String(param.name)}, $gentyp.toString, $key)"
      case None =>
        q"new PParameter(${Lit.String(param.name)})"
    }
    q"private val ${Pat.Var(Term.Name(s"$PARAMPREFIX${param.name}"))}: PParameter = $pparam"
  }

  def genInputKey(typ: GP.TypeAnno): meta.Term = {
    val gentyp = genType(typ)
    typ match {
      case TBool | TInt | TLong | TDouble | TString => q"new $tyPrimitiveKey($gentyp)"
      case _:TNode | _:TList => q"new $tyNodeTypeKey($gentyp)"
    }
  }


  def genBodyParam(param: Param): Stat =
    q"""val ${Pat.Var(Term.Name(VARPREFIX + param.name))}: PVariable =
          body.getOrCreateVariableByName(${Lit.String(param.name)})"""

  def genTempVar(name: String): Stat =
    q"val ${Pat.Var(Term.Name(VARPREFIX + name))}: PVariable = body.getOrCreateVariableByName(${Lit.String(name)})"

  def genLiteralVar(lit: Literal)(implicit gensym: Gensym): Stat = {
    val varName = genLiteralVarName(lit)
    q"val ${Pat.Var(Term.Name(LITPREFIX + varName))} = body.newConstantVariable(${genLiteral(lit)})"
  }

  def genLiteralVarName(lit: Literal): String = lit match {
    case IntLiteral(v) => "int" + v.hashCode()
    case LongLiteral(v) => "long" + v.hashCode()
    case DoubleLiteral(v) => "double" + v.hashCode()
    case StringLiteral(v) => "string" + v.hashCode
    case BooleanLiteral(v) => "boolean" + v.hashCode()
  }

  def genLiteral(lit: Literal): Lit = lit match {
    case IntLiteral(v) => Lit.Int(v)
    case LongLiteral(v) => Lit.Long(v)
    case DoubleLiteral(v) => Lit.Double(v)
    case StringLiteral(v) => Lit.String(v)
    case BooleanLiteral(v) => Lit.Boolean(v)

  }

  def genParamConstraint(param: Param): Option[Stat] = {
    // TODO only emit constraint for nodetypes?
    if (param.typ.isDefined && param.typ.get.isInstanceOf[TNode])
      Some(q"""new TypeConstraint(
            body,
            Tuples.flatTupleOf(${Term.Name(s"$VARPREFIX${param.name}")}),
            new $tyNodeTypeKey(${genType(param.typ.get)}))""")
    else None
  }

  def genConstraints(constraint: Atom): Seq[Stat] = constraint match {
    case Call(name, qargs, transitive, neg) =>
      val patternQueryName = genQueryClassName(name)
      val args = q"Tuples.flatTupleOf(..${qargs.map(transValue).toList})"
      val callQuery = q"${Term.Name(patternQueryName)}.instance().getInternalQueryRepresentation()"
      if (neg) Seq(q"new NegativePatternCall(body, $args, $callQuery)")
      else
        if (transitive)
          Seq(q"new BinaryTransitiveClosure(body, $args, $callQuery)")
        else
          Seq(q"new PositivePatternCall(body, $args, $callQuery)")
    case Compare(EqComparator, lhs, rhs) =>
      Seq(q"""new Equality(body, ${transValue(lhs)}, ${transValue(rhs)})""")
    case Compare(NeqComparator, lhs, rhs) =>
      Seq(q"""new Inequality(body, ${transValue(lhs)}, ${transValue(rhs)})""")
    case HasType(v: Var, typ) =>
      // TODO if type is not enumerable emit TypeFilterConstraint (only needed when we introduce lattices)
      Seq(q"""new TypeConstraint(
            body,
            Tuples.flatTupleOf(${Term.Name(s"var_${v.name}")}),
            new $tyNodeTypeKey(${genType(typ)}))""")

    case Path(src, trg, link) =>
      val key = genLinkKey(link)
      Seq(q"new TypeConstraint(body, Tuples.staticArityFlatTupleOf(${transValue(src)}, ${transValue(trg)}), $key)")

    case Native(code) => dialects.Sbt1(code).parse[Source].get.stats
  }

  def genLinkKey(link: Link): meta.Term = link match {
    case GP.ParentLink => oParentKey
    case GP.NextLink => oNextKey
    case GP.NamedLink(TNode(name), field) => q"new $tyLinkKey($tNamedLink($tNodeType($name), $field))"
  }

  def transValue(v: GP.Term): meta.Term = v match {
    case Var(name) => Term.Name(s"$VARPREFIX$name")
    case Constant(lit) => Term.Name(s"$LITPREFIX${genLiteralVarName(lit)}")
  }

  private def genType(typ: GP.TypeAnno): meta.Term = typ match {
    case TBool => genPrimitiveType("Boolean")
    case TInt => genPrimitiveType("Integer")
    case TLong => genPrimitiveType("Long")
    case TDouble => genPrimitiveType("Double")
    case TString => genPrimitiveType("String")
    case TNode(name) => q"$tNodeType($name)"
    case TList(ty) =>
      val tygen = genType(ty)
      q"$tListType($tygen)"
  }

  private def genPrimitiveType(name: String): meta.Term = q"$tPrimitiveType(${Lit.String("java.lang." + name)})"

}
