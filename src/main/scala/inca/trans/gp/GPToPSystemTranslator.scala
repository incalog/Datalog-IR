package inca.trans.gp

import inca.MetaElements.{Link, ListElementsLink, NamedLink, NodeType, ParentLink, PrimitiveType}
import inca.backend.indices.InputKey
import inca.backend.indices.InputKey.{LinkKey, NodeTypeKey, PrimitiveKey}
import inca.backend.virtual.ListElementsKey
import inca.lang.GraphPatternLang
import inca.lang.GraphPatternLang._
import inca.util.Gensym

import scala.meta._

class GPToPSystemTranslator(analysis: Seq[Object]) {
  val PARAMPREFIX = "param_"
  val VARPREFIX = "var_"
  val LITPREFIX = "lit_"

  val tNodeType = Term.Name(classOf[NodeType].getSimpleName)
  val tPrimitiveType = Term.Name(classOf[PrimitiveType].getSimpleName)
  val tNamedLink = Term.Name(classOf[NamedLink].getSimpleName)
  val tParentLink = Term.Name("ParentLink")

  val tLinkedTypeKey = Type.Select(Term.Name("InputKey"), Type.Name(classOf[NodeTypeKey].getSimpleName))
  val tLinkKey = Type.Select(Term.Name("InputKey"), Type.Name(classOf[LinkKey].getSimpleName))
  val tPrimitiveKey = Type.Select(Term.Name("InputKey"), Type.Name(classOf[PrimitiveKey].getSimpleName))
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

  def transGraphPattern(pat: GraphPattern): Source = {
    val fileNameType = Type.Name(genQueryClassName(pat.name))
    val fileNameTerm = Term.Name(genQueryClassName(pat.name))
    val fileNameLit = Lit.String(genQueryClassName(pat.name))

    val superClassParam = Init(
      Type.Name("TFQuerySpecification"),
      Name.Anonymous(),
      List(List(q"$fileNameTerm.GeneratedPQuery.INSTANCE")))

    val paramNames = pat.params.map(_.name)
    val paramTermNames = paramNames.map { n => Term.Name(s"$PARAMPREFIX${n}") }
    val paramLitName = paramNames.map { n => Lit.String(n) }
    val allVars = CollectGPVars(pat).toSet
    val gensym = new Gensym(allVars)

    // TODO there is a hard coded package for the resulting class
    source"""
      package inca.trans.generated

      ..${DeriveImportStatements(pat)}

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

                        ..${(CollectGPVars.transAlternative(body).distinct.diff(paramNames)).map(genTempVar).toList}
                        ..${CollectGPLits.transAlternative(body).distinct.map(genLiteralVar(_)(gensym)).toList}
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
    val pparam =
      if (param.typ.isDefined) {
        val qualifiedName = genType(param.typ.get).value
        val key = genInputKey(param.typ.get)
        q"new PParameter(${Lit.String(param.name)}, ${qualifiedName}, $key)"
      } else {
        q"new PParameter(${Lit.String(param.name)})"
      }
    q"private val ${Pat.Var(Term.Name(s"$PARAMPREFIX${param.name}"))}: PParameter = $pparam"
  }

  def genInputKey(typ: GraphPatternLang.Type): Term = typ match {
    case TBool => q"new $tPrimitiveKey($tPrimitiveType(${Lit.String("java.lang.Boolean")}))"
    case TInt => q"new $tPrimitiveKey($tPrimitiveType(${Lit.String("java.lang.Integer")}))"
    case TLong => q"new $tPrimitiveKey($tPrimitiveType(${Lit.String("java.lang.Long")}))"
    case TDouble => q"new $tPrimitiveKey($tPrimitiveType(${Lit.String("java.lang.Double")}))"
    case TString => q"new $tPrimitiveKey($tPrimitiveType(${Lit.String("java.lang.String")}))"
    case TType(wrapped) => q"new $tLinkedTypeKey($tNodeType(${genType(wrapped)}))"
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
    if (param.typ.isDefined && param.typ.get.isInstanceOf[TType])
      Some(q"""new TypeConstraint(
            body,
            Tuples.flatTupleOf(${Term.Name(s"$VARPREFIX${param.name}")}),
            new $tLinkedTypeKey($tNodeType(${genType(param.typ.get)})))""")
    else None
  }

  def genConstraints(constraint: Constraint): Seq[Stat] = constraint match {
    case Composition(call, neg) =>
      val patternQueryName = genQueryClassName(call.name)
      val args = q"Tuples.flatTupleOf(..${call.args.map(transValue).toList})"
      val callQuery = q"${Term.Name(patternQueryName)}.instance().getInternalQueryRepresentation()"
      if (neg) Seq(q"new NegativePatternCall(body, $args, $callQuery)")
      else
        if (call.transitive)
          Seq(q"new BinaryTransitiveClosure(body, $args, $callQuery)")
        else
          Seq(q"new PositivePatternCall(body, $args, $callQuery)")
    case Compare(EqComparator, lhs, rhs) =>
      Seq(q"""new Equality(body, ${transValue(lhs)}, ${transValue(rhs)})""")
    case Compare(NeqComparator, lhs, rhs) =>
      Seq(q"""new Inequality(body, ${transValue(lhs)}, ${transValue(rhs)})""")
    case Concept(v: Var, typ) =>
      // TODO if type is not enumerable emit TypeFilterConstraint (only needed when we introduce lattices)
      Seq(q"""new TypeConstraint(
            body,
            Tuples.flatTupleOf(${Term.Name(s"var_${v.name}")}),
            new $tLinkedTypeKey($tNodeType(${genType(typ)})))""")
    case Path(src, trg, link, typ) =>
      Seq(q"""new TypeConstraint(
            body,
            Tuples.staticArityFlatTupleOf(..${List(transValue(src), transValue(trg))}),
            ${genLinkKey(link)})
         """)
    // TODO
    case Check(code) => Seq()
  }

  def genLinkKey(link: Link): Term = link match {
    case ParentLink =>
      q"ParentKey"
    case ListElementsLink() =>
      q"ListElementsKey"
    case NamedLink(nodeType, fld)  =>
      q"new $tLinkKey($tNamedLink($tNodeType(${nodeType.name}), ${fld}))"
    case _ => throw new IllegalArgumentException("Does not support such a link")
  }

  def transValue(v: Value): Term = v match {
    case Var(name) => Term.Name(s"$VARPREFIX$name")
    case Constant(lit) => Term.Name(s"$LITPREFIX${genLiteralVarName(lit)}")
  }

  private def genType(typ: GraphPatternLang.Type): Lit.String = typ match {
    case TBool => genPrimitiveType("Boolean")
    case TInt => genPrimitiveType("Integer")
    case TLong => genPrimitiveType("Long")
    case TDouble => genPrimitiveType("Double")
    case TString => genPrimitiveType("String")
    case TType(wrapped) => Lit.String(wrapped.name)
  }

  private def genPrimitiveType(name: String): Lit.String = Lit.String(s"java.lang.${name}")

}
