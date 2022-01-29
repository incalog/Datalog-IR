package inca.backend.souffle

import inca.backend.hints.DataHints.{DataType, Selector, SelectorKey}
import inca.backend.hints.{DataHints, MagicSetHints}
import inca.backend.ir.Datalog
import inca.backend.ir.Datalog.BodyMustFail
import inca.backend.souffle.GenerateSouffle.{hasTypeRel, pathRel}
import inca.frontend.functional
import inca.frontend.functional.core.{DataConstructor, DataDef, TData}
import inca.frontend.souffle.Syntax._
import inca.runtime.context.DataModel
import inca.runtime.data.MockURI
import inca.util.{Scala, TupleOps}
import truechange.{AnyType, Attach, Detach, EditScript, Load, NamedLink, NamedTag, RootLink, SortType, URI, Unload, Update}
import truediff.Diffable

class GenerateSouffle(dataModel: DataModel) {

  // store name and number of inputs
  private var namedExtensionalRelations: Set[(String, Int)] = Set()
  private var relationDecls: Set[RuleSignature] = Set()


  def compileModule(module: Datalog.Module, datas: Seq[DataDef]): (String, Seq[Name]) = {
    val types = datas.map(compileDataDef)
    val rels = module.pats.flatMap(compilePattern)
    val namedExtRels = namedExtensionalRelations.flatMap { case (n, i) => generateExtensionalRelation(n, i) }
    val dataModelRels = compileDataModel(dataModel)
    val source = s"""
       |${types.mkString("\n")}
       |
       |${rels.mkString("\n")}
       |
       |${namedExtRels.mkString("\n")}
       |
       |${dataModelRels.mkString("\n")}
       |""".stripMargin
    val inputs = (rels ++ namedExtRels ++ dataModelRels).collect {
      case Input(rule, _, _) => rule
    }
    (source, inputs)
  }

  def getDataTypeOfCotr(ty: String, model: DataModel): Name = {
    // we are interested in the most precise (direct) supertype
    val supertypes = model.directNodeSupertypes.get(truechange.SortType(ty))

    if (supertypes.size > 1) {
      throw new IllegalArgumentException(s"Type ${ty} has more than one supertype: ${supertypes.mkString(", ")}")
    }
    supertypes.headOption match {
      case Some(truechange.SortType(sup)) => Name(sup)
      case None => Name(ty)
    }
  }

  def compileDataModel(model: DataModel): Seq[SouffleContent] = {
    val tyRels = model.types.toSeq.flatMap { case truechange.SortType(name) =>
      val dataType = getDataTypeOfCotr(name, model)
      val sig = RuleSignature(hasTypeRel(name), Seq(RuleParameter(Name("out"), DeclaredType(dataType))), false)
      val input = Input(hasTypeRel(name), "", "")
      Seq(sig, input)
    }
    val linkRels=  model.links.flatMap { case ((srcTy, field), trgTy) =>
      val dataType = getDataTypeOfCotr(srcTy, model)
      val sig = RuleSignature(pathRel(srcTy, field),
        Seq(
          RuleParameter(Name("out"), DeclaredType(dataType)),
          RuleParameter(Name("field"), DeclaredType(Name(trgTy.asInstanceOf[SortType].name)))), false)
      val input = Input(pathRel(srcTy, field), "", "")
      Seq(sig, input)
    }
    val litLinkRels=  model.litLinks.flatMap { case ((srcTy, field), trgTy) =>
      val dataType = getDataTypeOfCotr(srcTy, model)
      val sig = RuleSignature(pathRel(srcTy, field),
        Seq(
          RuleParameter(Name("out"), DeclaredType(dataType)),
          RuleParameter(Name("field"), compileLitTruechangeType(trgTy))), false)
      val input = Input(pathRel(srcTy, field), "", "")
      Seq(sig, input)
    }
    tyRels ++ linkRels ++ litLinkRels
  }

  def compileLitTruechangeType(ty: truechange.LitType): Type = ty match {
    case lit if lit == Datalog.TLiteral.Bool.litType => UnsignedType
    case lit if lit == Datalog.TLiteral.Int.litType => NumberType
    case lit if lit == Datalog.TLiteral.Long.litType => NumberType
    case lit if lit == Datalog.TLiteral.Double.litType => FloatType
    case lit if lit == Datalog.TLiteral.String.litType => SymbolType
  }

  def compileDataDef(data: DataDef): String = {
    val constructors = data.constrs.map { case DataConstructor(name, paramTypes) =>
      val params = paramTypes.zipWithIndex.map(p => s"x${p._2}:${compileFunType(p._1)}")
      s"$name {${params.mkString(",")}}"
    }

    s""".type ${data.name} =
       |  ${constructors.mkString(" | ")}
       |""".stripMargin
  }
  def compileFunType(ty: functional.core.Type): String = ty match {
    case TData(name) => name.name
    case functional.core.TScalaBoolean => "unsigned"
    case functional.core.TScalaInt => "number"
    case functional.core.TScalaDouble => "float"
    case functional.core.TScalaString => "symbol"
    case _ => throw new IllegalArgumentException(ty.toString)
  }

  def compilePattern(pat: Datalog.Pattern): Seq[SouffleContent] = {
    if (pat.hasHint(DataType.key))
      return Seq()
    val patname = Name(pat.name)

    val decl = RuleSignature(patname, pat.params.map(p => RuleParameter(Name(p.name), compileType(p.typ))), output = false)
    relationDecls += decl
//    val nonExtensionalBodies = pat.bodies.filter(!_.atoms.exists(_.isInstanceOf[Datalog.Path]))
    val rules = pat.bodies.flatMap { body =>
      val head = RuleHead(patname, pat.params.map(p => Variable(Name(p.name))))

      val atoms = body.atoms.map(compileAtom)
      for (alt <- TupleOps.cartesianProduct(atoms))
        yield RuleDefinition(Seq(head), RuleBody(alt.flatten))
    }
    val output = if (pat.hasHint(MagicSetHints.MainKey)) {
      Seq(Output(patname))
    } else Seq()
    decl +: (output ++ rules)
  }


  def compileAtom(atom: Datalog.Atom): Seq[Seq[Statement]] = atom match {
    case Datalog.Call(name, args, _, neg) =>
      Seq(Seq(RelationApplication(neg, None, Name(name), args.map(compileTerm))))
    case Datalog.ExtensionalCall(name, args, neg) =>
      if (args.isEmpty) {
        Seq(Seq())
      } else {
        namedExtensionalRelations += name -> args.size
        Seq(Seq(RelationApplication(neg, None, Name(name), args.map(compileTerm))))
      }
    case Datalog.Compare(Datalog.EqComparator, lhs, rhs) =>
      Seq(Seq(Equality(compileTerm(lhs), false, compileTerm(rhs))))
    case Datalog.Compare(Datalog.NeqComparator, lhs, rhs) =>
      Seq(Seq(Equality(compileTerm(lhs), true, compileTerm(rhs))))
    case Datalog.Computed(lhs, computation) =>
      compileComputation(computation).flatMap {
        case (statements, term) =>
          try { Some(statements ++ mkEquality(compileTerm(lhs), term)) }
          catch { case BodyMustFail => None }
      }
    case Datalog.HasType(t, typ) =>
      typ match {
        case Datalog.TNode(typeName) =>
          Seq(Seq(RelationApplication(false, None, hasTypeRel(typeName), Seq(compileTerm(t)))))
        case _ => throw new IllegalArgumentException(s"Do not suppport HasType of non-node type in $atom")
      }
    case Datalog.Path(src, srcTy, link, trg, trgTy) =>
      link match {
        case Datalog.NamedLink(Datalog.TNode(typeName), field) =>
          Seq(Seq(RelationApplication(false, None, pathRel(typeName, field), Seq(compileTerm(src), compileTerm(trg)))))
        case _ => throw new IllegalArgumentException(s"Only NamedLink paths are supported, in $atom")
      }
    case Datalog.NotHasType(t, typ) => throw new IllegalArgumentException(s"NotHasType not supported yet in $atom")
    case Datalog.NoPath(t, ty, link, termIsSource) => throw new IllegalArgumentException(s"NoPath not supported yet in $atom")
    case Datalog.Undef(t) => throw new IllegalArgumentException(s"Undef is not supported yet in $atom")
    case _ => Seq()
  }

  import meta.quasiquotes._
  def compileScalaTerm(term: meta.Term)(implicit subst: Map[String, Datalog.Term]): Seq[(Seq[Statement], Expression)] = term match {
    case meta.Term.Name(name) => Seq((Seq(), compileTerm(subst(name))))
    case meta.Lit.Int(v) => Seq((Seq(), NumberValue(v)))
    case meta.Lit.Boolean(true) => Seq((Seq(), NumberValue(1)))
    case meta.Lit.Boolean(false) => Seq((Seq(), NumberValue(0)))
    case meta.Lit.String(v) => Seq((Seq(), StringValue(v)))
    case meta.Lit.Double(v) => Seq((Seq(), FloatValue(v.toFloat)))

    case meta.Term.Apply(fun, meta.Lit.String(name) :: q"Seq(..$args)" :: Nil) if fun.syntax == Scala.symbolOf[MockURI].syntax =>
      val compiledArgs = args.map {
        case meta.Term.Name(n) => Variable(Name(n))
        case _ => throw new IllegalArgumentException(s"MockURI can only have variables as input: $term")
      }
      Seq((Seq(), ADTValue(Name(name), compiledArgs)))

    case meta.Term.ApplyInfix(e1, meta.Term.Name(op), _, e2::Nil) if builtInFunction.isDefinedAt(op) =>
      for ((cons1, arg1) <- compileScalaTerm(e1);
           (cons2, arg2) <- compileScalaTerm(e2))
        yield (cons1 ++ cons2, BuiltInFunctionCall(builtInFunction(op), Seq(arg1, arg2)))

    case meta.Term.ApplyInfix(e1, meta.Term.Name(op), _, e2::Nil) if builtInComparator.isDefinedAt(op) =>
      val (posCompare, negCompare) = builtInComparator(op)
      for ((cons1, arg1) <- compileScalaTerm(e1);
           (cons2, arg2) <- compileScalaTerm(e2);
           truth <- Seq(true, false))
        yield if (truth)
            (cons1 ++ cons2 :+ posCompare(arg1, arg2), NumberValue(1))
          else
            (cons1 ++ cons2 :+ negCompare(arg1, arg2), NumberValue(0))
  }

  def builtInFunction: PartialFunction[String, BuiltInFunction] = {
    case "+" => AddBuiltInFunction
    case "-" => SubBuiltInFunction
    case "*" => MultBuiltInFunction
    case "/" => DivBuiltInFunction
    case "^" => PowBuiltInFunction
    case "%" => ModBuiltInFunction
  }

  type Comparator = (Expression, Expression) => Statement
  def builtInComparator: PartialFunction[String, (Comparator, Comparator)] = {
    case ">" => (GreaterThan, LesserThanEqual)
    case ">=" => (GreaterThanEqual, LesserThan)
    case "<" => (LesserThan, GreaterThanEqual)
    case "<=" => (LesserThanEqual, GreaterThan)
    case "==" => (Equality(_, false, _), Equality(_, true, _))
    case "!=" => (Equality(_, true, _), Equality(_, false, _))
  }


  def compileComputation(computation: Datalog.Computation): Seq[(Seq[Statement], Expression)] = computation match {
    case Datalog.Evaluation(evalArgs, _, fun) =>
      val evalTerms = evalArgs.map(_._1)
      val funParamNames = fun.tree.params.map(_.name.value)
      implicit val subst: Map[String, Datalog.Term] = funParamNames.zip(evalTerms).toMap
      compileScalaTerm(fun.tree.body)
    case Datalog.CountAggregation(patName, args) =>
      throw new IllegalArgumentException("Count Aggregation not supported yet")
    case Datalog.CustomAggregation(typ, description, agg, patName, args, aggregatedColumn) =>
      throw new IllegalArgumentException("CustomAggregation not supported yet")
  }

  def compileTerm(term: Datalog.Term): Expression = term match {
    case Datalog.Var(name) => Variable(Name(name))
    case Datalog.Constant(lit) => lit match {
      case Datalog.IntLiteral(v) => NumberValue(v)
      case Datalog.LongLiteral(v) => NumberValue(v.toInt)
      case Datalog.DoubleLiteral(v) => FloatValue(v.toFloat)
      case Datalog.StringLiteral(v) => StringValue(v)
      case Datalog.BooleanLiteral(true) => NumberValue(1)
      case Datalog.BooleanLiteral(false) => NumberValue(0)
    }
  }

  def compileType(ty: Datalog.Type): Type = ty match {
    case Datalog.TData(name) => DeclaredType(Name(name))
    case Datalog.TLiteral.Bool => UnsignedType
    case Datalog.TLiteral.Int => NumberType
    case Datalog.TLiteral.Long => NumberType
    case Datalog.TLiteral.Double => FloatType
    case Datalog.TLiteral.String => SymbolType
    case Datalog.TScalaBoolean => UnsignedType
    case Datalog.TScalaInt => NumberType
    case Datalog.TScalaDouble => FloatType
    case Datalog.TScalaString => SymbolType
    case ty if ty.hasHint(DataHints.DataTypeNameKey) =>
      DeclaredType(Name(ty.hints(DataHints.DataTypeNameKey).asInstanceOf[DataHints.DataTypeName].name))
    case _ => throw new IllegalArgumentException(ty.toString)
  }

  def generateExtensionalRelation(name: String, numArgs: Int): Seq[SouffleContent] = {
    relationDecls.find(s => name == "ext_input$" + s.name) match {
      case Some(decl) =>
        Seq(
          RuleSignature(Name(name), decl.parameters.take(numArgs), false),
          Input(Name(name), "", "")
        )
      case None =>
        throw new IllegalArgumentException("There exists no input relation for the extensional relation")
    }
  }

  def mkEquality(e1: Expression, e2: Expression): Option[Equality] = (e1, e2) match {
    case _ if e1 == e2 => None // always true
    case (_: Variable, _) | (Wildcard, _) | (_, _: Variable) | (_, Wildcard) => Some(Equality(e1, false, e2))
    case _ => throw BodyMustFail // no variable involved, but expressions differ => always false
  }
}
object GenerateSouffle {
  def hasTypeRel(name: String): Name =
    Name("hasType$" + name)
  def pathRel(typeName: String, field: String): Name =
    Name(s"path$$${typeName}_$field")
}

object GenerateFacts {

  type Tuple = Seq[Expression]
  type Relation = (Name, Seq[Tuple])
  type EDB = Seq[Relation]

  def apply(terms: Seq[AnyRef], es: EditScript, mainRel: Name): EDB = {
    // store each URI we have seen and store the ADT value the uri represents
    // only interessted in Load and Attachs because we want to build up the EDB and do not support incremental updates ATM
    var uris: Map[URI, Expression] = Map()
    var rels: Seq[(Name, Tuple)] = Seq()
    val edits = es.coreEdits

    def transTerm(t: AnyRef): Expression = t match {
      case uri: URI => uris(uri)
      case lit: Any => transLit(lit)
    }

    edits.foreach {
      case Attach(node, NamedTag(tag), RootLink, null, ptag) => // insert tuple into extensional input relation
//        rels = rels :+ mainRel -> Seq(uris(node))
      case Attach(node, NamedTag(tag), NamedLink(link), parent, ptag) if parent != null =>
        rels = rels :+ pathRel(tag, link) -> Seq(uris(node), uris(parent))
      case Load(node, NamedTag(tag), kids, lits) =>
        // process kids and lits to constructs args
        val args = (0 until (kids.size + lits.size)).map { ix =>
          val name = s"_$ix"
          kids.find(_._1 == name) match {
            case Some((_, uri)) => uris(uri)
            case None => lits.find(_._1 == name) match {
              case Some((_, lit)) => transLit(lit)
              case None => throw new IllegalArgumentException(s"Could not find kid or lit for link _${ix}")
            }
          }
        }
        val adt = ADTValue(Name(tag), args)
        // tuple in has type relation
        rels = rels :+ hasTypeRel(tag) -> Seq(adt)

        // tuples in path relations
        kids.foreach { case (link, uri) =>
          rels = rels :+ pathRel(tag, link) -> Seq(adt, uris(uri))
        }
        lits.foreach { case (link, lit) =>
          rels = rels :+ pathRel(tag, link) -> Seq(adt, transLit(lit))
        }
        uris = uris + (node -> adt)
      case edit => throw new IllegalArgumentException(s"Unsupported edit in ${edit}")
    }

    rels = rels :+ mainRel -> terms.map(transTerm)
    rels.groupBy(_._1).map( kv => kv._1 -> kv._2.map(_._2)).toSeq
  }



  private def transLit(lit: Any): Expression = lit match {
    case v: Integer => NumberValue(v)
    case v: Boolean if v => NumberValue(1)
    case _: Boolean => NumberValue(0)
    case v: String => StringValue(v)
    case v: Double => FloatValue(v.toFloat)
    case v: Long => NumberValue(v.toInt)
    case _ => throw new IllegalArgumentException(s"Unsupported literal in ${lit}")
  }
}