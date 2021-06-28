package inca.backend.souffle

import inca.backend.hints.DataHints.{DataType, Selector, SelectorKey}
import inca.backend.hints.{DataHints, MagicSetHints}
import inca.backend.ir.Datalog
import inca.backend.ir.Datalog.BodyMustFail
import inca.frontend.functional
import inca.frontend.functional.core.{DataConstructor, DataDef, TData}
import inca.frontend.souffle.Syntax._
import inca.runtime.context.DataModel
import inca.util.TupleOps
import truechange.{AnyType, SortType}

class GenerateSouffle(dataModel: DataModel) {

  // store name and number of inputs
  private var namedExtensionalRelations: Set[(String, Int)] = Set()
  private var relationDecls: Set[RuleSignature] = Set()

  def hasTypeRel(name: String): String =
    "hasType$" + name
  def pathRel(typeName: String, field: String): String =
    s"path$$${typeName}_$field"

  def compileModule(module: Datalog.Module, datas: Seq[DataDef]): String = {
    val types = datas.map(compileDataDef)
    val rels = module.pats.flatMap(compilePattern)
    val namedExtRels = namedExtensionalRelations.flatMap { case (n, i) => generateExtensionalRelation(n, i) }
    val dataModelRels = compileDataModel(dataModel)
    s"""
       |${types.mkString("\n")}
       |
       |${rels.mkString("\n")}
       |
       |${namedExtRels.mkString("\n")}
       |
       |${dataModelRels.mkString("\n")}
       |""".stripMargin
  }

  def getDataTypeOfCotr(ty: String, model: DataModel): String = {
    // we are interested in the most precise (direct) supertype
    val supertypes = model.directNodeSupertypes.get(truechange.SortType(ty))

    if (supertypes.size > 1) {
      throw new IllegalArgumentException(s"Type ${ty} has more than one supertype: ${supertypes.mkString(", ")}")
    }
    supertypes.headOption match {
      case Some(truechange.SortType(sup)) => sup
      case None => ty
    }
  }

  def compileDataModel(model: DataModel): Seq[SouffleContent] = {
    val tyRels = model.types.toSeq.flatMap { case truechange.SortType(name) =>
      val dataType = getDataTypeOfCotr(name, model)
      val sig = RuleSignature(hasTypeRel(name), Seq(RuleParameter("out", DeclaredType(dataType))), false)
      val input = Input(hasTypeRel(name), "", "")
      Seq(sig, input)
    }
    val linkRels=  model.links.flatMap { case ((srcTy, field), trgTy) =>
      val dataType = getDataTypeOfCotr(srcTy, model)
      val sig = RuleSignature(pathRel(srcTy, field),
        Seq(
          RuleParameter("out", DeclaredType(dataType)),
          RuleParameter("field", DeclaredType(trgTy.asInstanceOf[SortType].name))), false)
      val input = Input(pathRel(srcTy, field), "", "")
      Seq(sig, input)
    }
    val litLinkRels=  model.litLinks.flatMap { case ((srcTy, field), trgTy) =>
      val dataType = getDataTypeOfCotr(srcTy, model)
      val sig = RuleSignature(pathRel(srcTy, field),
        Seq(
          RuleParameter("out", DeclaredType(dataType)),
          RuleParameter("field", compileLitTruechangeType(trgTy))), false)
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


    val decl = RuleSignature(pat.name, pat.params.map(p => RuleParameter(p.name, compileType(p.typ))), output = false)
    relationDecls += decl
//    val nonExtensionalBodies = pat.bodies.filter(!_.atoms.exists(_.isInstanceOf[Datalog.Path]))
    val rules = pat.bodies.flatMap { body =>
      val head = RuleHead(pat.name, pat.params.map(p => Variable(p.name)))

      val atoms = body.atoms.map(compileAtom)
      for (alt <- TupleOps.cartesianProduct(atoms))
        yield RuleDefinition(Seq(head), alt.flatten)
    }
    val output = if (pat.hasHint(MagicSetHints.MainKey)) {
      Seq(Output(pat.name))
    } else Seq()
    decl +: (output ++ rules)
  }


  def compileAtom(atom: Datalog.Atom): Seq[Seq[Statement]] = atom match {
    case Datalog.Call(name, args, _, neg) =>
      Seq(Seq(RuleApplication(neg, None, name, args.map(compileTerm))))
    case Datalog.ExtensionalCall(name, args, neg) =>
      if (args.isEmpty) {
        Seq(Seq())
      } else {
        namedExtensionalRelations += name -> args.size
        Seq(Seq(RuleApplication(neg, None, name, args.map(compileTerm))))
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
          Seq(Seq(RuleApplication(false, None, hasTypeRel(typeName), Seq(compileTerm(t)))))
        case _ => throw new IllegalArgumentException(s"Do not suppport HasType of non-node type in $atom")
      }
    case Datalog.Path(src, srcTy, link, trg, trgTy) =>
      link match {
        case Datalog.NamedLink(Datalog.TNode(typeName), field) =>
          Seq(Seq(RuleApplication(false, None, pathRel(typeName, field), Seq(compileTerm(src), compileTerm(trg)))))
        case _ => throw new IllegalArgumentException(s"Only NamedLink paths are supported, in $atom")
      }
    case Datalog.NotHasType(t, typ) => throw new IllegalArgumentException(s"NotHasType not supported yet in $atom")
    case Datalog.NoPath(t, ty, link, termIsSource) => throw new IllegalArgumentException(s"NoPath not supported yet in $atom")
    case Datalog.Undef(t) => throw new IllegalArgumentException(s"Undef is not supported yet in $atom")
    case _ => Seq()
  }

  def compileScalaTerm(term: meta.Term)(implicit subst: Map[String, Datalog.Term]): Seq[(Seq[Statement], Expression)] = term match {
    case meta.Term.Name(name) => Seq((Seq(), compileTerm(subst(name))))
    case meta.Lit.Int(v) => Seq((Seq(), NumberValue(v)))
    case meta.Lit.Boolean(true) => Seq((Seq(), NumberValue(1)))
    case meta.Lit.Boolean(false) => Seq((Seq(), NumberValue(0)))
    case meta.Lit.String(v) => Seq((Seq(), StringValue(v)))
    case meta.Lit.Double(v) => Seq((Seq(), FloatValue(v.toFloat)))

    case meta.Term.Apply(fun, meta.Lit.String(name) :: args) if fun.syntax == "inca.runtime.data.DataURI" =>
      val compiledArgs = args.map {
        case meta.Term.Name(n) => Variable(n)
        case _ => throw new IllegalArgumentException("DataURI can only have variables as input")
      }
      Seq((Seq(), ADTValue(name, compiledArgs)))

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
    case Datalog.Var(name) => Variable(name)
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
    case Datalog.TData(name) => DeclaredType(name)
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
      DeclaredType(ty.hints(DataHints.DataTypeNameKey).asInstanceOf[DataHints.DataTypeName].name)
    case _ => throw new IllegalArgumentException(ty.toString)
  }

  def generateExtensionalRelation(name: String, numArgs: Int): Seq[SouffleContent] = {
    relationDecls.find(s => name == "ext_input$" + s.name) match {
      case Some(decl) =>
        Seq(
          RuleSignature(name, decl.parameters.take(numArgs), false),
          Input(name, "", "")
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
