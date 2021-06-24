package inca.backend.souffle

import inca.backend.hints.{DataHints, MagicSetHints}
import inca.backend.hints.DataHints.DataType
import inca.backend.ir.Datalog
import inca.frontend.functional
import inca.frontend.functional.core.{DataConstructor, DataDef, TData}
import inca.frontend.souffle.Syntax._
import inca.runtime.context.DataModel

class GenerateSouffle {

  // store name and number of inputs
  private var extensionalRelations: Set[(String, Int)] = Set()
  private var relationDecls: Set[RuleSignature] = Set()

  private var dataModel: DataModel = _

  def compileModule(module: Datalog.Module, datas: Seq[DataDef], _dataModel: DataModel): String = {
    dataModel = _dataModel
    val types = datas.map(compileDataDef)
    val rels = module.pats.flatMap(compilePattern)
    val extRels = extensionalRelations.flatMap { case (n, i) => generateExtensionalRelation(n, i) }
    s"""
       |${types.mkString("\n")}
       |${rels.mkString("\n")}
       |${extRels.mkString("\n")}
       |""".stripMargin
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
    val rules = pat.bodies.map { body =>
      val head = RuleHead(pat.name, pat.params.map(p => Variable(p.name)))
      val atoms = body.atoms.flatMap(compileAtom)
      RuleDefinition(Seq(head), atoms)
    }
    val output = if (pat.hasHint(MagicSetHints.MainKey)) {
      Seq(Output(pat.name))
    } else Seq()
    decl +: (output ++ rules)
  }


  def compileAtom(atom: Datalog.Atom): Seq[Statement] = atom match {
    case Datalog.Call(name, args, _, neg) =>
      Seq(RuleApplication(neg, None, name, args.map(compileTerm)))
    case Datalog.ExtensionalCall(name, args, neg) =>
      if (args.isEmpty) {
        Seq()
      } else {
        extensionalRelations += name -> args.size
        Seq(RuleApplication(neg, None, name, args.map(compileTerm)))
      }
    case Datalog.Compare(Datalog.EqComparator, lhs, rhs) =>
      Seq(Equality(compileTerm(lhs), false, compileTerm(rhs)))
    case Datalog.Compare(Datalog.NeqComparator, lhs, rhs) =>
      Seq(Equality(compileTerm(lhs), true, compileTerm(rhs)))
    case Datalog.Computed(lhs, computation) => computation match {
      case Datalog.Evaluation(evalArgs, _, fun) =>
        val evalTerms = evalArgs.map(_._1)
        val funParamNames = fun.tree.params.map(_.name.value)
        implicit val subst = funParamNames.zip(evalTerms).toMap
        compileScalaTermToStatement(fun.tree.body, evalArgs.map(_._1), lhs)
      case _ => throw new IllegalArgumentException(s"Not supported ${computation}")
    }
    case Datalog.HasType(t, typ) =>
      val name = typ match {
        case Datalog.TNode(name) =>
          // TODO need to pass t to .input rel application at the last position
          name
        case _ => throw new IllegalArgumentException("Do not suppport HasType of non-node type")
      }
      Seq(RuleApplication(false, None, name, Seq(compileTerm(t))))
    case Datalog.Path(src, srcTy, link, trg, trgTy) =>
      // TODO need to pass trg to .input rel application
      // TODO how do we determine an order? based on link names? _0, _1, _2, out
      throw new IllegalArgumentException("Path not supported yet")
    case Datalog.NotHasType(t, typ) => throw new IllegalArgumentException("NotHasType not supported yet")
    case Datalog.NoPath(t, ty, link, termIsSource) => throw new IllegalArgumentException("NoPath not supported yet")
    case Datalog.Undef(t) => throw new IllegalArgumentException("Undef is not supported yet")
    case _ => Seq()
  }

  def compileScalaTermToStatement(term: meta.Term, args: Seq[Datalog.Term], lhs: Datalog.Term)(implicit subst: Map[String, Datalog.Term]): Seq[Statement] = term match {
    case meta.Term.Apply(fun, meta.Lit.String(name) :: args) if fun.syntax == "inca.runtime.data.DataURI" =>
      val compiledArgs = args.map {
        case meta.Term.Name(n) => Variable(n)
        case _ => throw new IllegalArgumentException("DataURI can only have variables as input")
      }
      val Datalog.Var(vname) = lhs
      Seq(Equality(Variable(vname), false, ADTValue(name, compiledArgs)))
    case meta.Term.ApplyInfix(_, meta.Term.Name("+"), _, _::Nil) =>
      val Datalog.Var(name) = lhs
      Seq(Equality(Variable(name), false,  BuiltInFunctionCall(AddBuiltInFunction, args.map(compileTerm))))
    case meta.Term.ApplyInfix(_, meta.Term.Name("-"), _, _::Nil) =>
      val Datalog.Var(name) = lhs
      Seq(Equality(Variable(name), false,  BuiltInFunctionCall(SubBuiltInFunction, args.map(compileTerm))))
    case meta.Term.ApplyInfix(_, meta.Term.Name("*"), _, _::Nil) =>
      val Datalog.Var(name) = lhs
      Seq(Equality(Variable(name), false,  BuiltInFunctionCall(MultBuiltInFunction, args.map(compileTerm))))
    case meta.Term.ApplyInfix(_, meta.Term.Name("/"), _, _::Nil) =>
      val Datalog.Var(name) = lhs
      Seq(Equality(Variable(name), false,  BuiltInFunctionCall(DivBuiltInFunction, args.map(compileTerm))))
    case meta.Term.ApplyInfix(_, meta.Term.Name("%"), _, _::Nil) =>
      val Datalog.Var(name) = lhs
      Seq(Equality(Variable(name), false,  BuiltInFunctionCall(ModBuiltInFunction, args.map(compileTerm))))
    case meta.Term.ApplyInfix(_, meta.Term.Name("=="), _, _::Nil) =>
      val Datalog.Constant(Datalog.BooleanLiteral(bool)) = lhs
      Seq(Equality(compileTerm(args(0)), !bool, compileTerm(args(1))))
    case meta.Term.ApplyInfix(_, meta.Term.Name("!="), _, _::Nil) =>
      val Datalog.Constant(Datalog.BooleanLiteral(bool)) = lhs
      Seq(Equality(compileTerm(args(0)), bool, compileTerm(args(1))))
    case meta.Term.ApplyInfix(l, meta.Term.Name(">"), _, r::Nil) =>
      // TODO what happens if we are interested in the result of this boolean operator?
      // TODO > is not a built in function but an atom in souffle, hence it does not produce a value
      val Datalog.Constant(Datalog.BooleanLiteral(bool)) = lhs
      if (true) {
        Seq(GreaterThan(compileScalaTerm(l), compileScalaTerm(r)))
      } else {
        Seq(LesserThanEqual(compileScalaTerm(l), compileScalaTerm(r)))
      }
    case meta.Term.ApplyInfix(l, meta.Term.Name(">="), _, r::Nil) =>
      val Datalog.Constant(Datalog.BooleanLiteral(bool)) = lhs
      if (bool) {
        Seq(GreaterThanEqual(compileScalaTerm(l), compileScalaTerm(r)))
      } else {
        Seq(LesserThan(compileScalaTerm(l), compileScalaTerm(r)))
      }
    case meta.Term.ApplyInfix(l, meta.Term.Name("<"), _, r::Nil) =>
      val Datalog.Constant(Datalog.BooleanLiteral(bool)) = lhs
      if (bool) {
        Seq(LesserThan(compileScalaTerm(l), compileScalaTerm(r)))
      } else {
        Seq(GreaterThanEqual(compileScalaTerm(l), compileScalaTerm(r)))
      }
    case meta.Term.ApplyInfix(l, meta.Term.Name("<="), _, r::Nil) =>
      val Datalog.Constant(Datalog.BooleanLiteral(bool)) = lhs
      if (bool) {
        Seq(LesserThanEqual(compileScalaTerm(l), compileScalaTerm(r)))
      } else {
        Seq(GreaterThan(compileScalaTerm(l), compileScalaTerm(r)))
      }
    case _ =>
      Seq()
  }

  def compileComputation(computation: Datalog.Computation)(implicit subt: Map[String, Datalog.Term]): Expression = computation match {
    case Datalog.Evaluation(evalArgs, resultType, code) => compileScalaTerm(code.tree.body)
    case Datalog.CountAggregation(patName, args) =>
      throw new IllegalArgumentException("Count Aggregation not supported yet")
    case Datalog.CustomAggregation(typ, description, agg, patName, args, aggregatedColumn) =>
      throw new IllegalArgumentException("CustomAggregation not supported yet")
  }

  def compileScalaTerm(term: meta.Term)(implicit subst: Map[String, Datalog.Term]): Expression = term match {
    case meta.Term.Name(name) => compileTerm(subst(name))
    case meta.Lit.Int(v) => NumberValue(v)
    case meta.Lit.Boolean(true) => NumberValue(-1)
    case meta.Lit.Boolean(false) => NumberValue(1)
    case meta.Lit.String(v) => StringValue(v)
    case meta.Lit.Double(v) => FloatValue(v.toFloat)

    case meta.Term.Apply(fun, meta.Lit.String(name) :: args) if fun.syntax == "inca.runtime.data.DataURI" =>
      val compiledArgs = args.map {
        case meta.Term.Name(n) => Variable(n)
        case _ => throw new IllegalArgumentException("DataURI can only have variables as input")
      }
      ADTValue(name, compiledArgs)

    case meta.Term.ApplyInfix(lhs, op, _, args) =>
      val builtInFunc: BuiltInFunction = op.value match {
        case "+" => AddBuiltInFunction
        case "-" => SubBuiltInFunction
        case "*" => MultBuiltInFunction
        case "/" => DivBuiltInFunction
        case "^" => PowBuiltInFunction
        case "%" => ModBuiltInFunction
        case _ => throw new IllegalArgumentException(s"No support for infix operation $op")
      }
      BuiltInFunctionCall(builtInFunc, (lhs +: args).map(compileScalaTerm))
    case _ =>
      throw new IllegalArgumentException(s"Not yet supported ${term.syntax}")
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
}
