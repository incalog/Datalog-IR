package inca.ddlog.backend

import inca.ir
import inca.ddlog.syntax.Declaration.{Relation, Rule}
import inca.ddlog.syntax.IOType.{Input, Output}
import inca.ir.extension.arithmetic as irarith
import inca.ir.extension.string as irstring
import inca.ir.extension.data as irdata
import inca.ir.extension.aggregate as iragg
import inca.ir.extension.data.*
import inca.ir.{RefByName, TNothing, WildcardArg, name2string}
import inca.ddlog.syntax.{Arg, Atom, Declaration, Expr, IOType, Identifier, Program, RhsClause, SimpleType, Term}
import inca.ir.extension.aggregate.AggregateColumnArg
import inca.ir.typing.Mode
import inca.foreign.ddlog.ir.primitive.{DDLogAggregationOperator, DDLogDefnModuleEntry}
import inca.util.Gensym

import scala.annotation.tailrec
import scala.collection.immutable.{AbstractSeq, LinearSeq}

// baseIR + arithmetic + strings + data + aggregation
object GenerateDDLog:
  def cleanName(name: String): String = name.replace("$", "_")
  def cleanName(name: ir.Name): String = cleanName(name.name)

import GenerateDDLog.cleanName

class GenerateDDLog:
  private var typeDependencies: Map[String, Set[String]] = Map()

  private def addTransitive[A, B](s: Set[(A, B)]) =
    s ++ (for ((x1, y1) <- s; (x2, y2) <- s if y1 == x2) yield (x1, y2))

  @tailrec
  private def transitiveClosure[A, B](s: Set[(A, B)]): Set[(A, B)] = {
    val t = addTransitive(s)
    if (t.size == s.size) s else transitiveClosure(t)
  }
  

  private val gensym = new Gensym()
  private def freshTmpName(): String = cleanName(gensym.fresh("_tmp"))

  var caseDefs: Map[ir.Name, Seq[irdata.CaseDefinition]] = Map()

  private def generateAdtFieldName(caseName: String, idx: Int) = s"param${cleanName(caseName)}$idx"

  def compileModule(module: ir.Module): Program =
    additionalAggregationFunctions = Set()
    aggregateRelation = Set()

    caseDefs = module.contents.collect {
      case cd: CaseDefinition => cd.data.ref.name -> cd
    }.groupBy(_._1).view.mapValues(e => e.map(x => x._2)).toMap

    val nestedDataTypes = caseDefs.toSet.flatMap { (dataName, cases) =>
      cases.flatMap(_.args.collect {
        case TData(ref) => cleanName(dataName) -> cleanName(ref.name.name)
      })
    }

    typeDependencies = transitiveClosure(nestedDataTypes).groupBy(_._1).map { case (k, v) => (k, v.map(_._2)) }
    val decls = module.contents.flatMap {
      case rel: ir.Relation => compileRelation(rel)
      case rel: ir.ExtensionalRelation => compileExtensionalRelation(rel)

      case data : irdata.DataDefinition =>
        val cleanDataName = cleanName(data.name)
        val compiledCases = caseDefs(data.name).map {
          case CaseDefinition(caseName, caseTypes, _) =>
            val cleanCaseName = cleanName(caseName.name)
            cleanCaseName -> caseTypes.zipWithIndex.map {
              case (ty@TData(RefByName(name)), idx) =>
                val dependencies = typeDependencies.getOrElse(cleanName(name.name), Set.empty[String])
                if (dependencies.contains(cleanDataName))
                  (generateAdtFieldName(caseName, idx), SimpleType.Ref(compileType(ty)))
                else
                  (generateAdtFieldName(caseName, idx), compileType(ty))
              case (ty, idx) =>
                (generateAdtFieldName(caseName, idx), compileType(ty))
            }
        }
        Seq(Declaration.Datatype(cleanDataName, compiledCases))
      case _ => Seq()
    }

    val externalContent = module.contents.collect { case DDLogDefnModuleEntry(_, code) => code }.mkString("\n")
    Program(decls ++ aggregateRelation, externalContent ++ additionalAggregationFunctions.mkString("\n"))

  def compileRelation(rel: ir.Relation): Seq[Declaration] =
    val args = rel.params.flatMap(compileParam)
    val decl = Declaration.Relation(Some(Output), Identifier.rel(cleanName(rel.name)), args)
    val rules = rel.bodies.map(b => compileBody(rel.name, rel.params, b))
    decl +: rules

  def compileExtensionalRelation(rel: ir.ExtensionalRelation): Seq[Declaration] =
    val args = rel.params.flatMap(compileParam)
    val decl = Declaration.Relation(Some(Input), Identifier.rel(cleanName(rel.name)), args)
    Seq(decl)

  def compileBody(relName: ir.Name, headParams: Seq[ir.Param], body: ir.Body): Declaration.Rule =
    val head = Atom.Call(Identifier.rel(cleanName(relName.name)), headParams.map(p => Expr.T(Term.VarTerm(Identifier.varn(cleanName(p.name.name))))))
    val ats = body.atoms.flatMap { a =>
      additionalEqs = Seq()
      compileAtomToRhsClause(a) +: additionalEqs
    }
    Declaration.Rule(Seq(head), ats)

  private var additionalEqs: Seq[RhsClause] = Seq()
  private def cleanArgs(args: Seq[ir.Arg]): Seq[ir.Arg] =
    // DDLog disallows atoms which use the same variable binding and bound, e.g.
    //  R(x) :- Q(x, x).
    // That is, we need to rewrite those atoms to:
    //  R(x) :- Q(x, x_1), x == x_1.
    var duplicateVars: Map[ir.Var, Int] = args.flatMap(_.vars).groupBy(identity)
      .flatMap { case (v, occurrences) =>
        val hasBindingOccurrence = occurrences.exists(_.typ.exists(_.mode.isBinding))
        val hasBoundOccurrence = occurrences.exists(_.typ.exists(_.mode.isBound))
        if ((occurrences.size > 1) && (hasBindingOccurrence && hasBoundOccurrence))
          Some(v -> occurrences.size)
        else
          None
      }
    args.map {
      case ir.TermArg(v: ir.Var) if duplicateVars.getOrElse(v, 1) > 1 =>
        duplicateVars += v -> (duplicateVars(v) - 1)
        val freshName = gensym.freshName(v.name)
        val lhsTerm = ir.Var(v.name)
        val rhsTerm = ir.Var(freshName)
        lhsTerm.typ = Some(ir.TermType(v.typ.get.ty, Mode.Bound))
        rhsTerm.typ = Some(ir.TermType(v.typ.get.ty, Mode.Bound))
        additionalEqs :+= compileAtomToRhsClause(ir.Eq(lhsTerm, rhsTerm))
        val freshVar = ir.Var(freshName)
        val freshArg = ir.TermArg(freshVar)
        freshVar.typ = Some(ir.TermType(v.typ.get.ty, Mode.Binding))
        freshArg
      case a => a
    }

  def compileAtomToRhsClause(at: ir.Atom, derefRhs: Boolean = false): RhsClause = at match
    case ir.Call(ref, args, false) => RhsClause.At(compileAtom(at))
    case ir.Call(ref, args, true) => RhsClause.Not(compileAtom(at))
    case ir.ExtensionalCall(ref, args, false) => RhsClause.At(compileAtom(at)) // same as call
    case ir.ExtensionalCall(ref, args, true) => RhsClause.Not(compileAtom(at)) // same as call
    case iragg.Aggregate(rel, args, op) => RhsClause.At(compileAtom(at))
    case ir.Eq(lhs, rhs, false) =>
      (lhs.typ.get, rhs.typ.get) match
        case (ir.TermType(_, Mode.Bound), ir.TermType(_, Mode.Bound)) => // comparison
          var rhsT = compileTermToExpression(rhs, false)
          rhsT = if (derefRhs) Expr.Deref(rhsT) else rhsT
          RhsClause.Eq(compileTermToExpression(lhs, false), rhsT)
        case (ir.TermType(_, Mode.Binding), ir.TermType(_, Mode.Binding)) =>
          throw IllegalStateException(s"Can not bind lhs and rhs: $at")
        case (ir.TermType(_, Mode.Binding), _) => // assign rhs to lhs
          val lhsT = compileTermToExpression(lhs, true)
          var rhsT = compileTermToExpression(rhs, false)
          rhsT = if (derefRhs) Expr.Deref(rhsT) else rhsT
          RhsClause.Ex(Expr.Assign(lhsT, rhsT))
        case (_, ir.TermType(_, Mode.Binding)) => // assign lhs to rhs
          var lhsT = compileTermToExpression(lhs, false)
          lhsT = if (derefRhs) Expr.Deref(lhsT) else lhsT
          val rhsT = compileTermToExpression(rhs, true)
          RhsClause.Ex(Expr.Assign(rhsT, lhsT))
        case _ =>
          throw IllegalStateException(s"Can not compile eq: $at")
    case ir.Eq(lhs, rhs, true) => RhsClause.NotEq(compileTermToExpression(lhs, false), compileTermToExpression(rhs, false))

    case irstring.RegexMatch(t, pattern, neg) => ???
    
    case irarith.BinCompare(lhs, rhs, op) =>  op match
      case "<" => RhsClause.Ex(Expr.LT(compileTermToExpression(lhs, false), compileTermToExpression(rhs, false)))
      case "<=" => RhsClause.Ex(Expr.LE(compileTermToExpression(lhs, false), compileTermToExpression(rhs, false)))
      case ">" => RhsClause.Ex(Expr.GT(compileTermToExpression(lhs, false), compileTermToExpression(rhs, false)))
      case ">=" => RhsClause.Ex(Expr.GE(compileTermToExpression(lhs, false), compileTermToExpression(rhs, false)))
      case _ => throw new RuntimeException("unsupported Binary Compare Operation: " + op)

    case irdata.Deconstruct(term, caseRef, args, neg) =>
      val tmp = freshTmpName()
      val caseDef = caseRef.target.get
      val dataDef = caseDef.data.ref.target.get
      val caseName = cleanName(caseDef.name)

      val numberOfWildcards = args.count {
        case ir.WildcardArg() => true
        case _ => false
      }
      val onlyWildcards = (numberOfWildcards == args.size) && args.nonEmpty
      val noWildcards = numberOfWildcards == 0

      if (neg && onlyWildcards) {
        val otherCaseDefs = caseDefs(dataDef.name).filter(c => c != caseDef)
        // TODO: Enumerate all other cases and add a new rule for each of them
        throw IllegalStateException("Can not compile Deconstruct which only contains wildcards.")
      } else if (!neg || (neg && noWildcards)) {
        val (fieldNames, compiledArgs, derefs) = args.zipWithIndex.map {
          case (a@ir.TermArg(t), idx) =>
            val isAssign = t.typ.get.mode.isBinding
            val fieldName = generateAdtFieldName(caseName, idx)
            t.typ.get.ty match
              case ty@TData(RefByName(tyName)) =>
                val recTys = typeDependencies.getOrElse(cleanName(tyName), Set.empty[String])
                if (recTys.contains(cleanName(dataDef.name.name)))
                  val tmpName = freshTmpName()
                  // compile the assign or comparison to deref
                  val lhsIR = ir.Var(ir.Name(tmpName))
                  lhsIR.typ = Some(ir.TermType(ty, Mode.Bound))
                  val deref = compileAtomToRhsClause(ir.Eq(a.t, lhsIR), true)
                  (fieldName, Some(Expr.T(Term.VarDeclTerm(Identifier.varn(tmpName)))), Some(deref))
                else
                  (fieldName, compileArg(a, isAssign), None)
              case _ =>
                (fieldName, compileArg(a, isAssign), None)
          case (w@WildcardArg(), idx) =>
            val fieldName = generateAdtFieldName(caseName, idx)
            (fieldName, compileArg(w, true), None)
          case (iragg.AggregateColumnArg(t), idx) =>
            throw IllegalStateException("Deconstruct can not contain Aggregate Column")
        }.unzip3
        RhsClause.Deconstruct(compileTermToExpression(term, false), cleanName(caseRef.name), fieldNames, compiledArgs.flatten, derefs.flatten, neg)
      } else {
        throw IllegalStateException("Can not compile Deconstruct which mixes wildcards and constants.")
      }

    case _ => throw new RuntimeException(s"unsupported Atom: ${at.getClass.getSimpleName}")

  private var aggregateRelation: Set[Declaration] = Set()
  private var additionalAggregationFunctions: Set[String] = Set()

  private def compileAggregationOperator(op: iragg.AggregationOperator, groupByTys: Seq[SimpleType]): String = op match
    case irarith.ArithmeticAggregationOperator.MaxInt | irarith.ArithmeticAggregationOperator.MaxDouble => "max"
    case irarith.ArithmeticAggregationOperator.MinInt | irarith.ArithmeticAggregationOperator.MinDouble => "min"
    case irarith.ArithmeticAggregationOperator.SumInt | irarith.ArithmeticAggregationOperator.SumDouble => "sum_of"
    case irarith.ArithmeticAggregationOperator.Count => "count_distinct"
    case DDLogAggregationOperator(name, ty, initCode, addCode) =>
      val aggName = s"agg_${name}_group"
      val cTy = compileType(ty)
      val aggFunction = s"""
      |function $aggName(g: Group<(${groupByTys.mkString(", ")}), $cTy>): $cTy
      |{
      |    var agg_value: $cTy = $initCode;
      |    for ((value, w) in g) {
      |        agg_value = $addCode(value, agg_value);
      |    };
      |    agg_value
      |}
      |""".stripMargin
      additionalAggregationFunctions += aggFunction
      aggName
    case _ => throw IllegalStateException(s"Can not compile aggregation operator: $op") // TODO: Support custom aggregation operators

  private def createAggregateRelation(rel: ir.Ref[ir.Relation], adornment: Seq[Mode], aggregateColumn: Int, op: iragg.AggregationOperator): Identifier.UCIdentifier =
    val adornS = adornment.map {
      case Mode.Bound => "b"
      case _ => "f"
    }.mkString("")
    // filter out the indices of the bound arguments that aren't the aggregate column
    val groupByArgsIndices = adornment.zipWithIndex.collect {
      case (Mode.Bound, i) if i != aggregateColumn => i
    }
    val aggRelName = Identifier.rel(cleanName(s"Agg${rel.name.name}_$adornS"))
    val targetRel = rel.target.get
    val ddRel = Relation(None, aggRelName, targetRel.params.flatMap(compileParam))
    
    val relName = Identifier.rel(cleanName(rel.name.name))
    val relArgs = targetRel.params.map(p => Expr.T(Term.VarTerm(Identifier.varn(cleanName(p.name)))))
    
    val aggColArg = relArgs(aggregateColumn)
    val newAggArg = Expr.T(Term.VarTerm(Identifier.varn(cleanName(freshTmpName()))))
    val aggRelArgs = groupByArgsIndices.map(relArgs) :+ newAggArg
    val groupByArgs = groupByArgsIndices.map(relArgs)

    val groupByTys = groupByArgsIndices.map(targetRel.params.map(p => compileType(p.ty)))
    val opName = compileAggregationOperator(op, groupByTys)
    
    val ddRule = Rule(
      Seq(Atom.Call(aggRelName, aggRelArgs)),
      Seq(
        RhsClause.At(Atom.Call(relName, relArgs)),
        RhsClause.GroupBy(newAggArg, aggColArg, groupByArgs, opName),
      )
    )
    aggregateRelation += ddRel
    aggregateRelation += ddRule
    aggRelName

  def compileAtom(at: ir.Atom): Atom = at match
    case agg@iragg.Aggregate(rel, args, op) =>
      val boundness = args.foldLeft(Seq[Mode]()) {
          case (acc, ir.TermArg(t)) => acc :+ t.typ.get.mode
          case (acc, ir.WildcardArg()) => acc :+ Mode.Binding
          case (acc, iragg.AggregateColumnArg(t)) => acc :+ Mode.Bound
      }

      val aggColumn = agg.aggregationColumns match
        case Seq(idx) => idx
        case _ => throw IllegalStateException("Only aggregations with a single argument are supported!")
      val aggCall = createAggregateRelation(rel, boundness, aggColumn, agg.op)

      val aggParamsOptions = args.flatMap {
        case a@ir.TermArg(t) if t.typ.exists(_.mode.isBound) => Some(compileArg(a, false))
        case _ => None
      }
      val aggParams = (aggParamsOptions :+ compileArg(args(aggColumn), false)).flatten
      Atom.Call(aggCall, aggParams)
    case ir.Call(ref, args, _) =>
      val argParam = cleanArgs(args).flatMap(a => compileArg(a, false))
      Atom.Call(Identifier.rel(cleanName(ref.toString)), argParam)
    case ir.ExtensionalCall(ref, args,_) =>
      val argParam = cleanArgs(args).flatMap(a => compileArg(a, false))
      Atom.Call(Identifier.rel(cleanName(ref.toString)), argParam)
    case _ => throw new RuntimeException(s"unsupported Call: $at")

  def compileArg(arg: ir.Arg, isAssign: Boolean): Option[Expr] = arg match
    case AggregateColumnArg(t) => Some(compileTermToExpression(t, isAssign))
    case ir.TermArg(t) if t.typ.exists(_.ty == ir.TNothing) => None
    case ir.TermArg(t) => Some(compileTermToExpression(t, isAssign))
    case w@ir.WildcardArg() if w.typ.exists(_.ty == TNothing) => None
    case ir.WildcardArg() => Some(Expr.T(Term.Wildcard()))
    case _ => throw new RuntimeException(s"Unsupportet Argument: $arg")

  def compileTermToExpression(term: ir.Term, isAssign: Boolean): Expr = term match
    case irarith.BinOp(lhs, rhs, op) => op match
      case "+" => Expr.Add(compileTermToExpression(lhs, isAssign), compileTermToExpression(rhs, isAssign))
      case "-" => Expr.Sub(compileTermToExpression(lhs, isAssign),compileTermToExpression(rhs, isAssign))
      case "*" => Expr.Mul(compileTermToExpression(lhs, isAssign),compileTermToExpression(rhs, isAssign))
      case "/" => Expr.Div(compileTermToExpression(lhs, isAssign),compileTermToExpression(rhs, isAssign))
      case "%" => Expr.Remainder(compileTermToExpression(lhs, isAssign),compileTermToExpression(rhs, isAssign))
      case "min" => Expr.Min(compileTermToExpression(lhs, isAssign), compileTermToExpression(rhs, isAssign))
      case "max" => Expr.Max(compileTermToExpression(lhs, isAssign), compileTermToExpression(rhs, isAssign))
      case _ => throw new RuntimeException("unsupported binary Operation: " + op)
    case irarith.UnOp(t, op) => op match
      case "-" => Expr.Neg(compileTermToExpression(t, isAssign))
      case "abs"=> Expr.Abs(compileTermToExpression(t, isAssign))
    case irstring.StringConcat(lhs, rhs) => Expr.Concat(compileTermToExpression(lhs, isAssign),compileTermToExpression(rhs, isAssign))
    case irstring.ToString(s) => Expr.ToString(compileTermToExpression(s, isAssign))
    case irstring.Substring(t, index, length) => ???
    case irstring.StringLength(t) => ???
    case irstring.OrdinalNumber(t) => ???
    case ir.Cast(t, ty) => Expr.Cast(compileTermToExpression(t, isAssign),compileType(ty))
    case _ => Expr.T(compileTerm(term, isAssign))

  def compileTerm(term: ir.Term, isAssign: Boolean): Term = term match
    case ir.Var(ref) if isAssign => Term.VarDeclTerm(Identifier.varn(cleanName(ref.name.name)))
    case ir.Var(ref) if !isAssign => Term.VarTerm(Identifier.varn(cleanName(ref.name.name)))
    case irarith.IntNum(value) => Term.IntLit(value)
    case irarith.DoubleNum(value) => Term.DoubleLit(value)
    case irstring.StringLit(s) => Term.StringLit(s)
    case irdata.Construct(ref, args) =>
      val caseDef = ref.target.get
      val dataDef = caseDef.data.ref.target.get
      val caseName = cleanName(caseDef.name)
      val (fieldNames, compiledArgs) = args.zipWithIndex.map { case (t, idx) =>
        val argTy = t.typ.get.ty
        argTy match
          case TData(RefByName(tyName)) =>
            val recTys = typeDependencies.getOrElse(cleanName(tyName), Set.empty[String])
            if (recTys.contains(cleanName(dataDef.name.name)))
              val compiled = compileTermToExpression(t, false) match
                case Expr.T(term) => Expr.T(Term.NewRef(term))
                case exp => exp
              (generateAdtFieldName(caseName, idx), compiled)
            else
              (generateAdtFieldName(caseName, idx), compileTermToExpression(t, false))
          case _ =>
            (generateAdtFieldName(caseName, idx), compileTermToExpression(t, false))
      }.unzip
      Term.Constructor(caseName, fieldNames, compiledArgs)
    case _ => throw new RuntimeException(s"Term Not defined ${term.getClass.getSimpleName}")

  def compileParam(param: ir.Param): Option[Arg] =
    val argName = Identifier.arg(cleanName(param.name.name))
    param.ty match
      case ir.TNothing => None
      case _ => Some(Arg(argName, compileType(param.ty)))

  def compileType(value: ir.Type): SimpleType = value match
    case irarith.TInt => SimpleType.Bigint
    case irarith.TDouble => SimpleType.Double
    case irstring.TString => SimpleType.String
    case irdata.TData(ref) =>
      val cleanDataName = cleanName(ref.name)
      SimpleType.TypeAlias(cleanDataName)
    case ir.TAny => throw IllegalArgumentException(s"Can not compile type: TAny")
    case ty => throw IllegalArgumentException(s"Can not compile type: $ty")
