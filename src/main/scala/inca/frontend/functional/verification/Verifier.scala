package inca.frontend.functional.verification

import inca.frontend.functional.Collect
import inca.frontend.functional.core.{Call, Let, Match, _}
import inca.util.{Gensym, Scala}
import smtlib.extensions.tip.Terms.{Case, CaseClass, CaseObject}
import smtlib.interpreters.Z3Interpreter
import smtlib.theories.Core
import smtlib.trees.Commands._
import smtlib.trees.Terms._
import smtlib.trees.{CommandsResponses, Terms}

import scala.collection.mutable

import scala.collection.mutable.ListBuffer


trait Response

case object VerifiedResponse extends Response
case object FalsifiedResponse extends Response
case object UnknownResponse extends Response

case class VerifierException(msg: String) extends Exception(msg)

class Verifier {

  type Property = AggregationProperty

  val varMem: mutable.Map[String, String] = mutable.Map()
  val functionDict: mutable.Map[String, FunctionDef] = mutable.Map()
  val dataDict: mutable.Map[String, DataDef] = mutable.Map()
  val partialOrders: mutable.Map[(String, String), Boolean] = mutable.Map()

  // TODO theoretically we need to pass a list of protected words in SMTlib to Gensym,
  //  but Gensym renames everything anyway, so it makes no difference

  def verify(module: Module): Map[String, Map[Property, Response]] = {
    implicit val gensym: Gensym = new Gensym(Seq())
    fillDicts(module)
    val aggregations: Map[String, Seq[Property]] = collectAggregations(module)
    val verificationScripts: Seq[Script] = aggregations.toSeq.map(ag => generateScript(ag._1, ag._2))
    val verificationResults = verificationScripts.zip(aggregations).map(s => getPropertyEval(s._1, s._2._2))
    combineVerificationResults(aggregations, verificationResults)
  }

  def combineVerificationResults(aggregations: Map[String, Seq[Property]], verificationResults: Seq[Map[Property, Response]]): Map[String, Map[Property, Response]] = {
    val reverseAggrMap: Map[String, String] = for ((k, v) <- varMem.toMap.filter(x => aggregations.contains(x._2))) yield (v, k)
    aggregations.keys.zip(verificationResults).map(res => (reverseAggrMap(res._1), res._2)).toMap
  }

  def fillDicts(module: Module)(implicit gensym: Gensym): Unit = module.content.foreach {
    case d: DataDef => dataDict += getHygienicName(d.name.name) -> d
    case f: FunctionDef => functionDict += getHygienicName(f.name.name) -> f
  }

  def getHygienicName(name: String)(implicit gensym: Gensym): String = {
    varMem.getOrElse(name, {
      val freshName = gensym.fresh(name)
      varMem += name -> freshName
      freshName
    })
  }

  def collectAggregations(module: Module)(implicit gensym: Gensym): Map[String, Seq[Property]] = {
    val aggrCollector = new Collect[(String, Seq[Property])] {
      override def transFun(func: FunctionDef): Seq[(String, Seq[Property])] = {
        if (func.annos.exists {
          case AggregationAnno(_) => true
          case _ => false
        }) {
          Seq((getHygienicName(func.name.name), getAggrProps(func)))
        } else {
          Seq()
        }
      }
    }
    aggrCollector(module).toMap
  }

  def getAggrProps(func: FunctionDef): Seq[Property] = {
    val aggrPropCollector = new Collect[Property] {
      override def transAnno(anno: Annotation): Seq[Property] = anno match {
        case AggregationAnno(props) => props
        case MainFunctionAnno => Seq()
      }
    }
    aggrPropCollector.transFun(func)
  }

  def getPropertyEval(s: Script, props: Seq[Property]): Map[Property, Response] = {
    val evalResults = evaluateScript(s)
    props.zip(evalResults).toMap
  }

  def evaluateScript(s: Script): Seq[Response] = {
    val interp = Z3Interpreter.buildDefault
    val evalResults: mutable.ListBuffer[Response] = mutable.ListBuffer()
    s.commands.foreach {
      cmd =>
        interp.eval(cmd) match {
          case CommandsResponses.CheckSatStatus(status) =>
            evalResults += (status match {
              case CommandsResponses.SatStatus => FalsifiedResponse
              case CommandsResponses.UnsatStatus => VerifiedResponse
              case CommandsResponses.UnknownStatus => UnknownResponse
            })
          case CommandsResponses.Error(msg) => throw VerifierException(s"z3 error interpreting command $cmd with error message \n ### \n $msg \n ### \n")
          case CommandsResponses.Unsupported => throw VerifierException(s"command $cmd is not supported by z3")
          case _ =>
        }
    }
    evalResults.toSeq
  }

  // Es wird angenommen, dass der Funktion der hygienische Name übergeben wird
  def generateScript(funcName: String, props: Seq[Property])(implicit gensym: Gensym): Script = {
    val calledFunctions: Seq[String] = collectCalledFunctions(functionDict(funcName))
    val inverseFunctionCalls: Seq[String] = getInverseFunctionCalls(funcName)
    val functions = (calledFunctions ++ inverseFunctionCalls :+ funcName).distinct
    val dataDefs = functions.flatMap(fName => collectUsedDataDefs(functionDict(fName))).distinct
    val transDataDefs = dataDefs.map(transDataDef)
    val transFuncDefs = transFunctionDefs(functions)
    val transProps = props.map(transAggrProperty(_, funcName))
    makeScript(transDataDefs ++ Seq(transFuncDefs) ++ transProps)
  }

  // Die Funktion gibt hygienische Namen zurück
  def collectCalledFunctions(func: FunctionDef)(implicit gensym: Gensym): Seq[String] = {
    val funcNameCollector = new Collect[String] {
      override def transExp(exp: Expression): Seq[String] = exp match {
        case Call(Var(name), args, _) => Seq(name.name) ++ args.flatMap(transExp)
        case _ => super.transExp(exp)
      }
    }
    /*
     Da auch Konstruktoraufrufe als Funktionsaufrufe gestaltet sind, wir aber nur "echte Funktionsaufrufe"
     haben wollen, filtern wir nach den Funktionen im dictionary. TODO imports des Moduls
     */
    var functions: Seq[String] = Seq()
    val allFuncs = funcNameCollector.transFun(func)
    var newFunctions: Seq[String] =
      allFuncs.map(getHygienicName).filter(functionDict.contains).distinct
    while (functions != newFunctions) {
      functions = newFunctions
      newFunctions = (functions ++ functions.flatMap(f =>
        funcNameCollector.transFun(functionDict(f))).map(getHygienicName).filter(functionDict.contains)).distinct
    }
    functions
  }

  // Diese Funktion gibt hygienische Namen zurück
  def getInverseFunctionCalls(funcName: String)(implicit gensym: Gensym): Seq[String] = {
    val func = functionDict(funcName)
    func.annos.flatMap {
      case AggregationAnno(props) => props.flatMap {
        case HasUnapply(invName) =>
          val hygInvName = getHygienicName(invName)
          val invFunc = functionDict(hygInvName)
          collectCalledFunctions(invFunc) :+ hygInvName
        case _ => Seq()
      }
    case _ => Seq()
    }
  }

  // Die Funktion gibt hygienische Namen zurück
  def collectUsedDataDefs(func: FunctionDef)(implicit gensym: Gensym): Seq[String] = {
    val dataNameCollector = new Collect[String] {
      override def transType(t: Type): Seq[String] = t match {
        case TData(name) => Seq(name.name)
        case _ => super.transType(t)
      }
    }
    var dataDefs: Seq[String] = Seq()
    var newDataDefs: Seq[String] =
      dataNameCollector.transFun(func).map(getHygienicName).filter(dataDict.contains).distinct
    while (dataDefs != newDataDefs) {
      dataDefs = newDataDefs
      newDataDefs = (dataDefs ++ dataDefs.flatMap(d =>
        dataNameCollector.transData(dataDict(d))).map(getHygienicName).filter(dataDict.contains)).distinct
    }
    dataDefs
  }

  // Es wird angenommen, dass der Funktion der hygienische Name übergeben wird
  def transDataDef(dataName: String)(implicit gensym: Gensym): Script = {
    val data = dataDict(dataName)
    val invariantScripts = data.annos.flatMap{
      case InvariantAnno(invariantNames) => Seq(generateInvariantsScript(invariantNames, dataName))
      // case PartialOrderAnnotation(relName) => verifyPartialOrder(relName, dataName)
      case _ => Seq()
    }
    val transConstrs = data.constrs.map(transDataConstructor)
    makeScript(Seq(
      Script(List(DeclareDatatypes(Seq((SSymbol(dataName), transConstrs)))))) ++ invariantScripts)
  }

  def transDataConstructor(c: DataConstructor)(implicit gensym: Gensym): Constructor = {
    val params = c.paramTypes.zipWithIndex.map { case (paramType, idx) =>
      val fieldName = gensym.fresh(s"${c.name.name}_Selector_$idx")
      val sort = transType(paramType)
      (SSymbol(fieldName), sort)
    }
    Constructor(SSymbol(getHygienicName(c.name.name)), params)
  }

  def generateInvariantsScript(invariantNames: Seq[String], dataName: String)(implicit gensym:Gensym): Script = {
    makeScript(invariantNames.map(name => {
      val hygienicInvariantName = getHygienicName(name)
      if(!functionDict.contains(hygienicInvariantName))
        throw VerifierException(s"Invariant Function $name called by data $dataName is not implemented")
      val invariantFunScript = transFunctionDefs(Seq(hygienicInvariantName))
      val freshVarName = gensym.fresh(dataName)
      val invariantAssertion = SMTlibScripts.invariant(dataName, hygienicInvariantName, freshVarName)
      makeScript(Seq(invariantFunScript, invariantAssertion))
    }))
  }

  def verifyPartialOrder(relName: String, dataName: String)(implicit gensym: Gensym): Unit = {
    val hygienicRelName = getHygienicName(relName)
    // TODO confirm correct signature (data, data) -> Boolean
    if(!functionDict.contains(hygienicRelName))
      throw VerifierException(s"Relation Function $relName called by data $dataName is not implemented")
    if(partialOrders.contains((hygienicRelName, dataName)))
      return
    val funScript = transFunctionDefs(Seq(relName))
    val partialOrderVerScript = makeScript(Seq(funScript,
      SMTlibScripts.reflexivity(hygienicRelName, dataName),
      SMTlibScripts.transitivity(hygienicRelName, dataName)))
    val evalResults = evaluateScript(partialOrderVerScript)
    if(evalResults.contains(FalsifiedResponse)){
      throw VerifierException(
        s"""Relation $relName was proven not to be a partial order for data $dataName. Evaluation results:
           |reflexivity: ${evalResults.head}
           |transitivity: ${evalResults(1)}
           |""".stripMargin)
    } else {partialOrders += (hygienicRelName, dataName) -> true}
  }

  // Es wird angenommen, dass der Funktion der hygienische Name übergeben wird
  def transFunctionDefs(funcNames: Seq[String])(implicit gensym: Gensym): Script = {
    val funDecls: mutable.ListBuffer[FunDec] = ListBuffer()
    val funBodys: mutable.ListBuffer[Term]= ListBuffer()
    funcNames.foreach{ funcName =>
      val func = functionDict(funcName)
      val transParams: Seq[SortedVar] =
        func.params.map(p => SortedVar(SSymbol(getHygienicName(p.name.name)), transType(p.typ)))
      val transOutType: Sort = transType(func.outType)
      funDecls += FunDec(SSymbol(funcName), transParams, transOutType)
      val transBody: Term = transExp(func.body)
      funBodys += transBody
    }
    Script(List(DefineFunsRec(funDecls.toSeq, funBodys.toSeq)))
  }

  def transType(typ: Type)(implicit gensym: Gensym): Sort = {
    typ match {
      case TScala(ty) => ty match {
        case Scala(meta.Type.Name("Int")) => Sort(Identifier(SSymbol("Int")))
        case Scala(meta.Type.Name("Boolean")) => Sort(Identifier(SSymbol("Bool")))
        case Scala(meta.Type.Name("Double")) => Sort(Identifier(SSymbol("Real")))
        case Scala(meta.Type.Name("String")) => Sort(Identifier(SSymbol("String")))
      }
      case TData(name) => Sort(Identifier(SSymbol(getHygienicName(name.name))))
      // TODO andere Cases
      case _ => throw VerifierException("Type needs to be specified")
    }
  }

  def transExp(ex: Expression)(implicit gensym: Gensym): Term = {
    ex match {
      case Var(name) => QualifiedIdentifier(Identifier(SSymbol(getHygienicName(name.name))))
      case Let(names, _, bound, body) =>
        val hygienicNames: Seq[String] = names.map(n => getHygienicName(n.name))
        val varNames: Seq[SSymbol] = hygienicNames.map(name => SSymbol(name))
        val boundTerms: Seq[Term] = {
          if (hygienicNames.length == 1) {
            Seq(transExp(bound))
          } else {
            if (hygienicNames.length < 1) {
              throw VerifierException(s"Let binding without variable bindings in this $ex")
            } else {
              bound match {
                case SetExp(es) => es.map(transExp)
                case _ => throw VerifierException(s"Expected SetExp containing the bound expressions, but got this $bound")
              }
            }
          }
        }
        val firstBinding = VarBinding(varNames.head, boundTerms.head)
        val otherBindings = varNames.tail.zip(boundTerms.tail).map(x => VarBinding(x._1, x._2))
        Terms.Let(firstBinding, otherBindings, transExp(body))

      case Match(matchee, cases) =>
        val scrut = transExp(matchee)
        val transCases = cases.map {
          case (ConstructorPattern(constr, args), body) =>
            val transPattern = if (args.isEmpty) {
              CaseObject(SSymbol(getHygienicName(constr.name)))
            } else {
              CaseClass(SSymbol(getHygienicName(constr.name)), args.map(arg => SSymbol(getHygienicName(arg.name))))
            }
            Case(transPattern, transExp(body))
          case _ => throw VerifierException(s"Expected Constructor Pattern, but got this $ex (Pattern Matching over Some and None not supported, because constructs are not used)") // Some und None werden erstmal nicht gebraucht
        }
        smtlib.extensions.tip.Terms.Match(scrut, transCases)

      case Call(fun, args, _) =>
        val transFun = transExp(fun)
        val transArgs = args.map(transExp)
        transFun match {
          case q: QualifiedIdentifier => if (transArgs.isEmpty) {
            q
          } else {
            FunctionApplication(q, transArgs)
          }
          case _ => throw VerifierException(s"Expected qualified identifier in function call, but got this $fun")
        }

      case If(cnd, thn, els) =>
        FunctionApplication(QualifiedIdentifier(Identifier(SSymbol("ite"))),
          Seq(cnd, thn, els).map(transExp))

      case Lambda(vs, body) =>
        val args = vs.map(v => SortedVar(SSymbol(getHygienicName(v._1.name)), transType(v._2)))
        smtlib.extensions.tip.Terms.Lambda(args, transExp(body))


      case BaseApplyInfix(left, op, right) =>
        val infixFun: (Term, Term) => Term = left.typ match {
          case Some(leftType) => right.typ match {
            case Some(rightType) =>
              val exc = new Exception(s"Operator $op on types $leftType and $rightType has no equivalent in SMTlib")
              val typeMap = metaInfixOps.getOrElse((leftType, rightType), throw exc)
              typeMap.getOrElse(op.tree.value, throw exc)
            case None => throw VerifierException(s"No type for rhs of $ex could be found")
          }
          case None => throw VerifierException(s"No type for lhs of $ex could be found")
        }
        infixFun(transExp(left), transExp(right))

      case BaseLit(code) =>
        code.tree match {
          case l: meta.Lit => transMetaLit(l)
          case _ => throw VerifierException(s"BaseLit $ex does not contain Term.Literal")
        }

      // TODO implement
      case Tuple(_) => ???
      case BaseApply(_, _) => ???
    }
  }

  // Es wird angenommen, dass der Funktion der hygienische Name übergeben wird
  def transAggrProperty(prop: Property, aggrName: String)(implicit gensym: Gensym): Script = {
    val paramTypeName = getParamTypeName(aggrName)
    prop match {
      case Associativity => SMTlibScripts.associativity(aggrName, paramTypeName)
      case Commutativity => SMTlibScripts.commutativity(aggrName, paramTypeName)
      case HasUnapply(invName) => SMTlibScripts.hasUnapply(aggrName, getHygienicName(invName), paramTypeName)
    }
  }

  def getParamTypeName(aggrName: String)(implicit gensym: Gensym): String = {
    val func = functionDict(aggrName)
    func.params.foreach(p => if (p.typ != func.params.head.typ) {
      throw new Exception("Aggregations should take two values of the same type")
    })
    transType(func.params.head.typ).id.symbol.name
  }

  def makeScript(scripts: Seq[Script]): Script = {
    Script(scripts.flatMap(s => s.commands).toList)
  }

  def transMetaLit(lit: meta.Lit): Term = {
    lit match {
      case meta.Lit(value) => value match {
        case b: Boolean => Core.BoolConst(b)
        case by: Byte => SNumeral(by)
        case ch: Char => SString(ch.toString)
        case d: Double => SDecimal(d)
        case f: Float => SDecimal(f)
        case i: Int => SNumeral(i)
        case _ => throw new Exception("Literal not implemented")
      }
      case _ => throw new Exception("Literal is not a literal")
    }
  }

  val integerDivision: (Term, Term) => Term = (left, right) => {
    FunctionApplication(QualifiedIdentifier(Identifier(SSymbol("div"))), Seq(
      FunctionApplication(QualifiedIdentifier(Identifier(SSymbol("to_real"))), Seq(left)),
      FunctionApplication(QualifiedIdentifier(Identifier(SSymbol("to_real"))), Seq(right))
    ))
  }

  val basicMetaInfixIntOps: Map[String, (Term, Term) => Term] = transformInfixMap(Map(
    "+" -> "+",
    "-" -> "-",
    "*" -> "*",
    "%" -> "mod",
    "<" -> "<",
    ">" -> ">",
    "<=" -> "<=",
    ">=" -> ">=",
    "==" -> "=",
    "!=" -> "distinct"
  ))

  val metaInfixIntOps: Map[String, (Term, Term) => Term] = basicMetaInfixIntOps ++ Map(
    "/" -> integerDivision
  )

  val infixRealOpsMap = Map(
    "+" -> "+",
    "-" -> "-",
    "*" -> "*",
    "/" -> "/",
    "<" -> "<",
    ">" -> ">",
    "<=" -> "<=",
    ">=" -> ">=",
    "==" -> "=",
    "!=" -> "distinct"
  )

  val metaInfixRealOps: Map[String, (Term, Term) => Term] = transformInfixMap(infixRealOpsMap)

  def transformInfixMapRightArgToReal(inputMap: Map[String, String]): Map[String, (Term, Term) => Term] = inputMap.map(x =>
    (x._1, (left: Term, right: Term) =>
      FunctionApplication(QualifiedIdentifier(Identifier(SSymbol(x._2))), Seq(
        left,
        FunctionApplication(QualifiedIdentifier(Identifier(SSymbol("to_real"))), Seq(right))
      ))))

  def transformInfixMapLeftArgToReal(inputMap: Map[String, String]): Map[String, (Term, Term) => Term] = inputMap.map(x =>
    (x._1, (left: Term, right: Term) =>
      FunctionApplication(QualifiedIdentifier(Identifier(SSymbol(x._2))), Seq(
        FunctionApplication(QualifiedIdentifier(Identifier(SSymbol("to_real"))), Seq(left)),
        right
      ))))

  val metaInfixIntRealOps: Map[String, (Term, Term) => Term] = transformInfixMapRightArgToReal(infixRealOpsMap)

  val metaInfixRealIntOps: Map[String, (Term, Term) => Term] = transformInfixMapLeftArgToReal(infixRealOpsMap)

  val metaInfixBoolOps: Map[String, (Term, Term) => Term] = transformInfixMap(Map(
    "&&" -> "and",
    "and" -> "and",
    "||" -> "or",
    "or" -> "or",
    "==" -> "=",
    "!=" -> "distinct"
  ))

  val metaInfixStringOps: Map[String, (Term, Term) => Term] = transformInfixMap(Map(
    "+" -> "str.++",
    "==" -> "=",
    "!=" -> "distinct"
  ))

  def transformInfixMap(inputMap: Map[String, String]): Map[String, (Term, Term) => Term] = inputMap.map(x =>
  (x._1, (left: Term, right: Term) =>
    FunctionApplication(QualifiedIdentifier(Identifier(SSymbol(x._2))), Seq(left, right))))

  val metaInfixOps: Map[(Type, Type), Map[String, (Term, Term) => Term]] = Map(
    (TScalaInt, TScalaInt) -> metaInfixIntOps,
    (TScalaLong, TScalaLong) -> metaInfixIntOps,
    (TScalaDouble, TScalaDouble) -> metaInfixRealOps,
    (TScalaBoolean, TScalaBoolean) -> metaInfixBoolOps,
    (TScalaString, TScalaString) -> metaInfixStringOps,
    (TScalaLong, TScalaDouble) -> metaInfixIntRealOps,
    (TScalaDouble, TScalaLong) -> metaInfixRealIntOps,
    (TScalaInt, TScalaDouble) -> metaInfixIntRealOps,
    (TScalaDouble, TScalaInt) -> metaInfixRealIntOps,
  )
}