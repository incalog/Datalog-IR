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

// TODO Datentyp für die Ausgabe, dem Ich einen prettyPrint gebe, damit das Testergebnis lesbar wird?
//  Lattices in .finca Dateien auslagern


trait Response

case object VerifiedResponse extends Response
case object FalsifiedResponse extends Response
case object UnknownResponse extends Response

abstract class VerifierException(msg: String) extends Exception(msg)

case class FunctionNotFoundException(name: String) extends VerifierException(s"Could not find function $name")
case class DataNotFoundException(name: String) extends VerifierException(s"Could not find abstract data type $name")
case class Z3Exception(msg: String) extends VerifierException("An exception occured while running z3, exception message: \n" + msg)
case class UnexpectedBehaviorException(msg: String) extends VerifierException(msg)
case class UnsupportedException(msg: String) extends VerifierException(msg)
case class SyntaxException(msg: String) extends VerifierException(msg)

class Verifier {

  val varMem: mutable.Map[String, String] = mutable.Map()
  val reverseVarMem: mutable.Map[String, String] = mutable.Map()
  val functionDict: mutable.Map[String, FunctionDef] = mutable.Map()
  val dataDict: mutable.Map[String, DataDef] = mutable.Map()
  // verificationResponses verwendet die originalen Namen als keys
  val verificationResponses: mutable.Map[String, Map[VerifiableProperty, Response]] = mutable.Map()

  // Theoretically we need to pass a list of protected words in SMTlib to Gensym,
  //  but Gensym renames everything anyway, so it makes no difference

  def verify(module: Module): Map[String, Map[VerifiableProperty, Response]] = {
    implicit val gensym: Gensym = new Gensym(Seq())
    fillDicts(module)
    val annotatedFunctions: Map[String, Seq[VerifiableProperty]] = collectAnnotated(module)
    val verificationScripts: Seq[Script] =
      annotatedFunctions.toSeq.map(an => generateScript(an._1, an._2, getAdditionalFunctions(an._2)))
    val verificationResults: Seq[Map[VerifiableProperty, Response]] =
      annotatedFunctions.zip(verificationScripts).map(tuple => tuple._1._2.zip(evaluateScript(tuple._2)).toMap).toSeq
    val annotatedVerificationResponses = annotatedFunctions.keys.zip(verificationResults).map(res => (getOriginalName(res._1), res._2)).toMap
    extendResponseMap(annotatedVerificationResponses, verificationResponses.toMap)
  }

  def extendResponseMap(mainMap: Map[String, Map[VerifiableProperty, Response]],
                        extension: Map[String, Map[VerifiableProperty, Response]]): Map[String, Map[VerifiableProperty, Response]] = {
    var extraEntries = extension
    mainMap.map(response =>
      if(extension.contains(response._1)) {
        val furtherResponses = extension.getOrElse(response._1, throw UnexpectedBehaviorException(s"Checked that" +
          s"${response._1} is key for $extension, but could not get it anyway"))
        extraEntries =  extraEntries.removed(response._1)
        (response._1, response._2 ++ furtherResponses)
      } else {
        response
      }
    ) ++ extraEntries
  }

  def getOriginalName(hygienicName: String): String = {
    reverseVarMem.getOrElse(hygienicName: String, throw UnexpectedBehaviorException(s"Hygienic Name " +
      s"$hygienicName is supposed to have matching original name in reverseVarMem"))
  }

  def fillDicts(module: Module)(implicit gensym: Gensym): Unit = module.content.foreach {
    case d: DataDef => dataDict += getHygienicName(d.name.name) -> d
    case f: FunctionDef => functionDict += getHygienicName(f.name.name) -> f
  }

  def getHygienicName(name: String)(implicit gensym: Gensym): String = {
    varMem.getOrElse(name, {
      val freshName = gensym.fresh(name)
      varMem += name -> freshName
      reverseVarMem += freshName -> name
      freshName
    })
  }

  def collectAnnotated(module: Module)(implicit gensym: Gensym): Map[String, Seq[VerifiableProperty]] = {
    val propCollector = new Collect[VerifiableProperty] {
      override def transAnno(anno: Annotation): Seq[VerifiableProperty] = anno match {
        case a: SoundnessAnno => Seq(a)
        case AggregationAnno(props) => props
        case _ => Seq()
      }
    }
    val annoFuncCollector: Collect[(String, Seq[VerifiableProperty])] = new Collect[(String, Seq[VerifiableProperty])] {
      override def transFun(func: FunctionDef): Seq[(String, Seq[VerifiableProperty])] = {
        if (func.annos.exists {
          case _: SoundnessAnno => true
          case _: AggregationAnno => true
          case _ => false
        }) {
          Seq((getHygienicName(func.name.name), propCollector.transFun(func)))
        } else {
          Seq()
        }
      }
    }
    annoFuncCollector(module).toMap
  }

  def getAdditionalFunctions(props: Seq[VerifiableProperty])(implicit gensym: Gensym): Seq[String] = {
    props.flatMap {
      case HasUnapply(unapplyName) => Seq(getHygienicName(unapplyName))
      case SoundnessAnno(c, pB, rB, pN) =>
        Seq(c, pB, rB, pN).distinct.map(getHygienicName)
      case _ => Seq()
    }
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
          case CommandsResponses.Error(msg) => throw Z3Exception(s"z3 error interpreting command $cmd with error message \n ### \n $msg \n ### \n")
          case CommandsResponses.Unsupported => throw Z3Exception(s"command $cmd is not supported by z3")
          case _ =>
        }
    }
    evalResults.toSeq
  }

  // Es wird angenommen, dass der Funktion der hygienische Name übergeben wird
  def generateScript(funcName: String, props: Seq[VerifiableProperty], additionalFuncs: Seq[String] = Seq())(implicit gensym: Gensym): Script = {
    val calledFunctions: Seq[String] = (Seq(funcName) ++ additionalFuncs).flatMap(fname => collectCalledFunctions(getFunctionDef(fname)))
    val functions = (calledFunctions ++ additionalFuncs :+ funcName).distinct
    val dataDefs = functions.flatMap(fName => collectUsedDataDefs(getFunctionDef(fName))).distinct
    val transDataDefs = dataDefs.map(transDataDef)
    val transFuncDefs = transFunctionDefs(functions)
    val transProps = props.map(transProperty(_, funcName))
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
     haben wollen, filtern wir nach den Funktionen im dictionary.
     */
    var functions: Seq[String] = Seq()
    val allFuncs = funcNameCollector.transFun(func)
    var newFunctions: Seq[String] =
      allFuncs.map(getHygienicName).filter(functionDict.contains).distinct
    while (functions != newFunctions) {
      functions = newFunctions
      newFunctions = (functions ++ functions.flatMap(f =>
        funcNameCollector.transFun(getFunctionDef(f))).map(getHygienicName).filter(functionDict.contains)).distinct
    }
    functions
  }

  // Es wird angenommen, dass der Funktion der hygienische Name übergeben wird
  def getFunctionDef(hygienicName: String): FunctionDef = functionDict.getOrElse(hygienicName,
    throw FunctionNotFoundException(getOriginalName(hygienicName)))

  def getDataDef(hygienicName: String): DataDef = dataDict.getOrElse(hygienicName,
    throw DataNotFoundException(getOriginalName(hygienicName)))

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
        dataNameCollector.transData(getDataDef(d))).map(getHygienicName).filter(dataDict.contains)).distinct
    }
    dataDefs
  }

  // Es wird angenommen, dass der Funktion der hygienische Name übergeben wird
  def transDataDef(dataName: String)(implicit gensym: Gensym): Script = {
    val data = getDataDef(dataName)
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
        throw FunctionNotFoundException(name)
      val invariantFunScript = transFunctionDefs(Seq(hygienicInvariantName))
      val freshVarName = gensym.fresh(dataName)
      val invariantAssertion = SMTlibScripts.invariant(dataName, hygienicInvariantName, freshVarName)
      makeScript(Seq(invariantFunScript, invariantAssertion))
    }))
  }

  // Es wird angenommen, dass der Funktion der hygienische Name übergeben wird
  def verifyPartialOrder(relName: String)(implicit gensym: Gensym): Boolean = {
    val dataName = getParamTypeName(relName)
    val verificationResponse: Map[VerifiableProperty, Response] =
      verificationResponses.getOrElse(getOriginalName(relName), {
        val functions = (collectCalledFunctions(getFunctionDef(relName)) :+ relName).distinct
        val dataDefs = functions.flatMap(fName => collectUsedDataDefs(getFunctionDef(fName))).distinct
        val transDataDefs = dataDefs.map(transDataDef)
        val transFuncDefs = transFunctionDefs(functions)
        val partialOrderVerScript = makeScript(transDataDefs ++ Seq(transFuncDefs,
          SMTlibScripts.reflexivity(relName, dataName),
          SMTlibScripts.transitivity(relName, dataName),
          SMTlibScripts.antisymmetry(relName, dataName)))
        val evaluationResults = evaluateScript(partialOrderVerScript)
        val response: (VerifiableProperty, Response) = PartialOrderAnno -> (if(evaluationResults.forall{
          case VerifiedResponse => true
        }) {
          VerifiedResponse
        } else {
          if (evaluationResults.contains(FalsifiedResponse)) {
            FalsifiedResponse
          } else {
            UnknownResponse
          }
        })
        verificationResponses += getOriginalName(relName) -> Map(response)
        Map(response)
      })
    if(verificationResponse(PartialOrderAnno) == VerifiedResponse) {
      true
    } else {
      false
    }
  }

  // Es wird angenommen, dass der Funktion der hygienische Name übergeben wird
  def transFunctionDefs(funcNames: Seq[String])(implicit gensym: Gensym): Script = {
    val funDecls: mutable.ListBuffer[FunDec] = ListBuffer()
    val funBodys: mutable.ListBuffer[Term]= ListBuffer()
    funcNames.foreach{ funcName =>
      val func = getFunctionDef(funcName)
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
      case _ => throw UnsupportedException(s"Type $typ is currently not supported as paramType")
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
              throw UnexpectedBehaviorException(s"Let binding without variable bindings in this" +
                s"$ex should not have been parsed")
            } else {
              bound match {
                case SetExp(es) => es.map(transExp)
                case _ => throw SyntaxException(s"Expected SetExp containing the bound expressions, but got this $bound")
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
          case (SomePattern(_), _) => throw UnsupportedException("Pattern Matching over " +
            "Some and None not supported, because constructs are not used")
          case (NonePattern(), _) => throw UnsupportedException("Pattern Matching over " +
            "Some and None not supported, because constructs are not used")
          case _ => throw SyntaxException(s"Expected Constructor Pattern, but got this $ex")
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
          case _ => throw SyntaxException(s"Expected qualified identifier in " +
            s"function call, but got this $fun")
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
            case None => throw SyntaxException(s"No type for rhs of $ex could be found")
          }
          case None => throw SyntaxException(s"No type for lhs of $ex could be found")
        }
        infixFun(transExp(left), transExp(right))

      case BaseLit(code) =>
        code.tree match {
          case l: meta.Lit => transMetaLit(l)
          case _ => throw SyntaxException(s"BaseLit $ex does not contain Term.Literal")
        }

      case Tuple(_) => throw UnsupportedException("Tuple expression currently not supported")
      case BaseApply(_, _) => throw UnsupportedException("Tuple expression currently not supported")
      case _ => throw UnexpectedBehaviorException("Matching should be exhaustive")
    }
  }

  // Es wird angenommen, dass der Funktion der hygienische Name übergeben wird
  def transProperty(prop: VerifiableProperty, funName: String)(implicit gensym: Gensym): Script = {
    prop match {
      case aggrProp: AggregationProperty =>
        val paramTypeName = getParamTypeName(funName)
        aggrProp match {
          case Associativity => SMTlibScripts.associativity(funName, paramTypeName)
          case Commutativity => SMTlibScripts.commutativity(funName, paramTypeName)
          case HasUnapply(invName) => SMTlibScripts.hasUnapply(funName, getHygienicName(invName), paramTypeName)
        }
      case anno@SoundnessAnno(concreteFunName, paramBetaName, resultBetaName, partialOrderName) =>
        val hygienicNames = Seq(concreteFunName, paramBetaName, resultBetaName, partialOrderName).map(getHygienicName)
        if (!verifyPartialOrder(hygienicNames(3))) {
          verificationResponses += getOriginalName(funName) -> Map(anno -> FalsifiedResponse)
          Script(List())
        } else {
          val paramTypeName = getParamTypeName(hygienicNames.head)
          SMTlibScripts.soundnessNAry(funName, hygienicNames.head, paramTypeName,
            hygienicNames(1), hygienicNames(2), hygienicNames(3), getNumParams(funName))
        }
      case _ => throw UnexpectedBehaviorException("Matching supposed to be exhaustive")
    }
  }

  def getParamTypeName(funName: String)(implicit gensym: Gensym): String = {
    val func = getFunctionDef(funName)
    func.params.foreach(p => if (p.typ != func.params.head.typ) {
      throw new Exception("Aggregations should take two values of the same type")
    })
    transType(func.params.head.typ).id.symbol.name
  }

  def getNumParams(funName: String): Int = {
    val func = getFunctionDef(funName)
    func.params.size
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

  val infixRealOpsMap: Map[String, String] = Map(
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