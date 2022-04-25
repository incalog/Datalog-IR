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
import inca.frontend.functional.verification.CompileToSMTLIB._

import scala.collection.mutable.ListBuffer


// Functional Program
// Collect functions with verification annotations IncA
// get provable properties IncA
// collect datatype definitions
// compile data types to smt lib
// compile function to smt lib (IncA -> SMTLIB)
// Foreach provable property add provable goal (assertion) (SMTLIB -> SMTLIB)
// execute z3 with smtlib as input
// SMTLIB

trait Response

case object SatisfiedResponse extends Response
case object UnsatisfiedResponse extends Response
case object UnknownResponse extends Response

case class VerifierException(msg: String) extends Exception

class Verifier {

  val varMem: mutable.Map[String, String] = mutable.Map()
  val functionDict: mutable.Map[String, FunctionDef] = mutable.Map()
  val dataDict: mutable.Map[String, DataDef] = mutable.Map()

  // TODO theoretically we need to pass a list of protected words in SMTlib to Gensym,
  //  but Gensym renames everything anyway, so it makes no difference
  /* TODO possible problems with my "hygienic renaming":
      - currently renaming variables, even though there should be no danger of conflict with
        SMTlib protected words: protected words are function names, they should not clash with
        variable names. But variables have to be renamed, because I do not have the context of
        Function or Variable vs Function Call when translating Var construct.
        is there a problem in this case?
          Let(x, 1,
            Let(x, 2,
              x))
        => change transExp?
   */
  def verify(module: Module): Map[String, Map[Property, Response]] = {
    implicit val gensym: Gensym = new Gensym(Seq())
    fillDicts(module)
    val aggregations: Map[String, Seq[Property]] = collectAggregations(module)
    val verificationScripts: Seq[Script] = aggregations.toSeq.map(ag => generateScript(ag._1, ag._2))
    val verificationResults = verificationScripts.zip(aggregations).map(s => getInterpResult(s._1, s._2._2))
    combineVerificationResuls(aggregations, verificationResults)
  }

  def combineVerificationResuls(aggregations: Map[String, Seq[Property]], verificationResults: Seq[Map[Property, Response]]): Map[String, Map[Property, Response]] = {
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

  def getInterpResult(s: Script, props: Seq[Property]): Map[Property, Response] = {
    val interp = Z3Interpreter.buildDefault
    val evalResults: mutable.ListBuffer[Response] = mutable.ListBuffer()
    s.commands.foreach {
      cmd =>
        interp.eval(cmd) match {
          case CommandsResponses.CheckSatStatus(status) =>
            evalResults += (status match {
              case CommandsResponses.SatStatus => UnsatisfiedResponse
              case CommandsResponses.UnsatStatus => SatisfiedResponse
              case CommandsResponses.UnknownStatus => UnknownResponse
            })
          case CommandsResponses.Error(msg) => throw VerifierException(s"z3 error interpreting command $cmd with error message \n ### \n $msg \n ### \n")
          case CommandsResponses.Unsupported => throw VerifierException(s"command $cmd is not supported by z3")
          case _ =>
        }
    }
    props.zip(evalResults).toMap
  }

  // Es wird angenommen, dass der Funktion der hygienische Name übergeben wird
  def generateScript(funcName: String, props: Seq[Property])(implicit gensym: Gensym): Script = {
    val calledFunctions: Seq[String] = collectCalledFunctions(functionDict(funcName))
    val functions = (calledFunctions :+ funcName).distinct
    val dataDefs = functions.flatMap(fName => collectUsedDataDefs(functionDict(fName))).distinct
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
      case UsesInvariantAnno(invariantNames) => Seq(generateInvariantsScript(invariantNames.map(_.name), dataName))
      case _ => Seq()
    }
    val transConstrs = data.constrs.map(transDataConstructor)
    // val freshDataName = gensym.fresh(dataName)
    makeScript(Seq(
      Script(List(DeclareDatatypes(Seq((SSymbol(dataName), transConstrs)))))) ++ invariantScripts)
  }

  // DeclareDatatypes(datatypes: Seq[(SSymbol, Seq[Constructor])])
  // Constructor(sym: SSymbol, fields: Seq[(SSymbol, Sort)])

  def transDataConstructor(c: DataConstructor)(implicit gensym: Gensym): Constructor = {
    val params = c.paramTypes.zipWithIndex.map { case (paramType, idx) =>
      val fieldName = gensym.fresh(s"${c.name.name}_Selector_$idx")
      val sort = transType(paramType)
      (SSymbol(fieldName), sort)
    }
    Constructor(SSymbol(getHygienicName(c.name.name)), params)
  }

  def generateInvariantsScript(invariantNames: Seq[String], dataName: String)(implicit gensym:Gensym): Script = {
    invariantNames.foreach { name =>
      if(!functionDict.contains(getHygienicName(name)))
        throw VerifierException(s"Invariant Function $name called by data $dataName is not implemented")
    }
    makeScript(invariantNames.map(name => {
      val hygienicName = getHygienicName(name)
      val invariantFuncScript = transFunctionDefs(Seq(hygienicName))
      val forallVariableName = gensym.fresh(dataName)
      val invariantAssertion = Script(List(
        Assert(smtForall(Seq(smtSortedVar(forallVariableName, dataName)),
          smtCall("=", Seq(
            smtCall(hygienicName, Seq(
              smtVarCall(forallVariableName)
            )),
            smtTrue()
          ))))
      ))
      // TODO André fragen, ob es sinnvol ist, eine CompileToSMTLIB Klasse zu haben
/*      val invariantAssertion = Script(List(
        Assert(Forall(SortedVar(SSymbol(forallVariableName), Sort(Identifier(SSymbol(dataName)))), Seq(),
          FunctionApplication(QualifiedIdentifier(Identifier(SSymbol("="))),
            Seq(FunctionApplication(QualifiedIdentifier(Identifier(SSymbol(name))),
              Seq(QualifiedIdentifier(Identifier(SSymbol(forallVariableName))))),
              Core.BoolConst(true)))))
      ))*/
      makeScript(Seq(invariantFuncScript, invariantAssertion))
    }))
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
  //DefineFun(funDef: FunDef)
  //FunDef(name: SSymbol, params: Seq[SortedVar], returnSort: Sort, body: Term)
  //SortedVar(name: SSymbol, sort: Sort)

  def transType(typ: Type)(implicit gensym: Gensym): Sort = {
    typ match {
      case TScala(ty) => ty match {
        case Scala(meta.Type.Name("Int")) => Sort(Identifier(SSymbol("Int")))
        case Scala(meta.Type.Name("Boolean")) => Sort(Identifier(SSymbol("Bool")))
        case Scala(meta.Type.Name("Double")) => Sort(Identifier(SSymbol("Real"))) //TODO floating point theory
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
              throw VerifierException("Let ohne variablen")
            } else {
              bound match {
                case SetExp(es) => es.map(transExp)
                // TODO könnte es auch was anderes sein?
                case _ => throw VerifierException("Hier sollte ein Set von expressions stehen")
              }
            }
          }
        }
        val firstBinding = VarBinding(varNames.head, boundTerms.head)
        val otherBindings = varNames.tail.zip(boundTerms.tail).map(x => VarBinding(x._1, x._2))
        Terms.Let(firstBinding, otherBindings, transExp(body))

      // Match(matchee: Expression, cases: Seq[(Pattern, Expression)])
      //ConstructorPattern(constr: Name, args: Seq[Name]) extends Pattern
      //NonePattern() extends Pattern
      //SomePattern(arg: Name) extends Pattern
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
          case _ => ??? // Some und None werden erstmal nicht gebraucht
        }
        smtlib.extensions.tip.Terms.Match(scrut, transCases)
      // Match(scrut: Term, cases: Seq[Case])
      // Case(pattern: Pattern, rhs: Term)
      // Default extends Pattern
      //CaseObject(sym: SSymbol) extends Pattern
      //CaseClass(sym: SSymbol, binders: Seq[SSymbol]) extends Pattern

      //Call(fun: Expression, args: Seq[Expression], transitive: Boolean = false)
      case Call(fun, args, _) =>
        val transFun = transExp(fun)
        val transArgs = args.map(transExp)
        transFun match {
          case q: QualifiedIdentifier => if (transArgs.isEmpty) {
            q
          } else {
            FunctionApplication(q, transArgs)
          }
          case _ => throw new Exception("Wir brauchen bei einem Funktionsaufruf einen qualified identifier")
        }
      //FunctionApplication(fun: QualifiedIdentifier, terms: Seq[Term])

      case If(cnd, thn, els) =>
        FunctionApplication(QualifiedIdentifier(Identifier(SSymbol("ite"))),
          Seq(cnd, thn, els).map(transExp))

      case Lambda(vs, body) =>
        val args = vs.map(v => SortedVar(SSymbol(getHygienicName(v._1.name)), transType(v._2)))
        smtlib.extensions.tip.Terms.Lambda(args, transExp(body))

      // TODO Tuples
      case Tuple(exps) => ???

/*      case BaseApplyInfix(left, op, right) =>
        val qualId = left.typ match {
          case Some(leftType) => right.typ match {
            case Some(rightType) =>
              val exc = new Exception(s"Operator $op on types $leftType and $rightType has no equivalent in SMTlib")
              val typeMap = metaInfixOps.getOrElse((leftType, rightType), throw exc)
                typeMap.getOrElse(op.tree.value, throw exc)
            case None => ???
          }
          case None => ???
        }
        FunctionApplication(qualId, Seq(left, right).map(transExp))*/

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

      case BaseApply(fun, args) =>
        fun.tree match {
          case f: meta.Term.Function =>
            // TODO mögliche Vorgehensweisen für anonyme Funktionen:
            //  1. Rückgabetyp der Übersetzungsfuntkionen ändern und es möglich machen,
            //    die Funktion als Command zurückzugeben, zB
            //      a) Rückgabe zum Tupel mit einer Seq von Commands erweitern,
            //        am besten direkt als FunctionDefs getypet (so würde ich es machen)
            //  2. anonyme Function inlinen
            val funParams = f.params.map {
              ???
            }
            val funBody = transMetaTerm(f.body)
            val funName: String = ???
            ???
          case f: meta.Term.Select =>
            // TODO Gibt es Funktionen die Ich damit vernachlässige?
            throw new Exception("Cannot translate library/class functions")
          case _ => throw new Exception("Function is not a function")
        }
    }
  }

  // Es wird angenommen, dass der Funktion der hygienische Name übergeben wird
  def transProperty(prop: Property, aggrName: String)(implicit gensym: Gensym): Script = {
    val paramTypeName = getParamTypeName(aggrName)
    prop match {
      case Associativity => PropertyScripts.associativity(aggrName, paramTypeName)
      case Commutativity => PropertyScripts.commutativity(aggrName, paramTypeName)
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

  type Property = AggregationProperty


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

  /* +, -, *, /, %, **, ==, !=, >, <, >=, <=, && (bzw and)
    || (bzw or),  &, |, ^, <<, >>,

    keine direkte Entsprechung: +=, -=, *=, /=, %=, **=, <<=, >>=,
    &= (bitw and assign), ^= (bitwise xor and assign), |= (bitw or assign),
    >>> (right shift zero fill)

    Prefix: ! (bzw not),  ~ (bitw ones compl)
  */

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

  val metaInfixRealOps: Map[String, (Term, Term) => Term] = transformInfixMap(Map(
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
  ))

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
  val metaInfixIntRealOps: Map[String, (Term, Term) => Term] = transformInfixMapRightArgToReal(Map(
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
  ))

  val metaInfixRealIntOps: Map[String, (Term, Term) => Term] = transformInfixMapLeftArgToReal(Map(
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
  ))

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

  // TODO nested integer division
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

  def transMetaParam(value: List[meta.Term.Param]): Term = ???

  def transMetaTerm(term: meta.Term): Term = term match {
    case meta.Term.If(term, term1, term2) =>
      FunctionApplication(QualifiedIdentifier(Identifier(SSymbol("ite"))),
        Seq(term, term1, term2).map(transMetaTerm))

    case meta.Term.ApplyInfix(lhs, op, targs, args) =>
      if (args.length != 1) { ???
        //FunctionApplication(meta(op),
        //  Seq(transMetaTerm(lhs)) ++ args.map(transMetaTerm))
      } else {
        // TODO Ich kann hier überprüfen, ob der Infix Operator in SMTlib chainable ist,
        //  vielleicht mit einer Liste von chainable Operatoren in SMTlib?
        throw new Exception("Cannot handle Infix Operation with several rhs arguments")
      }

    case l: meta.Lit => transMetaLit(l)

    case meta.Term.Name(name) => QualifiedIdentifier(Identifier(SSymbol(name)))

    case meta.Term.ApplyUnary(op, arg) =>
      val opName: String = op match {
        case meta.Term.Name(name) => name match {
          case "!" => "not"
          case "-" => "-"
        }
        case _ => throw new Exception("") //TODO
      }
      FunctionApplication(QualifiedIdentifier(Identifier(SSymbol(opName))),
        Seq(transMetaTerm(arg)))

    case meta.Term.Block(value) => ???
    case meta.Term.Match(term, value) => ???
    case meta.Term.Select(qual, name) => ???
    // TODO wenn Ich versuchen will, zu erkennen, ob das eine Integer Conversion
    //  ist, die Ich übersetzen kann, wie mache Ich das? Ich weiß ja nicht, zu
    //  welchem Typ qual auswertet

    case meta.Term.Apply(fun, args) =>
      fun match {
        case meta.Term.Name(name) =>
          val opName = name match {
            case "abs" => "abs" //TODO geht auch nur für Ints
            case _ => ???
          }
          FunctionApplication(QualifiedIdentifier(Identifier(SSymbol(opName))),
            args.map(transMetaTerm))
        case _ => ???
      }

    case _ => throw new Exception("Scala Content cannot be expressed in SMT-lib or is not yet implemented")
  }

}

// implicit val z3Interp = Z3Interepreter.buildDefault
// val script = ..
// Interpreter.execute(script)

// def xyz(script: Script)(implicit interp: Interpreter): ... = {
//   Interpreter.execute(script)
// }

// class X(x: Int)
// val o = new X(1)

// case class X(x: Int)
// val o = X(1)
// val o = X.apply(1)

// generated
// object X {
//   def apply(x: Int): X = new X(x)
//   def unapply(x: X): Option[Int]) = ...
// }
