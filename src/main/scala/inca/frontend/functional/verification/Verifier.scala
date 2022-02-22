package inca.frontend.functional.verification

import com.sun.jdi.InvalidTypeException
import inca.frontend.functional.core.{Call, Let, Match, _}
import inca.frontend.functional.Collect
import inca.util.{Gensym, Scala}
import smtlib.extensions.tip.Terms.{Case, CaseClass, CaseObject}
import smtlib.interpreters.Z3Interpreter
import smtlib.trees.Commands.{Constructor, DeclareDatatypes, DefineFun, FunDef, Script}
import smtlib.trees.Terms
import smtlib.trees.Terms.{Identifier, SSymbol, Sort, SortedVar, _}

import javax.naming.directory.InvalidAttributeValueException
import scala.collection.mutable


// Functional Program
// Collect functions with verification annotations IncA
// get provable properties IncA TODO
// collect datatype definitions
// compile data types to smt lib
// compile function to smt lib (IncA -> SMTLIB)
// Foreach provable property add provable goal (assertion) (SMTLIB -> SMTLIB) TODO
// execute z3 with smtlib as input TODO
// SMTLIB


class Verifier {

  def module: Module = ???

  val functionDict: mutable.Map[String, FunctionDef] = mutable.Map()
  val dataDict: mutable.Map[String, DataDef] = mutable.Map()

  def verify(module: Module): Unit = {
    fillDicts(module)
    val aggregations: Map[String, Seq[Property]] = collectAggregations(module)
    val verificationScripts: Seq[Script] = aggregations.toSeq.map(ag => generateScript(ag._1, ag._2))
    implicit val z3Interp: Z3Interpreter = Z3Interpreter.buildDefault
    verificationScripts.foreach(s =>
      smtlib.Interpreter.execute(s)
    )
  }

  def fillDicts(module: Module): Unit = module.content.foreach {
    case d: DataDef => dataDict += d.name.name -> d
    case f: FunctionDef => functionDict += f.name.name -> f
  }

  def collectAggregations(module: Module): Map[String, Seq[Property]] = {
    val aggrCollector = new Collect[(String, Seq[Property])] {
      override def transFun(func: FunctionDef): Seq[(String, Seq[Property])] = {
        if (func.annos.exists {
          // TODO Ich benutze main Annotations, weil AggregationAnnos noch nicht
          //  funktionieren (vor allem nicht mit dem Parser)
          case AggregationAnno(_) => true
          case _ => false
        }) {
          Seq((func.name.name, getAggrProps(func)))
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

  def generateScript(funcName: String, props: Seq[Property]): Script = {
    implicit val gensym: Gensym = new Gensym(Seq())
    val calledFunctions: Seq[String] = collectCalledFunctions(functionDict(funcName))
    val dataDefs = (calledFunctions :+ funcName).flatMap(fName => collectUsedDataDefs(functionDict(fName)))
    val transDataDefs = dataDefs.map(transDataDef)
    val functions = calledFunctions:+funcName
    val transFuncDefs = functions.map(transFunctionDef)
    val transProps = props.map(transProperty(_, funcName))
    makeScript(transDataDefs ++ transFuncDefs ++ transProps)
  }

  def collectCalledFunctions(func: FunctionDef): Seq[String] = {
    // TODO okay das mit vars zu machen?
    //val functions: mutable.Seq[String] = mutable.Seq()
    //val newFunctions: mutable.Seq[String] = mutable.Seq(CollectCalledFunctionNames.transFun(func))
    //while (functions != newFunctions) {
    //  functions = newFunctions
    //}
    val funcNameCollector = new Collect[String] {
      override def transExp(exp: Expression): Seq[String] = exp match {
        case Call(Var(name), args, _) =>
       Seq(name.name) ++ args.flatMap(super.transExp)
        case _ => super.transExp(exp)
      }
    }
    /*
     Da auch Konstruktoraufrufe als Funktionsaufrufe gestaltet sind, wir aber nur "echte Funktionsaufrufe"
     haben wollen, filtern wir nach den Funktionen im dictionary. TODO imports des Moduls
     */
    var functions: Seq[String] = Seq()
    var newFunctions: Seq[String] = funcNameCollector.transFun(func).filter(functionDict.contains).distinct
    while (functions != newFunctions) {
      functions = newFunctions
      newFunctions = (functions ++ functions.flatMap(f =>
        funcNameCollector.transFun(functionDict(f))).filter(functionDict.contains)).distinct
    }
    functions
  }

  def collectUsedDataDefs(func: FunctionDef): Seq[String] = {
    val dataNameCollector = new Collect[String] {
      override def transType(t: Type): Seq[String] = t match {
        case TData(name) => Seq(name.name)
        case _ => super.transType(t)
      }
    }
    var dataDefs: Seq[String] = Seq()
    var newDataDefs: Seq[String] = dataNameCollector.transFun(func).filter(dataDict.contains).distinct
    while (dataDefs != newDataDefs) {
      dataDefs = newDataDefs
      newDataDefs = (dataDefs ++ dataDefs.flatMap(d =>
        dataNameCollector.transData(dataDict(d))).filter(dataDict.contains)).distinct
    }
    dataDefs
  }

  // DataDef(annos: Seq[Annotation], vis: Option[Visibility], name: String, constrs: Seq[DataConstructor])
  // DataConstructor(name: String, paramTypes: Seq[Type])

  def transDataDef(dataName: String)(implicit gensym: Gensym): Script = {
    val data = dataDict(dataName)
    val transConstrs = data.constrs.map(c =>
      Constructor(SSymbol(c.name.name),
        c.paramTypes.map(paramType => {
          val fieldName = gensym.fresh(c.name.name)
          val sort = transType(paramType)
          (SSymbol(fieldName), sort)
        }))
    )
    Script(List(DeclareDatatypes(Seq((SSymbol(dataName), transConstrs)))))
  }

  // DeclareDatatypes(datatypes: Seq[(SSymbol, Seq[Constructor])])
  // Constructor(sym: SSymbol, fields: Seq[(SSymbol, Sort)])

  //FunctionDef(annos: Seq[Annotation], vis: Option[Visibility],
  //  name: String, params: Seq[Param], outType: Type, body: Expression)
  //Param(name: String, typ: Type)
  def transFunctionDef(funcName: String): Script = {
    val func = functionDict(funcName)
    val transParams: Seq[SortedVar] = func.params.map(p => SortedVar(SSymbol(p.name.name), transType(p.typ)))
    val transOutType: Sort = transType(func.outType)
    val transBody: Term = transExp(func.body)
    Script(List(DefineFun(FunDef(SSymbol(func.name.name), transParams, transOutType, transBody))))
  }
  //DefineFun(funDef: FunDef)
  //FunDef(name: SSymbol, params: Seq[SortedVar], returnSort: Sort, body: Term)
  //SortedVar(name: SSymbol, sort: Sort)

  def transType(typ: Type): Sort = {
    typ match {
      // TODO macht es Sinn, irgendeinen Error zu wählen, der zur Situation passt?
      case TScala(ty) => ty match {
        case Scala(scala.meta.Type.Name("Int")) => Sort(Identifier(SSymbol("Int")))
        case Scala(scala.meta.Type.Name("Boolean")) => Sort(Identifier(SSymbol("Bool")))
        case Scala(scala.meta.Type.Name("Double")) => Sort(Identifier(SSymbol("Real"))) //TODO floating point theory
        case Scala(scala.meta.Type.Name("String")) => Sort(Identifier(SSymbol("String")))
      }
      case TData(name) => Sort(Identifier(SSymbol(name.name)))
      // TODO andere Cases
      case _ => throw new Exception("Type needs to be specified")
    }
  }

  def transExp(ex: Expression): Term = {
    ex match {
      case Var(name) => QualifiedIdentifier(Identifier(SSymbol(name.name)))

      case Let(names, anno, bound, body) =>
        val varNames: Seq[SSymbol] = names.map(name => SSymbol(name.name))
        val boundTerms: Seq[Term] = if (names.length == 1) {
          Seq(transExp(bound))
        } else {
          if (names.length < 1) {
            throw new Exception("Let ohne variablen")
          } else {
            bound match {
              case SetExp(es) => es.map(transExp)
              // TODO könnte es auch was anderes sein?
              case _ => throw new Exception("Hier sollte ein Set von expressions stehen")
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
              CaseObject(SSymbol(constr.name))
            } else {
              CaseClass(SSymbol(constr.name), args.map(arg => SSymbol(arg.name)))
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
      case Call(fun, args, transitive) =>
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

      case BaseApplyInfix(left, op, right) =>
        val transOp = op.tree.value match {
          // TODO bei Bedarf mehr Infix Operatoren hinzufügen (Was, wenn das nicht in Core theory
          //  enthalten ist? Extra FunctionDefinition?)
          case "==" => "="
          case _ => op.tree.value
        }
        FunctionApplication(QualifiedIdentifier(Identifier(SSymbol(transOp))), Seq(left, right).map(transExp))

      case SetExp(es) => ???
    }
  }


  def transProperty(prop: Property, aggrName: String): Script = {
    val paramTypeName = getParamTypeName(aggrName)
    prop match {
      case Associativity => PropertyScripts.associativity(aggrName, paramTypeName)
      case Commutativity => PropertyScripts.commutativity(aggrName, paramTypeName)
    }
  }

  def getParamTypeName(aggrName: String): String = {
    val func = functionDict(aggrName)
    func.params.foreach(p => if(p.typ != func.params.head.typ){
      throw new Exception("Aggregations should take two values of the same type")
    })
    transType(func.params.head.typ).id.symbol.name
  }


  def makeScript(scripts: Seq[Script]): Script = {
    Script(scripts.flatMap(s => s.commands).toList)
  }

  type Property = AggregationProperty

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
