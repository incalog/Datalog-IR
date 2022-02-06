package inca.frontend.functional.verification

import com.sun.jdi.InvalidTypeException
import inca.frontend.functional.core.{Call, Let, Match, _}
import inca.frontend.functional.Collect
import inca.util.Gensym
import smtlib.interpreters.Z3Interpreter
import smtlib.trees.Commands.{Constructor, DeclareDatatypes, Script}
import smtlib.trees.Terms.{Identifier, SSymbol, Sort}

import javax.naming.directory.InvalidAttributeValueException
import scala.collection.mutable
//import smtlib.theories.Ints._
//import smtlib.trees.Terms._
//import smtlib.trees.Commands._


// Functional Program
// Collect functions with verification annotations IncA
// get provable properties IncA
// collect datatype definitions
// compile data types to smt lib
// compile function to smt lib (IncA -> SMTLIB)
// Foreach provable property add provable goal (assertion) (SMTLIB -> SMTLIB)
// execute z3 with smtlib as input
// SMTLIB


class Verifier {

  def module: Module = ???

  val functionDict: mutable.Map[Name, FunctionDef] = mutable.Map()
  val dataDict: mutable.Map[Name, DataDef] = mutable.Map()

  def verify(module: Module): Unit = {
    fillDicts(module)
    val aggregations: Map[Name, Seq[Property]] = collectAggregations(module)
    val verificationScripts: Seq[Script] = aggregations.toSeq.map(ag => generate(ag._1, ag._2))
    implicit val z3Interp: Z3Interpreter = Z3Interpreter.buildDefault
    verificationScripts.foreach(s =>
      smtlib.Interpreter.execute(s)
    )
  }

  def fillDicts(module: Module): Unit = module.content.foreach {
    case d: DataDef => dataDict += d.name -> d
    case f: FunctionDef => functionDict += f.name -> f
  }

  def collectAggregations(module: Module): Map[Name, Seq[Property]] = {
    val aggrCollector = new Collect[(Name, Seq[Property])] {
      override def transFun(func: FunctionDef): Seq[(Name, Seq[Property])] = {
        if (func.annos.exists {
          // TODO Ich benutze main Annotations, weil AggregationAnnos noch nicht
          //  funktionieren (vor allem nicht mit dem Parser)
          case AggregationAnno(props) => false
          case MainFunctionAnno => true
        }) {
          Seq((func.name, getAggrProps(func)))
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

  def generate(funcName: Name, props: Seq[Property]): Script = {
    implicit val gensym = new Gensym(Seq())
    val calledFunctions = collectCalledFunctions(functionDict(funcName))
    val dataDefs = (calledFunctions :+ funcName).flatMap(fName => collectUsedDataDefs(functionDict(fName)))
    val transDataDefs = dataDefs.map(transDataDef)
    val transFuncDefs = calledFunctions.map(transFunctionDef)
    val transProps = props.map(transProperty)
    makeScript(transDataDefs ++ transFuncDefs ++ transProps)
  }

  def collectCalledFunctions(func: FunctionDef): Seq[Name] = {
    // TODO okay das mit vars zu machen?
    //val functions: mutable.Seq[Name] = mutable.Seq()
    //val newFunctions: mutable.Seq[Name] = mutable.Seq(CollectCalledFunctionNames.transFun(func))
    //while (functions != newFunctions) {
    //  functions = newFunctions
    //}
    val funcNameCollector = new Collect[Name] {
      override def transExp(exp: Expression): Seq[Name] = exp match {
        case Call(fun, args, _) => fun match {
          case Var(name) => Seq(name) ++ args.flatMap(super.transExp)
          case _ => super.transExp(exp)
        }
        case _ => super.transExp(exp)
      }
    }
    /*
     Da auch Konstruktoraufrufe als Funktionsaufrufe gestaltet sind, wir aber nur "echte Funktionsaufrufe"
     haben wollen, filtern wir nach den Funktionen im dictionary. TODO imports des Moduls
     */
    var functions: Seq[Name] = Seq()
    var newFunctions: Seq[Name] = funcNameCollector.transFun(func).filter(functionDict.contains).distinct
    while (functions != newFunctions) {
      functions = newFunctions
      newFunctions = (functions ++ functions.flatMap(f =>
        funcNameCollector.transFun(functionDict(f))).filter(functionDict.contains)).distinct
    }
    functions
  }

  def collectUsedDataDefs(func: FunctionDef): Seq[Name] = {
    val dataNameCollector = new Collect[Name] {
      override def transType(t: Type): Seq[Name] = t match {
        case TData(name) => Seq(name)
        case _ => super.transType(t)
      }
    }
    var dataDefs: Seq[Name] = Seq()
    var newDataDefs: Seq[Name] = dataNameCollector.transFun(func).filter(dataDict.contains).distinct
    while (dataDefs != newDataDefs) {
      dataDefs = newDataDefs
      newDataDefs = (dataDefs ++ dataDefs.flatMap(d =>
        dataNameCollector.transData(dataDict(d))).filter(dataDict.contains)).distinct
    }
    dataDefs
  }

  // DataDef(annos: Seq[Annotation], vis: Option[Visibility], name: Name, constrs: Seq[DataConstructor])
  // DataConstructor(name: Name, paramTypes: Seq[Type])

  def transDataDef(dataName: Name)(implicit gensym: Gensym): Script = {
    val data = dataDict(dataName)
    val transConstrs = data.constrs.map(c =>
      Constructor(SSymbol(c.name.name),
        c.paramTypes.map(paramType => {
          val fieldName = gensym.fresh(c.name.name)
          val sort = paramType match {
            // TODO macht es Sinn, irgendeinen Error zu wählen, der zur Situation passt?
            case TScala(ty) => ty match {
              case scala.meta.Type.Name("Int") => Sort(Identifier(SSymbol("Int")))
              case scala.meta.Type.Name("Boolean") => Sort(Identifier(SSymbol("Bool")))
              case scala.meta.Type.Name("Double") => Sort(Identifier(SSymbol("Real")))
              case scala.meta.Type.Name("String") => ???
            }
            case TData(name) => Sort(Identifier(SSymbol(name.name)))
            case _ => throw new InvalidAttributeValueException("Constructor parameter type needs to be specified")
          }
          (SSymbol(fieldName), sort)
        }))
    )
    Script(List(DeclareDatatypes(Seq((SSymbol(dataName.name), transConstrs)))))
  }

  // DeclareDatatypes(datatypes: Seq[(SSymbol, Seq[Constructor])])
  // Constructor(sym: SSymbol, fields: Seq[(SSymbol, Sort)])

  def transFunctionDef(funcName: Name): Script = {
    val func = functionDict(funcName)
    Script(List())
  }

  def transProperty(prop: Property): Script = ???
  //prop match {
  //case Assoc => transAsssoc(name)
  //case Commutativity => transCommu(name)


  def makeScript(scripts: Seq[Script]): Script = ???

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
