package inca.lattice.verification

object z3 {

  /*
  Design Entscheidungen:
  - Lasse Indices weg und beschränke Identifiers auf Symbols
  -
  */

  //SPECIAL CONSTANTS (S_EXPRESSIONS)
  // TODO Ist es sinnvoll spec-const so umzusetzen/überhaupt umzusetzen?
  trait Constant
  case class NumeralConstant(i: Integer) extends Constant {
    override def toString: String = i.toString
  }
  case class StringConstant(s: String) extends Constant {
    override def toString: String = s"""\"$s\""""
  }

  // SORTS
  trait Sort
  case class NonParametricSort(name: String) extends Sort {
    override def toString: String = name
  }
  case class ParametricSort(id: String, params: Seq[Sort]) extends Sort {
    override def toString: String = s"($id ${params.mkString(" ")}"
  }
  val ZInt: NonParametricSort = NonParametricSort("Int")

  // TERMS AND FORMULAS
  case class VariableBinding(name: String, bound: Term) {
    override def toString: String = s"($name $bound)"
  }
  case class SortedVariable(name: String, sort: Sort) {
    override def toString: String = s"($name $sort)"
  }
  trait Pattern
  case class NonParametricPattern(name: String) extends Term {
    override def toString: String = name
  }
  case class ParametricPattern(name: String, params: Seq[String]) extends Pattern {
    override def toString: String = s"($name ${params.mkString(" ")})"
  }
  case class MatchCase(pattern: Pattern, body: Term) {
    override def toString: String = s"($pattern $body)"
  }

  // TERMS
  trait Term
  case class ConstantTerm(value: Constant) extends Term {
    override def toString: String = value.toString
  }
  case class Identifier(id: String) extends Term {
    override def toString: String = id
  }
  case class Call(id: Identifier, params: Seq[Term]) extends Term {
    override def toString: String = s"($id ${params.mkString(" ")})"
  }
  case class Let(bindings: Seq[VariableBinding], body: Term) extends Term {
    override def toString: String = s"(let (${bindings.mkString(" ")}) $body)"
  }
  case class Forall(vars: Seq[SortedVariable], body: Term) extends Term {
    override def toString: String = s"(forall (${vars.mkString(" ")}) $body)"
  }
  case class Exists(vars: Seq[SortedVariable], body: Term) extends Term {
    override def toString: String = s"(exists (${vars.mkString(" ")}) $body)"
  }
  case class Match(matchee: Term, cases: Seq[MatchCase]) extends Term {
    override def toString: String = s"(match $matchee (${cases.mkString(" ")}))"
  }
  // TODO Attributes lass ich auch erstmal weg
  // TODO Theory Declarations und Logic Declarations werde ich wohl nicht brauchen

  // OPTIONS
  trait BooleanValue
  case class True() extends BooleanValue {
    override def toString: String = "true"
  }
  case class False() extends BooleanValue {
    override def toString: String = "false"
  }
  trait Option
  case class PrintSuccessOption(b: Boolean) extends Option {
    override def toString: String = s":print-success $b"
  }
  case class ProduceProofsOption(b: Boolean) extends Option {
    override def toString: String = s":produce-proofs $b"
  }

  // COMMANDS
  case class SortDeclaration(name: String, num: NumeralConstant) {
    override def toString: String = s"($name $num)"
  }
  case class SelectorDeclaration(name: String, sort: Sort) {
    override def toString: String = s"($name $sort)"
  }
  case class ConstructorDeclaration(name: String, selectors: Seq[SelectorDeclaration]) {
    override def toString: String = s"($name ${selectors.mkString(" ")})"
  }
  // TODO Ich lasse die Datatype Declaration mit par mal aus, da ich sie nie brauchte.
  case class FunctionDeclaration(name: String, params: Seq[SortedVariable], returnSort: Sort) {
    override def toString: String = s"($name (${params.mkString(" ")}))"
  }
  case class FunctionDefinition(name: String, params: Seq[SortedVariable], returnSort: Sort,body: Term) {
    override def toString: String = s"$name (${params.mkString(" ")}) $returnSort $body"
  }
  // TODO prop_literal und check-sat-assuming brauche ich auch nicht, denke ich
  trait Command
  case class Assertion(term: Term) extends Command {
    override def toString: String = s"(assert $term)"
  }
  case class CheckSat() extends Command {
    override def toString: String = s"(check-sat)"
  }
  case class ConstantDeclaration(name: String, sort: Sort) extends Command {
    override def toString: String = s"(declare-const $name $sort)"
  }
  // TODO Abweichung von der Grammatik, da ich die datatype declaration mit par weggelassen habe
  case class DatatypeDeclaration(name: String, constructors: Seq[ConstructorDeclaration]) extends Command {
    override def toString: String = s"(declare-datatype $name (${constructors.mkString(" ")}))"
  }
  case class CommandFunctionDeclaration(name: String, paramSorts: Seq[Sort], returnSort: Sort) extends Command {
    override def toString: String = s"(declare-fun $name (${paramSorts.mkString(" ")}) $returnSort)"
  }
  case class CommandSortDeclaration(name: String, arity: Int) extends Command {
    override def toString: String = s"(declare-sort $name $arity)"
  }
  case class CommandFunctionDefinition(fundef: FunctionDefinition) extends Command {
    override def toString: String = s"(define-fun $fundef)"
  }
  // TODO Gegebenenfalls rekursive und mutual-rekursive Funktionen ergänzen
  case class SortDefinition(name: String, paramSorts: Seq[String], body: Sort) extends Command {
    override def toString: String = s"($name (${paramSorts.mkString(" ")}) $body)"
  }
  case class Echo(text: String) extends Command {
    override def toString: String = s"(echo $text)"
  }
  case class Exit() extends Command {
    override def toString: String = "(exit)"
  }
  case class GetModel() extends Command {
    override def toString: String = "(get-model)"
  }
  case class GetProof() extends Command {
    override def toString: String = "(get-proof)"
  }
  // TODO Ich glaube die z3 Syntax unterscheidet sich hier von der smtlib Syntax:
  //  Man kann kein Numeral in Push und Pop angeben
  case class Push() extends Command {
    override def toString: String = "(push)"
  }
  case class Pop() extends Command {
    override def toString: String = "(pop)"
  }
  case class Reset() extends Command {
    override def toString: String = "(reset)"
  }
  case class SetOption(op: Option) extends Command {
    override def toString: String = s"(set-option $op)"
  }

  case class Script(commands: Seq[Command]) {
    override def toString: String = commands.mkString("\n")
  }
}
