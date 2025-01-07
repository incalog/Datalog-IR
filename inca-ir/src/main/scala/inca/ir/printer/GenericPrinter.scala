package inca.ir.printer

import inca.ir.{Arg, Atom, Body, Module, ModuleEntry, Name, Param, Ref, Relation, Substitution, Term, TermType, Type}

trait GenericPrinter:
  def name: String
  def prettyPrint(module: Seq[Module]): String =
    module.map(prettyPrint).mkString("\n\n")
  def prettyPrint(subst: Substitution[?, ?]): String
  def prettyPrint(name: Name): String
  def prettyPrint(ref: Ref[?]): String
  def prettyPrint(module: Module): String
  def prettyPrint(moduleEntry: ModuleEntry): String
  def prettyPrint(body: Body): String
  def prettyPrint(arg: Arg): String
  def prettyPrint(param: Param): String
  def prettyPrint(termTy: TermType): String
  def prettyPrint(ty: Type): String
  def prettyPrint(atom: Atom): String
  def prettyPrint(term: Term): String



