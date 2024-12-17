package inca.ir.extension.mono.printer

import inca.ir.{Atom, Term, Type}
import inca.ir.extension.mono.{TMono, NewMono, NewMonoFor, ReadMono, WriteMono, MonoDefinition}
import inca.ir.printer.DatalogBaseIRPrinter

trait DatalogPrinter extends DatalogBaseIRPrinter:
  override def prettyPrint(atom: Atom): String = atom match
    case WriteMono(m, input, keys) =>
      val prefix = s"${prettyPrint(m)} += ${prettyPrint(input)}"
      if keys.nonEmpty then prefix + s"@{${keys.map(prettyPrint).mkString(",")}}" else prefix
    case _ => super.prettyPrint(atom)

  override def prettyPrint(term: Term): String = term match
    case NewMono(mono, keys, args) =>
      s"new ${prettyPrint(mono.name)}(${args.map(prettyPrint).mkString(", ")})@{${keys.map(prettyPrint).mkString(",")}}"
    case NewMonoFor(mono, keys, args, uniqueFor) =>
      s"new ${prettyPrint(mono.name)}(${args.map(prettyPrint).mkString(", ")}, ${uniqueFor.map(prettyPrint)})@{${keys.map(prettyPrint).mkString(",")}}"
    case ReadMono(m) =>  s"${prettyPrint(m)}.get"
    case _ => super.prettyPrint(term)

  override def prettyPrint(ty: Type): String = ty match
    case TMono(input, output, keys) =>
      val prefix = s"Mono[${prettyPrint(input)}, ${prettyPrint(output)}]"
      if keys.nonEmpty then prefix + s"@{${keys.map(prettyPrint).mkString(",")}}" else prefix
    case _ => super.prettyPrint(ty)


