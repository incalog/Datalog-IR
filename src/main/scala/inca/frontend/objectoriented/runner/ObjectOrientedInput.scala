package inca.frontend.objectoriented.runner

import inca.compiler.CompiledModule
import inca.frontend.runner.{EDBChange, Input, Relation}
import inca.runtime.Query.Specification
import inca.util.Scala.ScalaCompiler
import org.eclipse.viatra.query.runtime.matchers.tuple.{Tuple, Tuples}
import truechange.EditScript
import truediff.Diffable

import scala.jdk.CollectionConverters.CollectionHasAsScala


case class ObjectOrientedInput(terms: meta.Term*)
  extends Input {

  lazy val (change, args) = input(terms)

  private lazy val scalaCompiler: ScalaCompiler = new ScalaCompiler
  private lazy val loadedPsystemModule: String = scalaCompiler.define {
    import scala.meta._
    q"object O {..${compiled.psystemSource.stats}}".syntax
  }
  private lazy val specification: Specification = compiled.psystemModule.patterns(patName)()

  private def input(args: Seq[meta.Term]): (EDBChange, Relation) = {
    val (ess, cargs, _) = vals(args: _*).map {
      case arg: Diffable => (arg.loadEdits, arg.uri, arg)
      case lit => (EditScript(Seq()), lit, lit)
    }.unzip3
    val es = EditScript(ess.flatMap(_.edits))
    val change = EDBChange.structural(es)
    val paramNames = specification.getParameterNames.asScala.toSeq
    val rel = Relation.from("", paramNames, Seq(cargs))
    (change, rel)
  }

  private def vals(ts: meta.Term*): Seq[AnyRef] = {
    ts.map(a => {
      val syntax = s"{import ${loadedPsystemModule}.${compiled.name}._; ${a.syntax}}"
      scalaCompiler.compileAndLoadScala[AnyRef](syntax)
    })
  }
}
