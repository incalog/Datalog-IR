package inca.frontend.objectoriented.datalog

import inca.backend.transform.magic.demand.DemandTransformation.demandPatternExtensionalPrefix
import inca.compiler.CompiledModule
import inca.frontend.Constants.RelationName
import inca.frontend.datalog.{EDBChange, Relation}
import inca.runtime.Query.Specification
import inca.util.Scala.ScalaCompiler
import truechange.EditScript
import truediff.Diffable

import scala.jdk.CollectionConverters.CollectionHasAsScala

protected[datalog] final case class ObjectOrientedInput(terms: Seq[meta.Term], compiled: CompiledModule, relName: RelationName) {
  lazy val (change, args, diffables) = input(terms)

  private lazy val scalaCompiler: ScalaCompiler = new ScalaCompiler
  private lazy val loadedPsystemModule: String = scalaCompiler.define {
    import scala.meta._
    q"object O {..${compiled.psystemSource.stats}}".syntax
  }
  private lazy val specification: Specification = compiled.psystemModule.patterns(relName)()

  private def input(args: Seq[meta.Term]): (EDBChange, Relation, Seq[AnyRef]) = {
    val (ess, cargs, diffables) = vals(args: _*).map {
      case arg: Diffable => (arg.loadEdits, arg.uri, arg)
      case lit => (EditScript(Seq()), lit, lit)
    }.unzip3
    val paramNames = specification.getParameterNames.asScala.toSeq
    val inputParamNames = paramNames.slice(0, cargs.size)
    val rel = Relation.from(relName, inputParamNames, Seq(cargs))

    val adornment = "b".repeat(inputParamNames.size) + "f".repeat(paramNames.size-inputParamNames.size)

    val es = EditScript(ess.flatMap(_.edits))
    val insert = Relation.from(demandPatternExtensionalPrefix + relName + "$" + adornment, inputParamNames, Seq(cargs))
    val change = EDBChange(es, Seq(insert), Seq())
    (change, rel, diffables)
  }

  private def vals(ts: meta.Term*): Seq[AnyRef] = {
    ts.map(a => {
      val syntax = s"{import ${loadedPsystemModule}.${compiled.name}._; ${a.syntax}}"
      scalaCompiler.compileAndLoadScala[AnyRef](syntax)
    })
  }

  lazy val inheritanceEDB: Seq[Relation] = {
    val dataModel = compiled.dataModel
    val allTypes = dataModel.types
    val identitySubtypes = allTypes.map(typ => Seq(typ.name, typ.name))
    val realSubtypes = dataModel.nodeSupertypes.map { case (child, parent) => Seq(child.name, parent.name) }

    // FIXME: This is only required as long as we don't have negation for ExtensionalCall
    val notSubtypes = allTypes.flatMap { ty =>
      val tySupertypes = dataModel.nodeSupertypes.get(ty)
      (allTypes - ty).diff(tySupertypes).map { notSubtype =>
        Seq(ty.name, notSubtype.name)
      }
    }

    val subTypeRel = Relation.from("subtype", Seq("child", "parent"), identitySubtypes ++ realSubtypes)
    val notSubTypeRel = Relation.from("not#subtype", Seq("child", "parent"), notSubtypes)
    Seq(subTypeRel, notSubTypeRel)
  }
}