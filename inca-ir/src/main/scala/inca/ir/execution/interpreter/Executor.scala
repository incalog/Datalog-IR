package inca.ir.execution.interpreter

import inca.ir
import inca.ir.CompiledUnit
import inca.ir.analysis.IRConcreteInterpreter
import inca.ir.analysis.base.values.{CRelationValue, Value}
import inca.ir.execution.{ExecutorEngine, IRExecutor, Relation, RelationName, RelationUpdateListener, UnitRelation}
import inca.ir.extension.arithmetic.analysis.interpreter.{CDoubleV, CIntV}
import sturdy.values.references.AllocationSiteAddr

// TODO: Support Scala code
// TODO: Support incremental updates
class Executor extends IRExecutor:
  class Engine(mods: Seq[ir.Module]) extends ExecutorEngine:
    private var inputDirty = true
    private var cachedResult: Option[Map[String, Relation]] = None

    private def interp(mods: Seq[ir.Module], useCache: Boolean = true): Map[String, Relation] =
      if (!useCache || inputDirty || cachedResult.isEmpty) {
        val interp = IRConcreteInterpreter()
        interp.evalProgram(mods)
        val idb = interp.idb.getState
        val allRels = mods.flatMap(_.relations.values)
        val res = allRels.map { rel =>
          val addr = AllocationSiteAddr.Variable(rel.name.name)(true)
          val out = idb.get(addr) match
            case Some(crv) => 
              InterpreterRelation(rel.name.name, crv)
            case None =>
              // In case a relation failed
              val emptyTable = CRelationValue[Value](rel.params.map(_.name.name), Set())
              InterpreterRelation(rel.name.name, emptyTable)
          rel.name.name -> out
        }.toMap
        
        cachedResult = Some(res)
      }
      cachedResult.get

    def filter(res: Map[String, Relation], query: Relation): Relation =
      val rel = res.getOrElse(query.name, throw IllegalArgumentException(s"Can not find relation ${query.name}"))
      val matches = rel.entries.flatMap { el =>
        val flatEl = rel.flattenEntry(el)
        val matches = rel.entries.exists { query =>
          val flatQuery = rel.flattenEntry(query)
          flatEl.zipAll(flatQuery, null, null).forall {
            case (e, null) => true
            case (e, q) => e == q
          }
        }
        if (matches) Some(flatEl) else None
      }
      Relation.from(query.name, query.parameterNames, matches)

    override def measure(rel: Relation): Long =
      val start = System.nanoTime()
      filter(interp(mods, useCache = false), rel)
      System.nanoTime() - start

    override def read(rel: Relation): Relation =
      filter(interp(mods), rel)

    override def readAll(): Seq[Relation] =
      interp(mods).values.toSeq

    override def insert(edb: Relation): Unit =
      inputDirty = true
      // TODO: handle edb

    override def remove(edb: Relation): Unit =
      inputDirty = true
      // TODO: Handle edb
      ???

    override def addUpdateListener(up: RelationUpdateListener): Unit =
      throw IllegalArgumentException("Update listener is not supported")

    override def removeUpdateListener(up: RelationUpdateListener): Unit =
      throw IllegalArgumentException("Update listener is not supported")

  override def instantiate(m: CompiledUnit): Engine =
    new Engine(m.lowered)

// Lazy conversion of values
case class InterpreterRelation(name: String, table: CRelationValue[Value]) extends Relation:
  private var evaled: Boolean = false
  lazy val outputRel =
    evaled = true

    val queryMatches: Iterable[Seq[Any]] = table.rows.map { vs =>
      vs.map {
        case CIntV(i) => i
        case CDoubleV(d) => d
      }
    }

    Relation.from(name, parameterNames, queryMatches)

  override type Tuple = Any

  override def arity: Int = table.cols.size

  override def parameterNames: Seq[String] = table.cols

  override def size: Int = table.size

  override def entries: Iterable[Tuple] = outputRel.entries

  override def unflattenEntry(entry: Seq[Any]): Any = outputRel.unflattenEntry(entry)

  override def matches: Iterable[Seq[Any]] = outputRel.matches

  override def toString: RelationName =
    val entriesS =
      if (evaled)
        matches.map { e =>
          parameterNames.zip(e).map { case (name, value) =>
            s"$name: $value"
          }.mkString("(", ", ", ")")
        }.mkString("{", ", ", "}")
      else
        "?"
    s"${getClass.getSimpleName}(name: $name, size: $size, entries: $entriesS)"