package inca.ir.execution.interpreter

import inca.ir
import inca.ir.{CompiledUnit, Name}
import inca.ir.analysis.IRConcreteInterpreter
import inca.ir.analysis.base.values.{ConcreteRelation, Value}
import inca.ir.execution.{ADT, ExecutorEngine, IRExecutor, Relation, RelationName, RelationUpdateListener, UnitRelation, transformEDBInput}
import inca.ir.extension.arithmetic.{TDouble, TInt}
import inca.ir.extension.arithmetic.analysis.interpreter.{CDoubleV, CIntV}
import inca.ir.extension.data.{CaseDefinition, TData}
import inca.ir.extension.string.analysis.interpreter.CStringV
import inca.ir.extension.data.analysis.interpreter.CDataV
import inca.ir.extension.string.TString
import sturdy.values.references.AllocationSiteAddr

// TODO: Support Scala code
// TODO: Support incremental updates
class Executor extends IRExecutor:
  class Engine(mods: Seq[ir.Module]) extends ExecutorEngine:
    private var inputDirty = true
    private var cachedResult: Option[Map[String, Relation]] = None

    val interp = IRConcreteInterpreter()

    private def interp(mods: Seq[ir.Module], useCache: Boolean = true): Map[String, Relation] =
      if (!useCache || inputDirty || cachedResult.isEmpty) {
        interp.resetIDB()
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
              val emptyTable = ConcreteRelation[Value](rel.params.map(_.name.name), Set())
              InterpreterRelation(rel.name.name, emptyTable)
          rel.name.name -> out
        }.toMap

        inputDirty = false
        cachedResult = Some(res)
      }
      cachedResult.get

    def filter(idb: Map[String, Relation], query: Relation): Relation = idb.get(query.name) match
      case Some(rel) if query.isEmpty => rel
      case Some(rel) =>
        val matches = rel.entries.flatMap { el =>
          val flatEl = rel.flattenEntry(el)
          val matches = query.entries.exists { qt =>
            val flatQuery = query.flattenEntry(qt)
            flatEl.zipAll(flatQuery, null, null).forall {
              case (e, null) => true
              case (e, q) => e == q
            }
          }
          if (matches) Some(flatEl) else None
        }
        Relation.from(query.name, query.parameterNames, matches)
      case _ => throw IllegalArgumentException(s"Can not find relation ${query.name}")

    override def measure(rel: Relation): Long =
      val start = System.nanoTime()
      filter(interp(mods, useCache = false), rel)
      System.nanoTime() - start

    override def read(rel: Relation): Relation =
      filter(interp(mods), rel)

    override def readAll(): Seq[Relation] =
      interp(mods).values.toSeq

    private def transformADT(dataName: String, caseName: String, args: Seq[Any]): Any =
      val argTys = args.map {
        case _: CIntV => TInt
        case _: CDoubleV => TDouble
        case _: CStringV => TString
        case CDataV(caseDef, _) => caseDef.data
      }
      val caseDef = CaseDefinition(Name(caseName), argTys, TData(Name(dataName)))
      CDataV(caseDef, args.map(_.asInstanceOf[Value]))

    private def interpretfyTupleEntry(v: Any): Value =
      transformEDBInput(v)(CStringV.apply, CIntV.apply, CDoubleV.apply, transformADT).asInstanceOf[Value]

    private def relationToCRV(rel: Relation) =
      ConcreteRelation[Value](rel.parameterNames, rel.entries.map { e =>
        rel.flattenEntry(e).map(interpretfyTupleEntry)
      }.toSet)

    override def insert(edb: Relation): Unit =
      inputDirty = true
      interp.insertEDB(edb.name, relationToCRV(edb))


    override def remove(edb: Relation): Unit =
      inputDirty = true
      interp.removeEDB(edb.name, relationToCRV(edb))

    override def addUpdateListener(up: RelationUpdateListener): Unit =
      throw IllegalArgumentException("Update listener is not supported")

    override def removeUpdateListener(up: RelationUpdateListener): Unit =
      throw IllegalArgumentException("Update listener is not supported")

  override def instantiate(m: CompiledUnit): Engine =
    new Engine(m.lowered)

// Lazy conversion of values
case class InterpreterRelation(name: String, table: ConcreteRelation[Value]) extends Relation:
  private var evaled: Boolean = false
  lazy val outputRel =
    evaled = true

    val queryMatches: Iterable[Seq[Any]] = table.rows.map { vs =>
      vs.map {
        case CIntV(i) => i
        case CDoubleV(d) => d
        case CStringV(s) => s
        case v@CDataV(caseName, args) => v.toString // TODO: Generate Scala ADT class at runtime?
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