package inca.frontend.datalog.casestudy

import inca.frontend.datalog.compile.DatalogCompilerOptions
import inca.frontend.datalog.executor.DatalogExecutor
import inca.frontend.datalog.executor.DatalogExecutor.?
import inca.ir.execution.{Relation, Relation1, Relation2, Relation3, RelationUpdateListener}
import inca.util.FileUtil
import org.eclipse.viatra.query.runtime.rete.matcher.DRedReteBackendFactory
import org.scalatest.Ignore
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.mutable.ListBuffer

@Ignore
class ConstantAnalysisTest extends AnyFunSuite:
  val pipeline = List()
  val options = DatalogCompilerOptions.fromResource("datalog/Options.ini")
  options.irLogging.logModule = false
  val exec: DatalogExecutor = new DatalogExecutor(new inca.viatra.Executor(DRedReteBackendFactory.INSTANCE))

  var nextId: Int = 0

  import Expr.*
  enum Expr:
    case Num(value: Int)
    case Add(lhs: Expr, rhs: Expr)
    case Var(name: String)

    val id: Int = nextId
    nextId += 1

    override def toString: String = this match
      case Num(value) => s"$value"
      case Add(lhs, rhs) => s"$lhs + $rhs"
      case Var(name) => name

    override def equals(obj: Any): Boolean = obj match
      case that: Expr => this.id == that.id
      case _ => false
    override def hashCode(): Int = id

    def expressions: Seq[Expr] =
      this match
        case e@Add(lhs, rhs) => e +: (lhs.expressions ++ rhs.expressions)
        case e => Seq(e)

  import Stmt.*
  enum Stmt:
    case VarDef(name: String, value: Expr)
    case Block(list: List[Stmt])

    val id: Int = nextId
    nextId += 1

    override def toString: String = this match
      case VarDef(name, value) => s"$name = $value"
      case Block(ss) => ss.mkString("; ")

    override def equals(obj: Any): Boolean = obj match
      case that: Stmt => this.id == that.id
      case _ => false
    override def hashCode(): Int = id

    def expressions: Seq[Expr] =
      this match
        case VarDef(_, e) => e.expressions
        case Block(stmts) => stmts.flatMap(_.expressions)

    def statements: Seq[Stmt] =
      this match
        case v: VarDef => Seq(v)
        case b@Block(stmts) => Seq(b) ++ stmts.flatMap(_.statements)

  def firstNonBlock(s: Stmt): Option[Stmt] = s match
    case Block(ss) => ss.headOption.flatMap(firstNonBlock)
    case _ => Some(s)

  def lastNonBlock(s: Stmt): Option[Stmt] = s match
    case Block(ss) => ss.lastOption.flatMap(lastNonBlock)
    case _ => Some(s)

  def exprToEdbRelations(s: Stmt): Seq[Relation] =
    val nums = ListBuffer.empty[Expr.Num]
    val vars = ListBuffer.empty[Expr.Var]
    val adds = ListBuffer.empty[Expr.Add]

    s.expressions.foreach {
      case n: Num => nums += n
      case v: Var => vars += v
      case a: Add => adds += a
    }

    // edb num(Any, Int).
    val edbNum = Relation2("num", Seq("e", "v"), nums.toSeq.map(e => Seq(e, e.value)))
    // edb var(Any, String).
    val edbVar = Relation2("var", Seq("e", "n"), vars.toSeq.map(e => Seq(e, e.name)))
    // edb add(Any, Any, Any).
    val edbAdds = Relation3("add", Seq("e", "l", "r"), adds.toSeq.map(e => Seq(e, e.lhs, e.rhs)))

    Seq(edbNum, edbVar, edbAdds)

  def stmtToEdbRelations(s: Stmt): Seq[Relation] =
    val vardef = ListBuffer.empty[Stmt.VarDef]
    val blocks = ListBuffer.empty[Stmt.Block]

    s.statements.foreach {
      case v: VarDef => vardef += v
      case b: Block => blocks += b
    }

    // edb VarDef(Any, String, Any).
    val edbVarDef = Relation3("vardef", Seq("s", "n", "e"), vardef.toSeq.map(s => Seq(s, s.name, s.value)))

    //edb next(Any, Any).
    val edbNext = Relation2("next", Seq("s1", "s2"), blocks.flatMap { block =>
      block.list.zip(block.list.tail).flatMap((pred, succ) =>
        for (from <- lastNonBlock(pred); to <- firstNonBlock(succ)) yield
          Seq(from, to))
    })

    Seq(edbVarDef, edbNext)


  test("constant seq") {
    val code = FileUtil.readFileFromResource("datalog/casestudy/constantAnalysis.dl")
    val compiled = exec.compileDatalog(code, options)
    compiled.setPipeline(pipeline)
    val loaded = exec.loadDatalog(compiled)

    val s = Block(List(
      VarDef("a", Num(4)),
      VarDef("b", Num(2)),
      VarDef("c", Add(Var("a"), Var("b")))
    ))

    val edbs = exprToEdbRelations(s) ++ stmtToEdbRelations(s)
//    edbs.foreach(t => println(t.asTable))
    edbs.foreach(loaded.engine.insert)
    edbs.foreach(e => println(e.asTable))

    val cvalue = loaded.query("cvalue")
    println(cvalue.asTable)
  }

  test("constant seq - reassign") {
    val code = FileUtil.readFileFromResource("datalog/casestudy/constantAnalysis2.dl")
    val compiled = exec.compileDatalog(code, options)
    compiled.setPipeline(pipeline)
    val loaded = exec.loadDatalog(compiled)

    val stmts = List(
      VarDef("a", Num(4)),
      VarDef("b", Num(2)),
      VarDef("d", Add(Num(1), Num(2))),
      VarDef("a", Add(Var("a"), Var("b")))
    )
    val s = Block(stmts)

    val first = stmts.head
    val firstEDBRelation = Relation1("first", Seq("s"), Seq(Seq(first)))

    val edbs = firstEDBRelation +: (exprToEdbRelations(s) ++ stmtToEdbRelations(s))
    //    edbs.foreach(t => println(t.asTable))
    edbs.foreach(loaded.engine.insert)
    edbs.foreach(e => println(e.asTable))

    val cvalue = loaded.query("cvalue")
    println(cvalue.asTable)
  }
