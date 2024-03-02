package inca.ascent.syntax

import inca.ascent.syntax.Term.{StringLit, Var}
import inca.ascent.syntax.Aggregation.Count


// Rust wrapper around f32 to support eq and hash
val f32Wrapper =
  """
    |#[derive(Debug, Clone, Copy)]
    |pub struct Float(f32);
    |impl Hash for Float {
    |    fn hash<H: Hasher>(&self, state: &mut H) {
    |        let mut buf = [0; 4];
    |        let transmuted: u32 = unsafe { std::mem::transmute(self.0) };
    |        buf.copy_from_slice(&transmuted.to_le_bytes());
    |        buf.hash(state)
    |    }
    |}
    |impl PartialEq for Float {
    |    fn eq(&self, other: &Float) -> bool {
    |        self.0 == other.0
    |    }
    |}
    |impl Eq for Float {}
    |impl ops::Add for Float {
    |    type Output = Float;
    |    fn add(self, other: Float) -> Float {
    |        Float(self.0 + other.0)
    |    }
    |}
    |impl ops::Sub for Float {
    |    type Output = Float;
    |    fn sub(self, other: Float) -> Float {
    |        Float(self.0 - other.0)
    |    }
    |}
    |impl ops::Mul for Float {
    |    type Output = Float;
    |    fn mul(self, other: Float) -> Float {
    |        Float(self.0 * other.0)
    |    }
    |}
    |impl ops::Div for Float {
    |    type Output = Float;
    |    fn div(self, other: Float) -> Float {
    |        Float(self.0 / other.0)
    |    }
    |}
    |impl ops::Rem for Float {
    |    type Output = Float;
    |    fn rem(self, other: Float) -> Float {
    |        Float(self.0 % other.0)
    |    }
    |}
    |impl Ord for Float {
    |    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
    |        self.0
    |            .partial_cmp(&other.0)
    |            .unwrap_or(std::cmp::Ordering::Equal)
    |    }
    |}
    |impl PartialOrd for Float {
    |    fn partial_cmp(&self, other: &Float) -> Option<std::cmp::Ordering> {
    |        Some(self.cmp(&other))
    |    }
    |}
    |impl std::ops::AddAssign for Float {
    |    fn add_assign(&mut self, other: Self) {
    |        self.0 += other.0;
    |    }
    |}
    |impl std::iter::Sum for Float {
    |    fn sum<I: Iterator<Item = Self>>(iter: I) -> Self {
    |        let mut sum = Float(0.0);
    |        for item in iter {
    |            sum += item;
    |        }
    |        sum
    |    }
    |}
    |""".stripMargin


case class Program(content: Seq[ProgramContent], outputRels: Seq[ProgramContent.RelDecl]):
  override def toString: String =
    val (enums, ascentContent) = content.partition {
      case _: ProgramContent.CustomType => true
      case _ => false
    }

    val out = outputRels.map { decl =>
      val name = decl.name
      s"  println!(\"$name: {:?}\", prog.$name);"
    }.mkString("\n")
    val main = s"""
       |fn main() {
       |  let mut prog = AscentProgram::default();
       |  prog.run();
       |
       |$out
       |}
       |""".stripMargin

    s"""
      |use ascent::ascent;
      |use ascent::aggregators::{max,min,sum,count};
      |use std::hash::{Hash,Hasher};
      |use std::ops;
      |use std::cmp::Ordering;
      |
      |$f32Wrapper
      |
      |${enums.mkString("\n")}
      |
      |ascent!{
      |${ascentContent.mkString("\n")}
      |}
      |
      |$main
      |""".stripMargin


enum ProgramContent:
  case RelDecl(name: String, arg: Seq[FormatType])
  case Rule(name: String, param: Seq[(Term, FormatType)], body: Seq[Atom])
  case Fact(name: String, param: Seq[Term])
  case CustomType(enumName: String, cases: Seq[(String, Seq[FormatType])])


  override def toString: String = this match {
    case RelDecl(name, arg) =>
      s"relation $name(${arg.mkString(", ")});"
    case Rule(name, param, body) =>
      val params = param.map {
        case (p, FormatType.Custom(x)) =>
          if (body.exists {
            case Atom.Aggregator(atomp, Aggregation.Count(), _) => Term.Var(atomp) == p
            case Atom.Aggregator(atomp, Aggregation.Mean(_), _) => Term.Var(atomp) == p
            case _ => false
          }) {
            s"$p.clone() as ${FormatType.Custom(x)}"
          } else {
            s"$p.clone()"
          }
        case (p, ty) =>
          if (body.exists {
            case Atom.Aggregator(atomp, Aggregation.Count(), _) => Term.Var(atomp) == p
            case Atom.Aggregator(atomp, Aggregation.Mean(_), _) => Term.Var(atomp) == p
            case _ => false
          }) {
            s"$p as $ty "
          } else {
            p.toString
          }
      }
      s"$name(${params.mkString(", ")}) <-- ${body.mkString(", ")};"
    case Fact(name, param) =>
      val params = param.map {
        case p => if (p == Term.StringLit) s"$p.into()" else p.toString()
      }
      s"$name(${params.mkString(", ")});"
    case CustomType(dataName, cases) =>
      val param = cases.map {
        case (casename, casetypes) =>
          val casetype = casetypes.map { p =>
            if (p == FormatType.Custom(dataName))
              s"Box<$p>"
            else
              p.toString
          }

          s"$casename(${casetype.mkString(",")})"
      }
      val enumDestructors = cases.map {
        case (name, paramTys) =>
          val casesn = cases.map {
            case (pname, pty) =>
              if (pname == name) {
                val size = pty.size
                val paramVars = (0 until size).map(idx => s"param_${idx}")
                val paramVars2 = paramVars.zip(paramTys).map {
                  case (p, FormatType.Symbol) => s"$p.to_string()"
                  case (p, FormatType.Custom(cname)) if (cname == dataName) => s"*$p"
                  case (p, FormatType.Custom(cname))  => s"$p"
                  case (p, _) => p
                }
                if (paramVars2.length == 1) {
                  s"$dataName::$pname(${paramVars.mkString(", ")})=>Some(${paramVars2.mkString(", ")})"
                } else {
                  s"$dataName::$pname(${paramVars.mkString(", ")})=>Some((${paramVars2.mkString(", ")}))"
                }
              } else {
                val size2 = pty.size
                val paramvars3 = (0 until size2).map(idx => s"_")

                s"$dataName::$pname(${paramvars3.mkString(", ")})=> None"
              }
          }
          if (paramTys.length == 1) {
            s"fn destruct_$name(obj: $dataName) ->Option<${paramTys.mkString(", ")}>{\n return match obj{\n${casesn.mkString(s",\n")}\n};\n}"
          } else {
            s"fn destruct_$name(obj: $dataName) ->Option<(${paramTys.mkString(", ")})>{\n return match obj{\n${casesn.mkString(s",\n")}\n};\n}"
          }
      }
      s"""
         |#[derive(Debug, Eq, PartialEq, Clone, Hash)]
         |pub enum $dataName{
         |${param.mkString("  ", ",\n", "")}
         |}
         |
         |${enumDestructors.mkString("\n")}
         |""".stripMargin
  }


enum Aggregation:
  case Min(t: Term)
  case Sum(t: Term)
  case Max(t: Term)
  case Mean(t: Term)
  case Count()
  case Percentile(p: Term, t: Term)

  override def toString: String = this match {
    case Min(t) => s"min($t)"
    case Sum(t) => s"sum($t)"
    case Max(t) => s"max($t)"
    case Mean(t) => s"mean($t)"
    case Count() => s"count()"
    case Percentile(p, t) => s"(percentile($p))($t)"
  }


enum Term:
  case Binary(t1: Term, op: BinOp, t2: Term, calval: Seq[Term.Var])
  case Var(name: String)
  case Tuple(names: Seq[Term])
  case Unary(op: Unop, t: Term)
  case StringLit(s: String)
  case NumberLit(n: Int)
  case FloatLit(d: Float)
  case BooleanLit(b: Condition)
  case Wildcard
  case Ref(v: Term)
  //case Range(start: Int, end: Int)
  //case ListLit(s: Seq[Term])
  case TypeCast(t: Term, ty: FormatType)
  case CustomLit(dataName: String, caseName: String, paramEnum: Seq[FormatType], param: Seq[Term], callVal: Seq[Term.Var])
  case Concat(t: Seq[Term])
  case ToString(t: Term)

  override def toString: String = this match {
    case Binary(t1, op, t2, callval) => (t1, t2) match {
      case (Term.Var(x), Term.Var(y)) =>
        if (callval.contains(t1) && callval.contains(t2)) {
          s"*$t1 $op *$t2"
        } else if (callval.contains(t1)) {
          s"*$t1 $op $t2"
        } else if (callval.contains(t2)) {
          s"$t1 $op *$t2"
        } else {
          s"$t1 $op $t2"
        }

      case (Term.Var(x), _) =>
        if (callval.contains(t1)) {
          s"*$t1 $op $t2"
        } else {
          s"$t1 $op $t2"
        }
      case (_, Term.Var(y)) =>
        if (callval.contains(t2)) {
          s"$t1 $op *$t2"
        } else {
          s"$t1 $op $t2"
        }
      case (_) => s"$t1 $op $t2"
    }
    /*case CompareOp(t) => t.toString*/
    case Unary(op, t) => s"$op$t"
    case Var(name) => name
    case Tuple(names) => if (names.length == 1) {
      s"${names.mkString(", ")}"
    } else {
      s"(${names.mkString(", ")})"
    }
    case CustomLit(dataName, caseName, paramEnum, param, callval) =>
      val enumTypes = paramEnum
      val params = param.zip(enumTypes).map {
        case (t@Term.CustomLit(dataName2, caseName2, pty, pTerm, callval), ty) =>
          if (dataName2 == dataName) {
            s"Box::new(${t})"
          } else if (ty == FormatType.Custom(dataName2)) {
            t.toString
          } else {
            throw IllegalArgumentException("Not the right parametertype")
          }
        case (Term.Var(x), pty) => pty match {
          case FormatType.Custom(dataName2) =>
            if (dataName2 == dataName) {
              s"Box::new(${Term.Var(x)}.clone())"
            } else {
              s"${Term.Var(x)}.clone()"
            }
          case FormatType.Symbol =>
            if (callval.contains(Term.Var(x))) {
              s"*${Term.Var(x)}.to_string()"
            } else {
              s"${Term.Var(x)}.to_string()"
            }
          case _ =>
            if (callval.contains(Term.Var(x))) {
              s"*${Term.Var(x)}"
            } else {
              s"${Term.Var(x)}"
            }
        }
        case (p, _) => p.toString
      }
      s"$dataName::$caseName(${params.mkString(",")})"
    case NumberLit(n) => n.toString
    case FloatLit(d) => s"Float($d)"
    case BooleanLit(b) => b.toString
    case Wildcard => "_"
    case StringLit(s) => s"""<&str as Into<String>>::into("$s")"""
    case ToString(t) => s"$t.to_string()"
    case Ref(v) => s"&$v"
    //case Range(start, end) => start.toString + ".." + end.toString
    //case ListLit(s) => s"vec![${s.mkString(", ")}]"
    case TypeCast(t, ty) => s"$t as $ty"
    case Concat(t) =>
      val s = "{}".repeat(t.size)
      s"format!(\"$s\",${t.mkString(",")})"
  }


enum Atom:
  case Call(name: String, param: Seq[Term])
  case Let(v: (Term, FormatType), expr: Term)
  case For(v: Term.Var, expr: Term)
  case ConditionalClause(cond: Condition)
  case Aggregator(name: String, agg: Aggregation, dom: Atom)
  case Not(atom: Atom)
  case Deconstruct(t: Term, c: String, tmp: String, arg: Seq[Term], neg: Boolean)


  override def toString: String = this match {
    case Call(name, param) => s"$name(${param.mkString(", ")})"
    case Let(v, expr) =>
      val expr1 = expr match
        case Var(n) => v._2 match
          case FormatType.Custom(x) => s"${Term.Var(n)}.clone()"
          case p => expr.toString
        case StringLit(s) => s"\"$s\""
        case p => p.toString
      s"let ${v._1} = $expr1"
    case For(v, expr) => s"for $v in $expr"
    case ConditionalClause(cond) => s"if $cond"
    case Aggregator(name, agg, dom) =>
      s"agg $name =$agg in $dom"
    case Not(atom) => s"!$atom"
    case Deconstruct(t, c, tmp, arg, neg) =>
      val cond = if (neg) {
        s"if $tmp.is_none()"
      } else {
        s"if !$tmp.is_none()"
      }
      if (arg.length == 1) {
        s"let $tmp = destruct_$c($t.clone()), $cond, let ${arg.mkString(",")}=$tmp.unwrap()"
      } else {
        s"let $tmp = destruct_$c($t.clone()), $cond, let (${arg.mkString(",")})=$tmp.unwrap()"
      }

  }

enum Condition:
  case Equal(t1: Term, t2: Term, t_agg: Boolean = false)
  case NotEqual(t1: Term, t2: Term)
  case GreaterThan(t1: Term, t2: Term)
  case LesserThan(t1: Term, t2: Term)
  case GreaterTHanEqual(t1: Term, t2: Term)
  case LesserThanEqual(t1: Term, t2: Term)
  case Not(t: Term)
  case Tru
  case Fls

  override def toString: String = this match {
    case Equal(t1, t2, t_agg) =>
      (t1, t2, t_agg) match {
        case (Term.Var(_), Term.Var(_), _) => s"*$t1 == *$t2"
        case (Term.Var(_), _, false) => s"*$t1 == $t2"
        case (Term.Var(_), _, true) => s"$t1 == $t2"
        case (_, Term.Var(_), false) => s"$t1 == *$t2"
        case (_, Term.Var(_), true) => s"$t1 == $t2"
        case (_, _, _) => s"$t1 == $t2"
      }
    case NotEqual(t1, t2) =>
      (t1, t2) match {
        case (Term.Var(_), Term.Var(_)) => s"*$t1 != *$t2"
        case (Term.Var(_), _) => s"*$t1 != $t2"
        case (_, Term.Var(_)) => s"$t1 != *$t2"
        case (_, _) => s"$t1 != $t2"
      }
    case GreaterThan(t1, t2) =>
      (t1, t2) match {
        case (Term.Var(_), Term.Var(_)) => s"*$t1 > *$t2"
        case (Term.Var(_), _) => s"*$t1 > $t2"
        case (_, Term.Var(_)) => s"$t1 > *$t2"
        case (_, _) => s"$t1 > $t2"
      }
    case LesserThan(t1, t2) =>
      (t1, t2) match {
        case (Term.Var(_), Term.Var(_)) => s"*$t1 < *$t2"
        case (Term.Var(_), _) => s"*$t1 < $t2"
        case (_, Term.Var(_)) => s"$t1 < *$t2"
        case (_, _) => s"$t1 < $t2"
      }

    case GreaterTHanEqual(t1, t2) =>
      (t1, t2) match {
        case (Term.Var(_), Term.Var(_)) => s"*$t1 >= *$t2"
        case (Term.Var(_), _) => s"*$t1 >= $t2"
        case (_, Term.Var(_)) => s"$t1 >= *$t2"
        case (_, _) => s"$t1 >= $t2"
      }
    case LesserThanEqual(t1, t2) =>
      (t1, t2) match {
        case (Term.Var(_), Term.Var(_)) => s"*$t1 <= *$t2"
        case (Term.Var(_), _) => s"*$t1 <= $t2"
        case (_, Term.Var(_)) => s"$t1 <= *$t2"
        case (_, _) => s"$t1 <= $t2"
      }
    case Not(t) => s"!$t"
    case Tru => "true"
    case Fls => "false"

  }

enum BinOp:
  case Add
  case Sub
  case Mul
  case Div
  case Rem
  case LAnd
  case LOr

  override def toString: String = this match {
    case Add => "+"
    case Sub => "-"
    case Mul => "*"
    case Div => "/"
    case Rem => "%"
    case LAnd => "&&"
    case LOr => "||"
  }

enum Unop:
  case Lnot
  case neg

  override def toString: String = this match {
    case Lnot => "!"
    case neg => "-"
  }

enum FormatType:
  case Number
  case Float
  case Symbol
  case Unsigned
  case Custom(name: String)

  override def toString: String = this match {
    case Number => "i32"
    case Float => "Float"
    case Symbol => "String"
    case Unsigned => "u32"
    case Custom(name) => s"$name"
  }

