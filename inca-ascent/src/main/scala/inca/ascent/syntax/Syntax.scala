package inca.ascent.syntax

// Rust wrapper around f32 to support eq and hash
val f32Wrapper =
  """
    |#[derive(Debug, Clone, Copy, Serialize)]
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
    |impl FromStr for Float {
    |    type Err = std::num::ParseFloatError;
    |    fn from_str(s: &str) -> Result<Self, Self::Err> {
    |        let parsed = s.parse::<f32>()?;
    |        Ok(Float(parsed))
    |    }
    |}
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

def read_edb_template(size: Int): String =
  val tyArgs = (1 until size+1).map(i => s"T$i").mkString(", ") + ","
  val tyParamS = (1 until size+1).map(i => s"T$i: FromStr").mkString(", ")
  val tyArgConstraintS = (1 until size+1).map(i => s"    <T$i as FromStr>::Err: std::fmt::Debug").mkString(",\n")
  val tyFieldS = (1 until size+1).map {
    i => s"            let field$i = record[${i-1}].parse::<T$i>().expect(&format_error_msg!($i));"
  }.mkString("\n")
  val tyFillVec = (1 until size+1).map(i => s"field$i").mkString(", ")  + ","
  s"""
     |fn parse_tsv_$size<$tyParamS>(file_path: &str) -> Vec<($tyArgs)>
     |where
     |$tyArgConstraintS
     |{
     |    let mut records: Vec<($tyArgs)> = Vec::new();
     |    let file = File::open(file_path).expect("Failed to open file");
     |    let mut reader = csv::ReaderBuilder::new().has_headers(false).delimiter(b'\\t').from_reader(file);
     |
     |    for result in reader.records() {
     |        let record = result.expect("Failed to read record");
     |        if record.len() == $size {
     |$tyFieldS
     |            records.push(($tyFillVec));
     |        } else {
     |            panic!("Invalid number of fields in record: {:?}", record);
     |        }
     |    }
     |    records
     |}
     |
     |""".stripMargin


case class Program(content: Seq[ProgramContent], outputRels: Seq[ProgramContent.RelDecl]):
  override def toString: String =
    val (enums, ascentContentWithEdb) = content.partition {
      case _: ProgramContent.CustomType => true
      case _ => false
    }

    val edbRelDecls = content.collect { case decl: ProgramContent.RelDecl if decl.isEdb => decl }
    val edbReadHelper = edbRelDecls.map(_.arg.size).distinct.map(read_edb_template).mkString("\n")

    val (edbFilesContent, ascentContent) = ascentContentWithEdb.partition {
      case ProgramContent.EDBFile(name, path) => true
      case _ => false
    }
    val edbFiles = edbFilesContent.map {
      case ProgramContent.EDBFile(name, path) => name -> path
    }.toMap

    val fillEdbs = edbRelDecls.flatMap { decl =>
      val name = decl.name
      val size = decl.arg.size
      edbFiles.get(name) match
        case Some(file) => Some(s"""  prog.$name = parse_tsv_$size::<${decl.arg.mkString(", ")}>("$file");""")
        case None => None
    }.mkString("\n")

    val out = outputRels.map { decl =>
      val name = decl.name
      s"""  println!("{{\\"name\\": \\"$name\\", \\"size\\": ${decl.arg.size}, \\"elements\\": {}}}\", serde_json::to_string(&prog.$name).unwrap());"""
    }.mkString("\n")

    val main = s"""
       |fn main() {
       |  let mut prog = AscentProgram::default();
       |$fillEdbs
       |  prog.run();
       |
       |$out
       |}
       |""".stripMargin

    s"""
      |#![allow(warnings)] // suppress all warnings
      |
      |use ascent::ascent;
      |use ascent::aggregators::{max,min,sum,count};
      |use std::hash::{Hash,Hasher};
      |use std::ops;
      |use std::cmp::Ordering;
      |use std::fs::File;
      |use std::str::FromStr;
      |use serde::Serialize;
      |
      |macro_rules! format_error_msg {
      |    ($$($$args:expr),*) => {
      |        format!("Failed to parse field {} into expected type", $$($$args),*)
      |    };
      |}
      |
      |$edbReadHelper
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
  case EDBFile(name: String, path: String)
  case RelDecl(name: String, arg: Seq[FormatType], isEdb: Boolean = false)
  case Rule(name: String, param: Seq[(Term, FormatType)], body: Seq[Atom])
  case Fact(name: String, param: Seq[Term])
  case CustomType(enumName: String, cases: Seq[(String, Seq[FormatType])])

  override def toString: String = this match {
    case RelDecl(name, arg, _) =>
      s"relation $name(${arg.mkString(", ")});"
    case Rule(name, param, body) =>
      val params = param.map {
        case (p, FormatType.Custom(x)) =>
          if (body.exists {
            case Atom.Aggregator(atomp, Aggregation.Count(), _) => Term.Var(atomp) == p
            case Atom.Aggregator(atomp, Aggregation.Mean(_), _) => Term.Var(atomp) == p
            case _ => false
          }) {
            s"$p as ${FormatType.Custom(x)}"
          } else {
            s"$p"
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
        case p@Term.StringLit(_) => s"$p.into()"
        case p => p.toString()
      }
      s"$name(${params.mkString(", ")});"

    case CustomType(dataName, cases) =>
      // construct one destruct function for each case of this data type
      val paramS = cases.map {
        case (caseName, caseTypes) =>
          val caseType = caseTypes.map {
            case p@FormatType.Custom(`dataName`) => s"Box<$p>"
            case p => p.toString
          }
          s"$caseName(${caseType.mkString(",")})"
      }.mkString("  ", ",\n", "")

      val enumDestructors = cases.map {
        case (name, paramTys) =>
          val caseNames = cases.map {
            case (`name`, pty) =>
              val size = pty.size
              val paramVars = (0 until size).map(idx => s"param_${idx}")
              val destrArgs = paramVars.zip(paramTys).map {
                case (p, FormatType.Custom(`dataName`)) => s"*$p"
                case (p, FormatType.Symbol) => s"$p.to_string()"
                case (p, _) => p
              }
              val argS = if (destrArgs.length == 1) destrArgs.mkString(", ") else destrArgs.mkString("(", ", ", ")")
              s"$dataName::$name(${paramVars.mkString(", ")}) => Some($argS)"
            case (pname, pty) =>
              val size2 = pty.size
              val wildcardArgs = (0 until size2).map(idx => "_").mkString(", ")
              s"$dataName::$pname($wildcardArgs) => None"
          }.mkString("    ", ",\n    ", "")

          val paramS = if (paramTys.length == 1) paramTys.mkString(", ") else paramTys.mkString("(", ", ", ")")
          s"""
             |fn destruct_$name(obj: $dataName) -> Option<$paramS>{
             |  return match obj {
             |$caseNames
             |  };
             |}""".stripMargin
      }.mkString("\n")

      s"""
         |#[derive(Debug, Eq, PartialEq, Clone, Hash, Serialize)]
         |pub enum $dataName{
         |$paramS
         |}
         |
         |$enumDestructors
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
  case Binary(t1: Term, op: BinOp, t2: Term)
  case Var(name: String)
  case DeRef(t: Term)
  case Clone(t: Term)
  case Unary(op: Unop, t: Term)
  case StringLit(s: String)
  case NumberLit(n: Int)
  case FloatLit(d: Float)
  case Wildcard
  case TypeCast(t: Term, ty: FormatType)
  case CustomLit(dataName: String, caseName: String, param: Seq[Term])
  case Concat(t: Seq[Term])
  case ToString(t: Term)
  case Box(t: Term)

  override def toString: String = this match {
    case Binary(t1, op, t2) => s"$t1 $op $t2"
    case Unary(op, t) => s"$op$t"
    case Var(name) => name
    case DeRef(t) => s"*$t"
    case Clone(t) => s"$t.clone()"
    case Box(t) => s"Box::new($t)"
    case CustomLit(dataName, caseName, params) => s"$dataName::$caseName(${params.mkString(",")})"
    case Wildcard => "_"
    case NumberLit(n) => n.toString
    case FloatLit(d) => s"Float($d)"
    case StringLit(s) => s"""<&str as Into<String>>::into(r###"$s"###)"""
    case ToString(t) => s"$t.to_string()"
    case TypeCast(t, ty) => s"$t as $ty"
    case Concat(t) =>
      val s = "{}".repeat(t.size)
      s"format!(\"$s\",${t.mkString(",")})"
  }

enum Atom:
  case Call(name: String, param: Seq[Term])
  case Let(v: Term, expr: Term)
  case Aggregator(name: String, agg: Aggregation, dom: Atom)
  case Not(atom: Atom)
  case Deconstruct(t: Term, c: String, tmp: String, arg: Seq[Term], neg: Boolean)

  case Equal(t1: Term, t2: Term)
  case NotEqual(t1: Term, t2: Term)
  case GreaterThan(t1: Term, t2: Term)
  case LesserThan(t1: Term, t2: Term)
  case GreaterThanEqual(t1: Term, t2: Term)
  case LesserThanEqual(t1: Term, t2: Term)

  override def toString: String = this match {
    case Call(name, param) => s"$name(${param.mkString(", ")})"
    case Let(v, expr) => s"let $v = $expr"
    case Aggregator(name, agg, dom) => s"agg $name = $agg in $dom"
    case Not(atom) => s"!$atom"
    case Deconstruct(t, c, tmp, args, neg) =>
      val cond = if (neg) s"if $tmp.is_none()" else s"if !$tmp.is_none()"
      val unpack = if (args.length == 1) args.mkString(", ") else args.mkString("(", ", ", ")")
      s"let $tmp = destruct_$c($t), $cond, let $unpack=$tmp.unwrap()"
    case Equal(t1, t2) => s"if $t1 == $t2"
    case NotEqual(t1, t2) => s"if $t1 != $t2"
    case GreaterThan(t1, t2) => s"if $t1 > $t2"
    case LesserThan(t1, t2) => s"if $t1 < $t2"
    case GreaterThanEqual(t1, t2) => s"if $t1 >= $t2"
    case LesserThanEqual(t1, t2) => s"if $t1 <= $t2"
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
    case Custom(name) => name
  }