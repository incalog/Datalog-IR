package inca.util.compileroptions

import inca.util.FileUtil


trait CompilerOptionSection:
  def name: String
  def readBoolean(option: String): Boolean = false
  def readString(option: String): String = ""
  def readInt(option: String): Int = 0
  def readDouble(option: String): Double = 0.0

  override def toString: String = s"[$name]"

// Just use the default value for whatever option is read
case class DefaultCompilerOptionSection(name: String)  extends CompilerOptionSection


case class ConcreteCompilerOptionSection(name: String, defaults: Map[String, Any]) extends CompilerOptionSection:
  private var options: Map[String, Any] = defaults

  override def toString: String =
    val sec = s"[$name]\n"
    val entries = options.map { case (k, v) => s"$k = $v" }.mkString("\n")
    s"$sec$entries"

  private def readValue(option: String): Any =
    options.get(option) match
      case Some(op) => op
      case _ => throw IllegalAccessException(s"No option named: $option in section $name")

  override def readBoolean(option: String): Boolean =
    readValue(option) match
      case b: Boolean => b
      case v => throw IllegalAccessException(s"Expected boolean but found: $v")

  override def readString(option: String): String =
    readValue(option) match
      case s: String => s
      case v => throw IllegalAccessException(s"Expected string but found: $v")

  override def readInt(option: String): Int =
    readValue(option) match
      case i: Int => i
      case v => throw IllegalAccessException(s"Expected int but found: $v")

  override def readDouble(option: String): Double =
    readValue(option) match
      case d: Double => d
      case v => throw IllegalAccessException(s"Expected int but found: $v")

  def update(name: String, value: Any): Unit =
    options += name -> value

case class CompilerOptions(defaults: Seq[(String, Seq[(String, Any)])]):
  private var options: Map[String, CompilerOptionSection] = defaults
    .map((s, o) => s -> ConcreteCompilerOptionSection(s, o.toMap))
    .toMap

  override def toString: String =
    options.values.mkString("\n")

  def update(section: CompilerOptionSection): Unit =
    options += section.name -> section

  def apply(section: String): CompilerOptionSection =
    options.get(section) match
      case Some(sec) => sec
      case _ => DefaultCompilerOptionSection(section)


object CompilerOptions:
  def fromResource(path: String): CompilerOptions =
    val content = FileUtil.readFileFromResource(path)
    val parsedOptions = IniParser.parse(content)
    val compilerOptions = CompilerOptions(parsedOptions)
    compilerOptions

  implicit val default: CompilerOptions = CompilerOptions(Seq(
      "ir_logging" -> Seq(
        "typed" -> true,
        // Lowerings
        "lowerings" -> true,
        // Optimizations
        "optimizations" -> true,
        // Stats
        "stats_before_lowering" -> false,
        "stats_before_optimization" -> false,
        "stats_after_optimization" -> false
      ),
    ))