package inca.util

case class CompilerOptionSection(name: String, options: Map[String, Any]):
  private def readValue(option: String): Any =
    options.get(option) match
      case Some(op) => op
      case _ => throw IllegalAccessException(s"No option named: $option in section $name")

  def readBoolean(option: String): Boolean =
    readValue(option) match
      case b: Boolean => b
      case v => throw IllegalAccessException(s"Expected boolean but found: $v")


case class CompilerOptions(defaults: (String, Seq[(String, Any)])*):
  private var options: Map[String, CompilerOptionSection] = defaults
    .map((s, o) => s -> CompilerOptionSection(s, o.toMap))
    .toMap

  def apply(section: String): CompilerOptionSection =
    options.get(section) match
      case Some(sec) => sec
      case _ => throw IllegalAccessException(s"No section named: $section")


object Implicits:
  implicit val compilerOptions: CompilerOptions = CompilerOptions(
      "ir" -> Seq(
        // Lowerings
        "print_lowerings" -> false,
        "print_typed_lowerings" -> true,
        // Optimizations
        "print_optimizations" -> true,
        // Stats
        "print_stats_before_lowering" -> false,
        "print_stats_before_optimization" -> false,
        "print_stats_after_optimization" -> false
      ),
      "fun" -> Seq(),
      "oodl" -> Seq()
    )