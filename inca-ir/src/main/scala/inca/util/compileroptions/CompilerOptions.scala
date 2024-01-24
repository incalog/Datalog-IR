package inca.util.compileroptions

import inca.util.FileUtil

class Section(val name: String, defaults: Map[String, Any]):
  private var options: Map[String, Any] = defaults

  override def toString: String =
    val sec = s"[$name]\n"
    val entries = options.map { case (k, v) => s"$k = $v" }.mkString("\n")
    s"$sec$entries"

  private def readValue[T](option: String): Option[T] = options.get(option).asInstanceOf[Option[T]]
  def readBoolean(option: String): Boolean = readValue(option).getOrElse(false)
  def readString(option: String): String = readValue(option).getOrElse("")
  def readInt(option: String): Int = readValue(option).getOrElse(0)
  def readDouble(option: String): Double = readValue(option).getOrElse(0.0)
  def update(name: String, value: Any): Unit = options += name -> value


case class IRLoggingSection(override val name: String, defaults: Map[String, Any]) extends Section(name, defaults):
  // Log type information when logging a module
  def logTypeInformation: Boolean = readBoolean("typed")
  def logTypeInformation_=(newVal: Boolean): Unit = update("typed", newVal)

  // Log the module before any lowering
  def logModule: Boolean = readBoolean("module")
  def logModule_=(newVal: Boolean): Unit = update("module", newVal)

  // Log all lowering steps
  def logLowerings: Boolean = readBoolean("lowerings")
  def logLowerings_=(newVal: Boolean): Unit = update("lowerings", newVal)

  // Log all optimization steps
  def logOptimizations: Boolean = readBoolean("optimizations")
  def logOptimizations_=(newVal: Boolean): Unit = update("optimizations", newVal)

  // Log all stats before the lowering are applied
  def logStatsBeforeLowering: Boolean = readBoolean("stats_before_lowering")
  def logStatsBeforeLowering_=(newVal: Boolean): Unit = update("stats_before_lowering", newVal)

  // Log all stats before the optimizations are applied
  def logStatsBeforeOptimizations: Boolean = readBoolean("stats_before_optimization")
  def logStatsBeforeOptimizations_=(newVal: Boolean): Unit = update("stats_before_optimization", newVal)

  // Log all stats after the optimizations are applied
  def logStatsAfterOptimizations: Boolean = readBoolean("stats_after_optimization")
  def logStatsAfterOptimizations_=(newVal: Boolean): Unit = update("stats_after_optimization", newVal)


class CompilerOptions protected(defaults: Seq[(String, Seq[(String, Any)])]):

  protected var options: Map[String, Section] = defaults
    .map((s, o) => s -> createSection(s, o.toMap))
    .toMap

  override def toString: String = options.values.mkString("\n\n")

  protected def createSection(name: String, entries: Map[String, Any]): Section = name match
    case "ir_logging" => IRLoggingSection("ir_logging", entries)
    case _ => Section(name, entries)

  // All available properties
  def irLogging: IRLoggingSection =
     apply("ir_logging") match
       case sec: IRLoggingSection => sec
       case _ => throw IllegalStateException("Expected IRLoggingSection, but got Section")

  protected def setDefaults(): Unit =
    irLogging.logTypeInformation = false
    irLogging.logModule = false
    irLogging.logLowerings = false
    irLogging.logOptimizations = false
    irLogging.logStatsBeforeLowering = false
    irLogging.logStatsBeforeOptimizations = false
    irLogging.logStatsAfterOptimizations = false

  def update(section: Section): Unit = options += section.name -> section

  def apply(section: String): Section =
    options.get(section) match
      case Some(sec) => sec
      case _ =>
        val sec = createSection(section, Map())
        options += section -> sec
        sec


object CompilerOptions:
  def fromResource(path: String): CompilerOptions =
    val content = FileUtil.readFileFromResource(path)
    val parsedOptions = IniParser.parse(content)
    val compilerOptions = CompilerOptions(parsedOptions)
    compilerOptions

  implicit def default: CompilerOptions =
    val options = CompilerOptions(Seq())
    options.setDefaults()
    options