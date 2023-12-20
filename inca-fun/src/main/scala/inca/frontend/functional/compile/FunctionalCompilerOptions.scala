package inca.frontend.functional.compile

import inca.util.FileUtil
import inca.util.compileroptions.{CompilerOptions, IniParser, Section}

case class FunctionalLoggingSection(override val name: String, defaults: Map[String, Any]) extends Section(name, defaults):
  // Log type information when logging a module
  def logTypeInformation: Boolean = readBoolean("typed")
  def logTypeInformation_=(newVal: Boolean): Unit = update("typed", newVal)
  // Log the main module before transformation to IR code
  def logModule: Boolean = readBoolean("module")
  def logModule_=(newVal: Boolean): Unit = update("module", newVal)
  // Log the normalized module
  def logNormalizedModule: Boolean = readBoolean("normalized")
  def logNormalizedModule_=(newVal: Boolean): Unit = update("normalized", newVal)
  // Log all relations
  def verboseOutput: Boolean = readBoolean("verbose_output")
  def verboseOutput_= (newVal: Boolean): Unit = update("verbose_output", newVal)


final class FunctionalCompilerOptions(defaults: Seq[(String, Seq[(String, Any)])] = Seq()) extends CompilerOptions(defaults):
  override protected def createSection(name: String, entries: Map[String, Any]): Section = name match
    case "fun_logging" => FunctionalLoggingSection("fun_logging", entries)
    case _ => super.createSection(name, entries)

  def funLogging: FunctionalLoggingSection =
    options.get("fun_logging") match
      case Some(sec: FunctionalLoggingSection) => sec
      case _ => FunctionalLoggingSection("fun_logging", Map())

  override def setDefaults(): Unit =
    irLogging.logTypeInformation = true
    irLogging.logModule = true
    irLogging.logLowerings = false
    irLogging.logOptimizations = false

    funLogging.logTypeInformation = true
    funLogging.logModule = true
    funLogging.logNormalizedModule = false
    funLogging.verboseOutput = false

object FunctionalCompilerOptions:
  def fromResource(path: String): FunctionalCompilerOptions =
    val content = FileUtil.readFileFromResource(path)
    val parsedOptions = IniParser.parse(content)
    val compilerOptions = FunctionalCompilerOptions(parsedOptions)
    compilerOptions

  implicit val default: FunctionalCompilerOptions =
    val options = FunctionalCompilerOptions()
    options.setDefaults()
    options
