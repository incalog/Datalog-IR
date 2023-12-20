package inca.frontend.datalog.compile

import inca.util.FileUtil
import inca.util.compileroptions.{CompilerOptions, IniParser, Section}

case class DatalogLoggingSection(override val name: String, defaults: Map[String, Any]) extends Section(name, defaults):
  // Log type information when logging a module
  def logTypeInformation: Boolean = readBoolean("typed")
  def logTypeInformation_=(newVal: Boolean): Unit = update("typed", newVal)
  // Log the main module before transformation to IR code
  def logModule: Boolean = readBoolean("module")
  def logModule_=(newVal: Boolean): Unit = update("module", newVal)
  // Log all relations
  def verboseOutput: Boolean = readBoolean("verbose_output")
  def verboseOutput_= (newVal: Boolean): Unit = update("verbose_output", newVal)

final class DatalogCompilerOptions(defaults: Seq[(String, Seq[(String, Any)])] = Seq()) extends CompilerOptions(defaults):
  override protected def createSection(name: String, entries: Map[String, Any]): Section = name match
    case "datalog_logging" => DatalogLoggingSection("datalog_logging", entries)
    case _ => super.createSection(name, entries)

  def datalogLogging: DatalogLoggingSection =
    options.get("datalog_logging") match
      case Some(sec: DatalogLoggingSection) => sec
      case _ => DatalogLoggingSection("datalog_logging", Map())

  override def setDefaults(): Unit =
    irLogging.logTypeInformation = true
    irLogging.logModule = true
    irLogging.logLowerings = false
    irLogging.logOptimizations = false

    datalogLogging.logTypeInformation = true
    datalogLogging.logModule = true
    datalogLogging.verboseOutput = false

object DatalogCompilerOptions:
  def fromResource(path: String): DatalogCompilerOptions =
    val content = FileUtil.readFileFromResource(path)
    val parsedOptions = IniParser.parse(content)
    val compilerOptions = DatalogCompilerOptions(parsedOptions)
    compilerOptions

  implicit val default: DatalogCompilerOptions =
    val options = DatalogCompilerOptions()
    options.setDefaults()
    options
