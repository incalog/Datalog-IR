package inca.frontend.oodl.compile

import inca.util.FileUtil
import inca.util.compileroptions.{CompilerOptions, IniParser, Section}

case class OODLLoggingSection(override val name: String, defaults: Map[String, Any]) extends Section(name, defaults):
  // Log type information when logging a module
  def logTypeInformation: Boolean = readBoolean("typed")
  def logTypeInformation_=(newVal: Boolean): Unit = update("typed", newVal)
  // Log the main module before transformation to IR code
  def logModule: Boolean = readBoolean("module")
  def logModule_=(newVal: Boolean): Unit = update("module", newVal)
  // Log the ssa transformed module
  def logSSAModule: Boolean = readBoolean("ssa")
  def logSSAModule_=(newVal: Boolean): Unit = update("ssa", newVal)
  // Log all relations
  def verboseOutput: Boolean = readBoolean("verbose_output")
  def verboseOutput_= (newVal: Boolean): Unit = update("verbose_output", newVal)


final class OODLCompilerOptions(defaults: Seq[(String, Seq[(String, Any)])] = Seq()) extends CompilerOptions(defaults):
  override protected def createSection(name: String, entries: Map[String, Any]): Section = name match
    case "oodl_logging" => OODLLoggingSection("oodl_logging", entries)
    case _ => super.createSection(name, entries)

  def oodlLogging: OODLLoggingSection =
    options.get("oodl_logging") match
      case Some(sec: OODLLoggingSection) => sec
      case _ => OODLLoggingSection("oodl_logging", Map())

  override def setDefaults(): Unit =
    irLogging.logTypeInformation = true
    irLogging.logModule = true
    irLogging.logLowerings = false
    irLogging.logOptimizations = false

    oodlLogging.logTypeInformation = true
    oodlLogging.logModule = true
    oodlLogging.logSSAModule = false
    oodlLogging.verboseOutput = false

object OODLCompilerOptions:
  def fromResource(path: String): OODLCompilerOptions =
    val content = FileUtil.readFileFromResource(path)
    val parsedOptions = IniParser.parse(content)
    val compilerOptions = OODLCompilerOptions(parsedOptions)
    compilerOptions

  implicit val default: OODLCompilerOptions =
    val options = OODLCompilerOptions()
    options.setDefaults()
    options
