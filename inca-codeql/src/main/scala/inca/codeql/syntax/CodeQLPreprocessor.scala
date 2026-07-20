package inca.codeql.syntax

import inca.codeql.compile.CompiledCodeQlUnit

import scala.collection.mutable
import java.io.File

case class CodeQlCompilerOptions(
                                  importPaths: List[String] = List.empty,
                                  encoding: String = "UTF-8"
                                )

object CodeQlCompilerOptions {
  val default: CodeQlCompilerOptions = CodeQlCompilerOptions()
}

class CodeQLPreprocessor(options: CodeQlCompilerOptions = CodeQlCompilerOptions.default) {

  // Track already processed files to avoid duplicate expansion
  private val processedFiles = mutable.Set[String]()

  // Regex to match import statements in QL
  // Matches: import ModuleName, private import ModuleName, or import /path/to/file.qll
  private val importPattern = """^(?:private\s+)?import\s+([^\s;]+)\s*;?\s*$""".r

  /**
   * Preprocess QL source by recursively expanding all imports
   * @param source The QL source code as a string
   * @return Preprocessed source with all imports replaced by their content
   */
  def preprocess(source: String): String = {
    preprocessInternal(source, resolvedPath = "<root>", baseDir = None)
  }

  private def preprocessInternal(
                                  source: String,
                                  resolvedPath: String,
                                  baseDir: Option[File]
                                ): String = {
    // Mark this file as processed to avoid circular/duplicate imports
    if (resolvedPath != "<root>") {
      if (processedFiles.contains(resolvedPath)) {
        return "" // Already processed, return empty
      }
      processedFiles.add(resolvedPath)
    }

    val lines = source.split("\n", -1).toList
    val output = new StringBuilder()

    for (line <- lines) {
      line.trim match {
        case importPattern(importPath) =>
          // Resolve and expand the import
          val importedContent = resolveAndReadImport(importPath, baseDir)
          importedContent match {
            case Some(content) =>
              // Recursively preprocess the imported content
              val newBaseDir = baseDir.orElse(getBaseDirFromPath(importPath))
              val expandedContent = preprocessInternal(content, importPath, newBaseDir)
              output.append(expandedContent).append("\n")
            case None =>
              // Import not found, keep the original line (or could throw error)
              output.append(line).append("\n")
          }
        case _ =>
          // Regular line, keep as-is
          output.append(line).append("\n")
      }
    }

    output.toString()
  }

  /**
   * Resolve an import path and read its content
   */
  private def resolveAndReadImport(importPath: String, baseDir: Option[File]): Option[String] = {
    // Try to find the file in various locations
    val possiblePaths = buildSearchPaths(importPath, baseDir)

    for (path <- possiblePaths) {
      val file = new File(path)
      if (file.exists() && file.isFile) {
        return Some(scala.io.Source.fromFile(file, options.encoding).mkString)
      }
    }

    None
  }

  /**
   * Build list of possible paths to search for an import
   */
  private def buildSearchPaths(importPath: String, baseDir: Option[File]): List[String] = {
    val paths = mutable.ListBuffer[String]()

    // Default CodeQL library base path
    val codeqlLibBase = "/Users/mats/Projects/codeql/qlpacks/codeql/java-queries/1.11.4/.codeql/libraries/codeql/java-all/9.1.2/"

    // If it's an absolute path, try it directly
    if (importPath.startsWith("/")) {
      paths += importPath
      // Try with .qll extension (CodeQL library files)
      if (!importPath.endsWith(".qll")) {
        paths += s"$importPath.qll"
      }
    } else {
      // Convert module-style imports to path
      // Handle both dot notation (semmle.code.FileSystem) and :: notation (DataFlow::DataFlow)
      val normalizedPath = importPath.replace("::", "/").replace(".", "/")

      // Relative import - try relative to base directory
      baseDir.foreach { dir =>
        paths += new File(dir, normalizedPath).getAbsolutePath
        if (!normalizedPath.endsWith(".qll")) {
          paths += new File(dir, s"$normalizedPath.qll").getAbsolutePath
        }
      }

      // Try CodeQL library base path and subdirectories
      paths += new File(codeqlLibBase, normalizedPath).getAbsolutePath
      if (!normalizedPath.endsWith(".qll")) {
        paths += new File(codeqlLibBase, s"$normalizedPath.qll").getAbsolutePath
      }

      // Search subdirectories of the CodeQL library path
      searchSubdirectories(new File(codeqlLibBase), normalizedPath, paths)

      // Try configured import paths
      for (importDir <- options.importPaths) {
        paths += new File(importDir, normalizedPath).getAbsolutePath
        if (!normalizedPath.endsWith(".qll")) {
          paths += new File(importDir, s"$normalizedPath.qll").getAbsolutePath
        }
      }
    }

    paths.toList
  }

  /**
   * Recursively search subdirectories for the import file
   */
  private def searchSubdirectories(
                                    baseDir: File,
                                    fileName: String,
                                    paths: mutable.ListBuffer[String]
                                  ): Unit = {
    if (baseDir.exists() && baseDir.isDirectory) {
      // Try direct match in base dir
      val directMatch = new File(baseDir, fileName)
      if (directMatch.exists()) {
        paths += directMatch.getAbsolutePath
      }
      val qllMatch = new File(baseDir, s"$fileName.qll")
      if (qllMatch.exists()) {
        paths += qllMatch.getAbsolutePath
      }

      // Search subdirectories
      baseDir.listFiles().foreach { child =>
        if (child.isDirectory) {
          searchSubdirectories(child, fileName, paths)
        }
      }
    }
  }

  /**
   * Extract base directory from an import path
   */
  private def getBaseDirFromPath(importPath: String): Option[File] = {
    val file = new File(importPath)
    if (file.isAbsolute) {
      Option(file.getParentFile)
    } else {
      None
    }
  }

  /**
   * Reset the processor state (useful for processing multiple independent files)
   */
  def reset(): Unit = {
    processedFiles.clear()
  }
}