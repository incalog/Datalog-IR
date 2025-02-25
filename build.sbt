
name := "inca"

ThisBuild / organization := "de.uni-mainz.informatik.pl"
ThisBuild / version := "0.1"

ThisBuild / scalacOptions += "-deprecation"

ThisBuild / Test / parallelExecution := false

// This patches the class path to make sbt-test work
ThisBuild / fork := true

val scalaVersionString = "3.5.2"
val scalaTestVersionString = "3.2.16"


val truediffVersion = "0.1.5-SNAPSHOT"

lazy val inca_ir = (project in file("inca-ir"))
  .settings(
    scalaVersion := scalaVersionString,

    resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",

    libraryDependencies ++= Seq(
      ("de.uni-mainz.informatik.pl" %% "truechange" % truediffVersion).cross(CrossVersion.for3Use2_13),

      "de.uni-mainz.informatik.pl" %% "benchmark-scala" % "0.1",
      "org.scalatest" %% "scalatest" % scalaTestVersionString % "test",
      "de.uni-mainz.informatik.pl" %% "sturdy_core" % "0.1",
      "org.typelevel" %% "cats-parse" % "0.3.9",
      "org.typelevel" %% "cats-core" % "2.9.0",
      ("com.regblanc" %% "scala-smtlib" % "0.2.1-42-gc68dbaa").cross(CrossVersion.for3Use2_13),
    )
  )

lazy val inca_fun = (project in file("inca-fun"))
  .dependsOn(inca_ir % "compile->compile")
  .dependsOn(inca_viatra % "test->test")
  .dependsOn(inca_souffle % "test->test")
  .dependsOn(inca_ascent % "test->test")
  .dependsOn(inca_ddlog % "test->test")
  .dependsOn(inca_foreign_scala % "compile->compile")
  .dependsOn(inca_foreign_ddlog % "compile->compile")
  //.dependsOn(inca_foreign_scala % "compile->compile")
  .settings(
    scalaVersion := scalaVersionString,

    libraryDependencies ++= Seq(
      // Additional data structures, such as MultiDict
      "org.scala-lang.modules" %% "scala-collection-contrib" % "0.3.0",

      "org.scalatest" %% "scalatest" % scalaTestVersionString % "test",
      "org.typelevel" %% "cats-parse" % "0.3.9",
      "org.typelevel" %% "cats-core" % "2.9.0",
    )
  )

lazy val inca_oodl = (project in file("inca-oodl"))
  .dependsOn(inca_ir % "compile->compile")
  .dependsOn(inca_foreign_scala % "compile->compile")
  .dependsOn(inca_viatra % "test->test")
  .dependsOn(inca_souffle % "test->test")
  //.dependsOn(inca_foreign_scala % "compile->compile")
  .settings(
    scalaVersion := scalaVersionString,

    libraryDependencies ++= Seq(
      // Additional data structures, such as MultiDict
      "org.scala-lang.modules" %% "scala-collection-contrib" % "0.3.0",

      "org.scalatest" %% "scalatest" % scalaTestVersionString % "test",
      "org.typelevel" %% "cats-parse" % "0.3.9",
      "org.typelevel" %% "cats-core" % "2.9.0",
    )
  )

lazy val inca_datalog = (project in file("inca-datalog"))
  .dependsOn(inca_ir % "compile->compile")
  .dependsOn(inca_viatra % "compile->compile; test->test")
  .dependsOn(inca_souffle % "test->test")
  .dependsOn(inca_ascent % "test->test")
  .settings(
    scalaVersion := scalaVersionString,

    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % scalaTestVersionString % "test",
      "org.typelevel" %% "cats-parse" % "0.3.9",
      "org.typelevel" %% "cats-core" % "2.9.0",
    )
  )

lazy val inca_foreign_scala = (project in file("inca-foreign-scala"))
  .dependsOn(inca_ir % "compile->compile")
  .settings(
    scalaVersion := scalaVersionString,

    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % scalaTestVersionString % "test",
    )
  )

lazy val inca_viatra = (project in file("inca-viatra"))
  .dependsOn(inca_ir % "compile->compile")
  .dependsOn(inca_foreign_scala % "compile->compile")
  .settings(
    scalaVersion := scalaVersionString,

    resolvers += "Eclipse Releases" at "https://repo.eclipse.org/content/groups/releases",
    resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",

    libraryDependencies ++= Seq(
      ("de.uni-mainz.informatik.pl" %% "truechange" % truediffVersion).cross(CrossVersion.for3Use2_13),
      ("de.uni-mainz.informatik.pl" %% "truediff" % truediffVersion).cross(CrossVersion.for3Use2_13),

      // Get logging information from viatra
      "org.apache.logging.log4j" %% "log4j-api-scala" % "13.1.0",

      "org.scalatest" %% "scalatest" % scalaTestVersionString % "test",
      // Additional data structures, such as MultiDict
      "org.scala-lang.modules" %% "scala-collection-contrib" % "0.3.0",
      // Datalog query engine
      "org.eclipse.emf" % "org.eclipse.emf.ecore" % "2.23.0",
      "org.eclipse.collections" % "eclipse-collections" % "10.4.0",
      "org.eclipse.viatra" % "viatra-query-runtime" % "2.9.0",
      // Required for runtime reflection and code execution
      //("org.scala-lang" %% "scala3-interfaces" % scalaVersion.value).cross(CrossVersion.disabled)
      "org.scala-lang" %% "scala3-staging" % scalaVersion.value,
    )
  )

lazy val inca_souffle = (project in file("inca-souffle"))
  .dependsOn(inca_ir % "compile->compile")
  .dependsOn(inca_viatra % "test->test")
  //.dependsOn(inca_ascent % "test->test")
  .settings(
    scalaVersion := scalaVersionString,
    resolvers += "Eclipse Releases" at "https://repo.eclipse.org/content/groups/releases",
    resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",

    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-parse" % "0.3.9",
      "org.typelevel" %% "cats-core" % "2.9.0",

      "org.scalatest" %% "scalatest" % scalaTestVersionString % "test",
      // Additional data structures, such as MultiDict
      "org.scala-lang.modules" %% "scala-collection-contrib" % "0.3.0",
      "com.lihaoyi" %% "upickle" % "3.2.0",
    )
  )

inca_foreign_scala / libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % scalaTestVersionString % Test
)

lazy val hazel_typing = (project in file("hazel-typing"))
  .dependsOn(inca_ir % "compile->compile")
  .dependsOn(inca_foreign_scala % "compile->compile")
  .dependsOn(inca_viatra % "compile->compile")
  .dependsOn(hazel_typing_diffable % "compile->compile")
  .settings(
    scalaVersion := scalaVersionString,

    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % scalaTestVersionString % "test",
    )
  )
lazy val hazel_typing_diffable = (project in file("hazel-typing-diffable"))
  .settings(
    scalaVersion := "2.13.10",
    scalacOptions += "-Ymacro-annotations",
    resolvers += "Eclipse Releases" at "https://repo.eclipse.org/content/groups/releases",
    resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",

    libraryDependencies ++= Seq(
      ("de.uni-mainz.informatik.pl" %% "truechange" % truediffVersion).cross(CrossVersion.for3Use2_13),
      ("de.uni-mainz.informatik.pl" %% "truediff" % truediffVersion).cross(CrossVersion.for3Use2_13),

      "org.scalatest" %% "scalatest" % scalaTestVersionString % "test",
    )
  )

lazy val inca_casestudy = (project in file("inca-casestudy"))
  .dependsOn(inca_ir % "compile->compile")
  .dependsOn(inca_foreign_scala % "compile->compile")
  .dependsOn(inca_viatra % "compile->compile")
  .dependsOn(inca_casestudy_diffable % "compile->compile")
  .dependsOn(inca_souffle % "compile->compile")
  .dependsOn(inca_ascent % "compile->compile")
  .settings(
    scalaVersion := scalaVersionString,

    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % scalaTestVersionString % "test",
      "org.scalanlp" %% "breeze" % "2.1.0",
      "org.scalanlp" %% "breeze-viz" % "2.1.0"
    )
  )

lazy val inca_casestudy_diffable = (project in file("inca-casestudy-diffable"))
  .settings(
    scalaVersion := "2.13.10",
    scalacOptions += "-Ymacro-annotations",
    resolvers += "Eclipse Releases" at "https://repo.eclipse.org/content/groups/releases",
    resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",

    libraryDependencies ++= Seq(
      ("de.uni-mainz.informatik.pl" %% "truechange" % truediffVersion).cross(CrossVersion.for3Use2_13),
      ("de.uni-mainz.informatik.pl" %% "truediff" % truediffVersion).cross(CrossVersion.for3Use2_13),
    )
  )

lazy val inca_ascent = (project in file("inca-ascent"))
  .dependsOn(inca_ir % "compile->compile")
  .settings(
    scalaVersion := scalaVersionString,

    resolvers += "Eclipse Releases" at "https://repo.eclipse.org/content/groups/releases",
    resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",

    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-parse" % "0.3.9",
      "org.typelevel" %% "cats-core" % "2.9.0",

      "org.scalatest" %% "scalatest" % scalaTestVersionString % "test",
      // Additional data structures, such as MultiDict
      "org.scala-lang.modules" %% "scala-collection-contrib" % "0.3.0",
      "com.lihaoyi" %% "upickle" % "3.2.0",
    )
  )

lazy val inca_bddbddb = (project in file("inca-bddbddb"))
  .dependsOn(inca_ir % "compile->compile")
  .settings(
    scalaVersion := scalaVersionString,

    resolvers += "Eclipse Releases" at "https://repo.eclipse.org/content/groups/releases",
    resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",

    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-parse" % "0.3.9",
      "org.typelevel" %% "cats-core" % "2.9.0",

      "org.scalatest" %% "scalatest" % scalaTestVersionString % "test",
      // Additional data structures, such as MultiDict
      "org.scala-lang.modules" %% "scala-collection-contrib" % "0.3.0",
    )
  )

lazy val inca_foreign_ddlog = (project in file("inca-foreign-ddlog"))
  .dependsOn(inca_ir % "compile->compile")
  .settings(
    scalaVersion := scalaVersionString,

    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % scalaTestVersionString % "test",
    )
  )

lazy val inca_ddlog = (project in file("inca-ddlog"))
  .dependsOn(inca_ir % "compile->compile")
  .dependsOn(inca_foreign_ddlog % "compile->compile")
  .settings(
    scalaVersion := scalaVersionString,

    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-parse" % "0.3.9",
      "org.typelevel" %% "cats-core" % "2.9.0",

      "org.scalatest" %% "scalatest" % scalaTestVersionString % "test",
      // Additional data structures, such as MultiDict
      "org.scala-lang.modules" %% "scala-collection-contrib" % "0.3.0",
    )
  )
