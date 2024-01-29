
name := "inca"

ThisBuild / organization := "de.uni-mainz.informatik.pl"
ThisBuild / version := "0.1"

ThisBuild / scalacOptions += "-deprecation"

ThisBuild / Test / parallelExecution := false

// This patches the class path to make sbt-test work
ThisBuild / fork := true

val scalaVersionString = "3.3.0"


val truediffVersion = "0.1.5-SNAPSHOT"

lazy val inca_ir = (project in file("inca-ir"))
  .settings(
    scalaVersion := scalaVersionString,

    resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",

    libraryDependencies ++= Seq(
      ("de.uni-mainz.informatik.pl" %% "truechange" % truediffVersion).cross(CrossVersion.for3Use2_13),

      "org.scalatest" %% "scalatest" % "3.2.16" % "test",
      "org.typelevel" %% "cats-parse" % "0.3.9",
      "org.typelevel" %% "cats-core" % "2.9.0",
    )
  )

lazy val inca_fun = (project in file("inca-fun"))
  .dependsOn(inca_ir % "compile->compile")
  .dependsOn(inca_viatra % "test->test")
  .dependsOn(inca_souffle % "test->test")
  .dependsOn(inca_foreign_scala % "compile->compile")
  //.dependsOn(inca_foreign_scala % "compile->compile")
  .settings(
    scalaVersion := scalaVersionString,

    libraryDependencies ++= Seq(
      // Additional data structures, such as MultiDict
      "org.scala-lang.modules" %% "scala-collection-contrib" % "0.3.0",

      "org.scalatest" %% "scalatest" % "3.2.16" % "test",
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

      "org.scalatest" %% "scalatest" % "3.2.16" % "test",
      "org.typelevel" %% "cats-parse" % "0.3.9",
      "org.typelevel" %% "cats-core" % "2.9.0",
    )
  )

lazy val inca_datalog = (project in file("inca-datalog"))
  .dependsOn(inca_ir % "compile->compile")
  .dependsOn(inca_viatra % "compile->compile; test->test")
  .dependsOn(inca_souffle % "test->test")
  .settings(
    scalaVersion := scalaVersionString,

    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.16" % "test",
      "org.typelevel" %% "cats-parse" % "0.3.9",
      "org.typelevel" %% "cats-core" % "2.9.0",
    )
  )

lazy val inca_foreign_scala = (project in file("inca-foreign-scala"))
  .dependsOn(inca_ir % "compile->compile")
  .settings(
    scalaVersion := scalaVersionString,

    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.16" % "test",
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

      "org.scalatest" %% "scalatest" % "3.2.16" % "test",
      // Additional data structures, such as MultiDict
      "org.scala-lang.modules" %% "scala-collection-contrib" % "0.3.0",
      // Datalog query engine
      "org.eclipse.emf" % "org.eclipse.emf.ecore" % "2.23.0",
      "org.eclipse.collections" % "eclipse-collections" % "10.4.0",
      "org.eclipse.viatra" % "viatra-query-runtime" % "2.7.0",
      // Required for runtime reflection and code execution
      //("org.scala-lang" %% "scala3-interfaces" % scalaVersion.value).cross(CrossVersion.disabled)
      "org.scala-lang" %% "scala3-staging" % scalaVersion.value,
    )
  )

lazy val inca_souffle = (project in file("inca-souffle"))
  .dependsOn(inca_ir % "compile->compile")
  .settings(
    scalaVersion := scalaVersionString,
    resolvers += "Eclipse Releases" at "https://repo.eclipse.org/content/groups/releases",
    resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",

    libraryDependencies ++= Seq(
      ("de.uni-mainz.informatik.pl" %% "truechange" % truediffVersion).cross(CrossVersion.for3Use2_13),
      ("de.uni-mainz.informatik.pl" %% "truediff" % truediffVersion).cross(CrossVersion.for3Use2_13),

      "org.scalatest" %% "scalatest" % "3.2.16" % "test",
      // Additional data structures, such as MultiDict
      "org.scala-lang.modules" %% "scala-collection-contrib" % "0.3.0",
    )
  )

inca_foreign_scala / libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "latest.integration" % Test
)

lazy val hazel_typing = (project in file("hazel-typing"))
  .dependsOn(inca_ir % "compile->compile")
  .dependsOn(inca_foreign_scala % "compile->compile")
  .dependsOn(inca_viatra % "compile->compile")
  .dependsOn(hazel_typing_diffable % "compile->compile")
  .settings(
    scalaVersion := "3.3.0",

    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.16" % "test",
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

      "org.scalatest" %% "scalatest" % "3.2.16" % "test",
    )
  )

