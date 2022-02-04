
name := "inca"

ThisBuild / organization := "de.uni-mainz.informatik.pl"
ThisBuild / version := "0.1"
ThisBuild / scalaVersion := "2.13.1"
ThisBuild / scalacOptions += "-target:11"

Test / parallelExecution := false

val truediffVersion = "0.1.5-SNAPSHOT"

lazy val inca = (project in file(".")).settings(
  resolvers += "Eclipse Releases" at "https://repo.eclipse.org/content/groups/releases",
  resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",

  scalacOptions ++= Seq("-Ymacro-annotations", "-J-Xss10m"),

  Test / classLoaderLayeringStrategy := ClassLoaderLayeringStrategy.ScalaLibrary,

  libraryDependencies ++= Seq(
    "de.uni-mainz.informatik.pl" %% "truechange" % truediffVersion,
    "de.uni-mainz.informatik.pl" %% "truediff" % truediffVersion,
    "org.scala-lang.modules" %% "scala-collection-contrib" % "0.2.1",
    "org.scalameta" %% "scalameta" % "4.2.3",
    "org.eclipse.viatra" % "viatra-query-runtime" % "2.4.0",
    "org.eclipse.emf" % "org.eclipse.emf.ecore" % "2.23.0",
    "org.eclipse.collections" % "eclipse-collections" % "10.4.0",
    "com.google.guava" % "guava" % "28.2-jre",
    "com.lihaoyi" %% "fastparse" % "2.1.3",
    "com.lihaoyi" %% "scalaparse" % "2.1.3",
    "com.regblanc" %% "scala-smtlib" % "0.2.1-42-gc68dbaa",

    "org.scalatest" %% "scalatest" % "3.1.0" % "test",
  )
)

lazy val souffle_frontend = (project in file("souffle-frontend"))
  .dependsOn(inca % "compile->compile;test->test")
  .settings(
  name := "souffle-frontend",

  resolvers += "Eclipse Releases" at "https://repo.eclipse.org/content/groups/releases",

  libraryDependencies ++= Seq(
    "com.lihaoyi" %% "fastparse" % "2.1.3",
    "org.scalatest" %% "scalatest" % "3.1.0" % "test",
    "de.uni-mainz.informatik.pl" %% "truechange" % truediffVersion,
    "org.eclipse.collections" % "eclipse-collections" % "10.4.0",

    "de.uni-mainz.informatik.pl" %% "truediff" % truediffVersion % "test",
    "org.eclipse.emf" % "org.eclipse.emf.ecore" % "2.23.0" % "test",
  )
)


