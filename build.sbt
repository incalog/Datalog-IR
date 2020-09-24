
name := "inca"

ThisBuild / organization := "de.uni-mainz.informatik.pl"
ThisBuild / version := "0.1"
ThisBuild / scalaVersion := "2.13.1"
ThisBuild / scalacOptions += "-target:11"

Test / parallelExecution := false

lazy val inca = (project in file(".")).settings(
  resolvers += "Eclipse Releases" at "https://repo.eclipse.org/content/groups/releases",

  scalacOptions ++= Seq("-Ymacro-annotations", "-J-Xss10m"),

  libraryDependencies ++= Seq(
    "de.uni-mainz.informatik.pl" %% "truechange" % "0.1.1",
    "org.scala-lang.modules" %% "scala-collection-contrib" % "0.2.1",
    "org.scalameta" %% "scalameta" % "4.2.3",
    "org.eclipse.viatra" % "viatra-query-runtime" % "2.4.0",
    "org.eclipse.emf" % "org.eclipse.emf.ecore" % "2.23.0",

    "de.uni-mainz.informatik.pl" %% "truediff" % "0.1.1" % "test",
    "org.scalatest" %% "scalatest" % "3.1.0" % "test",
  )
)

lazy val souffle_importer = (project in file("souffle-importer")).dependsOn(inca).settings(
  name := "souffle-importer",

  resolvers += "Eclipse Releases" at "https://repo.eclipse.org/content/groups/releases",

  libraryDependencies ++= Seq(
    "com.lihaoyi" %% "fastparse" % "2.1.3",
    "org.scalatest" %% "scalatest" % "3.1.0" % "test",
    "de.uni-mainz.informatik.pl" %% "truechange" % "0.1.1",

    "de.uni-mainz.informatik.pl" %% "truediff" % "0.1.1" % "test",
    "org.eclipse.emf" % "org.eclipse.emf.ecore" % "2.23.0" % "test",
  )
)

