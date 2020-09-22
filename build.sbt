name := "inca-scala"

organization := "de.uni-mainz.informatik.pl"

version := "0.1"

scalaVersion := "2.13.1"

scalacOptions ++= Seq(
  "-target:11",
  "-Ymacro-annotations"
  , "-J-Xss10m"
)

javacOptions ++= Seq("-source", "11")

Test / parallelExecution := false

libraryDependencies += "de.uni-mainz.informatik.pl" %% "truechange" % "0.1.1"
libraryDependencies += "de.uni-mainz.informatik.pl" %% "truediff" % "0.1.1" % "test"

libraryDependencies += "org.apache.commons" % "commons-collections4" % "4.4"

libraryDependencies += "org.scala-lang.modules" %% "scala-collection-contrib" % "0.2.1"
libraryDependencies += "org.scalameta" %% "scalameta" % "4.2.3"
libraryDependencies += "org.scalactic" %% "scalactic" % "3.1.0"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.0" % "test"
libraryDependencies += "com.lihaoyi" %% "fastparse" % "2.1.3"
libraryDependencies += "com.lihaoyi" %% "scalaparse" % "2.1.3"

resolvers += "Eclipse Releases" at "https://repo.eclipse.org/content/groups/releases"
libraryDependencies += "org.eclipse.viatra" % "viatra-query-runtime" % "2.3.0"
libraryDependencies += "org.eclipse.emf" % "org.eclipse.emf.ecore" % "2.19.0"

libraryDependencies += "com.google.guava" % "guava" % "28.2-jre"

mainClass in (Compile, run) := Some("inca.frontend.util.CompilerFrontend")