name := "IncAPP"

version := "0.1"

scalaVersion := "2.13.1"

scalacOptions ++= Seq(
  "-Ymacro-annotations",
  "-Ymacro-debug-lite"
)

// https://mvnrepository.com/artifact/org.apache.commons/commons-collections4
libraryDependencies += "org.apache.commons" % "commons-collections4" % "4.4"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.1.0"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.0" % "test"

resolvers += "Eclipse Releases" at "https://repo.eclipse.org/content/groups/releases"
libraryDependencies += "org.eclipse.viatra" % "viatra-query-runtime" % "2.3.0"