name := "IncAPP"

version := "0.1"

scalaVersion := "2.13.1"

scalacOptions ++= Seq(
  "-Ymacro-annotations"
//, "-Ymacro-debug-lite"
  , "-J-Xss10m"
)

// https://mvnrepository.com/artifact/org.apache.commons/commons-collections4
libraryDependencies += "org.apache.commons" % "commons-collections4" % "4.4"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.1.0"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.0" % "test"
libraryDependencies += "com.lihaoyi" %% "fastparse" % "2.1.3"

resolvers += "Eclipse Releases" at "https://repo.eclipse.org/content/groups/releases"
libraryDependencies += "org.eclipse.viatra" % "viatra-query-runtime" % "2.3.0"
libraryDependencies += "org.eclipse.emf" % "org.eclipse.emf.ecore" % "2.19.0"

libraryDependencies += "com.google.guava" % "guava" % "28.2-jre"