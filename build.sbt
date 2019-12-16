name := "inca-scala"

version := "0.1"

scalaVersion := "2.13.1"

resolvers += Resolver.sonatypeRepo("snapshots")

libraryDependencies += "org.scalameta" %% "scalameta" % "4.2.3"
//// https://mvnrepository.com/artifact/org.eclipse.viatra/viatra-query-runtime
//libraryDependencies += "org.eclipse.viatra" % "viatra-query-runtime" % "2.3.0.M4"
// https://mvnrepository.com/artifact/org.eclipse.viatra/viatra-query-runtime
libraryDependencies += "org.eclipse.viatra" % "viatra-query-runtime" % "2.2.0"
