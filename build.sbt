scalaVersion := "2.10.1"

organization := "com.precog"

name := "templater"

version := "0.1-SNAPSHOT"

libraryDependencies += "org.specs2" %% "specs2" % "1.15-SNAPSHOT" % "test"

libraryDependencies += "org.streum" %% "configrity-core" % "1.0.0"

libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-compiler" % _)

libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-reflect" % _)

resolvers += "specs2-resolver-0" at "https://oss.sonatype.org/content/repositories/releases"

resolvers += "specs2-resolver-1" at "https://oss.sonatype.org/content/repositories/snapshots"

net.virtualvoid.sbt.graph.Plugin.graphSettings
