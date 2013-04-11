scalaVersion := "2.10.1"

organization := "com.precog"

name := "templater"

version := "0.1-SNAPSHOT"

libraryDependencies += "org.specs2" %% "specs2" % "1.15-SNAPSHOT" % "test"

libraryDependencies += "org.streum" %% "configrity-core" % "1.0.0"

libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-compiler" % _)

libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-reflect" % _)

libraryDependencies += "com.github.scala-incubator.io" %% "scala-io-core" % "0.4.2"

libraryDependencies += "com.github.scala-incubator.io" %% "scala-io-file" % "0.4.2"

libraryDependencies += "com.github.scopt" %% "scopt" % "2.1.0"

resolvers += "sonatype-public" at "https://oss.sonatype.org/content/groups/public"

resolvers += "specs2-resolver-0" at "https://oss.sonatype.org/content/repositories/releases"

resolvers += "specs2-resolver-1" at "https://oss.sonatype.org/content/repositories/snapshots"

net.virtualvoid.sbt.graph.Plugin.graphSettings
