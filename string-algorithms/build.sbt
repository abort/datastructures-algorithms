name := "string-algorithms"

version := "0.1"

scalaVersion := "2.12.3"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.1"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"
libraryDependencies += "com.storm-enroute" %% "scalameter-core" % "0.8.2"
resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/releases"


mainClass in (Compile, run) := Some("SuffixTrie")