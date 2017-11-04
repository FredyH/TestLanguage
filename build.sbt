name := "TestLanguage"

version := "1.0"

scalaVersion := "2.12.4"

resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.6"

libraryDependencies += "org.apache.commons" % "commons-text" % "1.1"

libraryDependencies += "org.apache.bcel" % "bcel" % "6.1"

