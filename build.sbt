name := "TestLanguage"

version := "1.0"

scalaVersion := "2.11.11"

resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.6"

libraryDependencies += "de.opal-project" %% "common" % "0.8.15"

libraryDependencies += "de.opal-project" %% "abstract-interpretation-framework" % "0.8.15"

libraryDependencies += "de.opal-project" %% "bytecode-representation" % "0.8.15"

libraryDependencies += "de.opal-project" %% "architecture-validation" % "0.8.15"

libraryDependencies += "de.opal-project" %% "bytecode-assembler" % "0.8.15"

libraryDependencies += "de.opal-project" %% "bytecode-creator" % "0.8.15"