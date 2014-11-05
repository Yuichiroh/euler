// import AssemblyKeys._ // put this at the top of the file

name := "yuima.euler"

version := "1.0"

scalaVersion := "2.11.1"

resolvers ++= Seq("snapshots" at "http://scala-tools.org/repo-snapshots",
    "releases" at "http://scala-tools.org/repo-releases",
    "maven2" at "http://repo1.maven.org/maven2/")

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "2.0" % "test"

// seq(assemblySettings: _*)
