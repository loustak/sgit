name := "sgit"

version := "1.3"

scalaVersion := "2.13.0"

scalacOptions := Seq("-unchecked", "-deprecation")

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.8"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % "test"
libraryDependencies += "com.github.pathikrit" %% "better-files" % "3.8.0"
libraryDependencies += "com.github.scopt" %% "scopt" % "4.0.0-RC2"
