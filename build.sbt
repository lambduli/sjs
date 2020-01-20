scalaVersion := "2.12.7"
organization := "com.example"

lazy val lipo = (project in file("."))
  .settings(
    name := "Lipo",
    libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"
  )