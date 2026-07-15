ThisBuild / scalaVersion := "3.8.3"

lazy val root = (project in file("."))
  .settings(
    name := "lambda",
    idePackagePrefix := Some("com.yuuki14202028")
  )

scalacOptions += "-feature"

libraryDependencies += "org.typelevel" %% "cats-core" % "2.13.0"
libraryDependencies += "org.typelevel" %% "cats-parse" % "1.1.0"
libraryDependencies += "org.typelevel" %% "kittens" % "3.5.0"
libraryDependencies += "org.scalameta" %% "munit" % "1.3.3" % Test

testFrameworks += new TestFramework("munit.Framework")