ThisBuild / scalaVersion := "3.8.3"

lazy val root = (project in file("."))
  .settings(
    name := "lambda",
    idePackagePrefix := Some("com.yuuki14202028")
  )

scalacOptions ++= Seq("-feature", "-deprecation")

libraryDependencies += "org.scalameta" %% "munit" % "1.3.3" % Test

testFrameworks += new TestFramework("munit.Framework")
