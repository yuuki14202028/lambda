ThisBuild / scalaVersion := "3.8.3"

lazy val root = (project in file("."))
  .settings(
    name := "lambda",
    idePackagePrefix := Some("com.yuuki14202028")
  )

libraryDependencies += "org.typelevel" %% "cats-core" % "2.13.0"
libraryDependencies += "org.typelevel" %% "cats-parse" % "1.1.0"
libraryDependencies += "org.typelevel" %% "kittens" % "3.5.0"