name := "SMC"

version := "0.1.0"

scalaVersion := "3.5.0"

// Dependencies
libraryDependencies += "org.scalameta" %% "munit" % "1.0.4" % Test

Test / testOptions += Tests.Argument(TestFrameworks.ScalaTest, "-u", "target/test-reports")