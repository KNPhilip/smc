name := "SMC"

version := "0.1.0"

scalaVersion := "3.5.0"

libraryDependencies += "org.scalameta" %% "munit" % "1.0.4" % Test

Test / testOptions += Tests.Argument(TestFrameworks.JUnit, "-v", "-u", "target/test-reports")