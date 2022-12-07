lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.github.netfalo",
      scalaVersion := "2.13.6"
    )),
    name := "aoc2022"
  )

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % Test

testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oD")
