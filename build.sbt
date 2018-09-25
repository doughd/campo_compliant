lazy val root = (project in file(".")).
  settings(
    name := "compliant",
    version := "0.1.0",
    scalaVersion := "2.11.8",
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.0.1" % Test,
      "org.scalactic" %% "scalactic" % "3.0.1" % Test,
      "org.scalacheck" %% "scalacheck" % "1.13.4" % Test
    )
  )

// Include short exceptions on error
testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oS")
