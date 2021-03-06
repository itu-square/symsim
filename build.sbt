name := "symsim"

ThisBuild / scalaVersion := "3.0.0-RC2"

val scalatestVersion = "3.2.7"
val catsVersion = "2.5.0"


scalacOptions ++= Seq (
  "-deprecation",
  "-feature",
  "-Xfatal-warnings",
)


libraryDependencies ++= Seq (
  "org.scalacheck" %% "scalacheck" % "1.15.3" % Test,
  "org.scalatest" %% "scalatest-freespec" % scalatestVersion % Test,
  "org.scalatest" %% "scalatest-shouldmatchers" % scalatestVersion % Test,
  "org.scalatest" %% "scalatest-mustmatchers" % scalatestVersion % Test,
  "org.scalatestplus" %% "scalacheck-1-15" % (scalatestVersion+".0") % Test,
  "org.typelevel" %% "cats-core" % catsVersion,
  "org.typelevel" %% "cats-laws" % catsVersion,
  "org.typelevel" %% "discipline-scalatest" % "2.1.3",
  "org.typelevel" %% "paiges-core" % "0.4.1"
)

Test / parallelExecution := false
Test / testOptions += Tests.Argument("-oD")
// Test / testOptions += Tests.Argument (TestFrameworks.ScalaTest, "-h", "target/report")
