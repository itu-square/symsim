name := "symsim"

ThisBuild / scalaVersion := "3.3.1"

val scalatestVersion = "3.2.17"
val catsVersion = "2.6.1"


scalacOptions ++= Seq (
  "-deprecation",
  "-feature",
  "-source:future", // doesn't work with stryker
  "-language:adhocExtensions",
)


libraryDependencies ++= Seq (
  "org.scalacheck" %% "scalacheck" % "1.15.3" % Test,
  "org.scalatest" %% "scalatest-freespec" % scalatestVersion % Test,
  "org.scalatest" %% "scalatest-shouldmatchers" % scalatestVersion % Test,
  "org.scalatest" %% "scalatest-mustmatchers" % scalatestVersion % Test,
  "org.scalatestplus" %% "scalacheck-1-17" % (scalatestVersion+".0") % Test,
  "org.typelevel" %% "cats-core" % catsVersion,
  "org.typelevel" %% "cats-laws" % catsVersion,
  "org.typelevel" %% "discipline-scalatest" % "2.1.5",
  "org.typelevel" %% "paiges-core" % "0.4.2",
  "org.scalanlp" %% "breeze" % "2.1.0",
  "org.typelevel" %% "spire" % "0.18.0" // for probula
)

Test / parallelExecution := false
Test / testOptions += Tests.Argument("-oD")
// Test / testOptions += Tests.Argument (TestFrameworks.ScalaTest, "-h", "target/report")
