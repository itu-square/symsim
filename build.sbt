name := "symsim"

scalaVersion := "2.13.5"

scalacOptions ++= Seq (
  "-deprecation", 
  "-feature", 
  "-Xfatal-warnings"
)

// (testOptions in Test) += Tests.Argument(TestFrameworks.ScalaTest, "-h", "target/report")

libraryDependencies ++= Seq (  
  "org.scalacheck" %% "scalacheck" % "1.14.1" % Test,
  "org.scalatest" %% "scalatest-freespec" % "3.2.0" % Test,
  "org.scalatest" %% "scalatest-shouldmatchers" % "3.2.0" % Test,
  "org.scalatest" %% "scalatest-mustmatchers" % "3.2.0" % Test,
  "org.scalatestplus" %% "scalacheck-1-14" % "3.2.0.0" % Test,
  "com.github.alexarchambault" %% "scalacheck-shapeless_1.14" % "1.2.3" % Test,
  "org.typelevel" %% "cats-core" % "2.3.1",
  "org.typelevel" %% "cats-laws" % "2.3.1",
  "org.typelevel" %% "discipline-scalatest" % "2.1.1" 
)

