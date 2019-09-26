scalaVersion := "2.13.0"

// https://tpolecat.github.io/2017/04/25/scalac-flags.html
scalacOptions ++= Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-explaintypes",
  "-feature",
  "-language:existentials",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-language:postfixOps",
  "-unchecked",
  "-Xcheckinit",
  "-Xlint:_",
  "-Xfatal-warnings",
  "-Wdead-code",
  "-Wextra-implicit",
  "-Wnumeric-widen",
  "-Wunused:implicits",
  "-Wunused:imports",
  "-Wunused:locals",
  //"-Wunused:params",
  "-Wunused:patvars",
  "-Wunused:privates",
  "-Wvalue-discard",
)

conflictManager := ConflictManager.strict

dependencyOverrides += scalaOrganization.value % "scala-library" % scalaVersion.value
dependencyOverrides += scalaOrganization.value % "scala-reflect" % scalaVersion.value

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.10.3")

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % Test
