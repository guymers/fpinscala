scalaVersion := "2.12.1"

// https://tpolecat.github.io/2014/04/11/scalac-flags.html
scalacOptions ++= Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-language:existentials",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-language:postfixOps",
  "-unchecked",
  "-Xfatal-warnings",
  "-Xfuture",
  "-Xlint",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-unused-import",
  "-Ywarn-value-discard"
)

conflictManager := ConflictManager.strict

dependencyOverrides += { "org.scala-lang" % "scala-library" % scalaVersion.value }
dependencyOverrides += { "org.scala-lang" % "scala-reflect" % scalaVersion.value }
dependencyOverrides += "org.scala-lang.modules" %% "scala-xml" % "1.0.6"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"
